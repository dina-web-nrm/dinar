#!/usr/bin/Rscript

require("biotools")

pkgs <- c(
  "RMySQL",
  "plyr", # for vectorised iterative functions
  "dplyr",  # for quick data frame operations
  "stringr"  # for regexp parsing functions
)

import(pkgs)

# set global option to not use factors
# this has side-effect on read.csv
options(stringsAsFactors = FALSE)
options(row.names = FALSE)

# this defines the command line options
args <- "db_url, d, 2, character, source URL for db with media metadata
src_url, s, 2, character, source URL for The Naturalist server
dest_url, m, 2, character, dest URL for service to write media data to"

# defaults

opt <- get_args(args)

db_url <- "dina-db.nrm.se:3306"
src_url <- "http://test.naturforskaren.se/naturalist"
dest_url <- "http://ingimar-hp-elitebook-8470p.nrm.se:8080/MediaServerResteasy/"

if (!is.null(opt$db_url)) {
  db_url <- opt$db_url
} else {
  message("Using default db URL: ", db_url)
}

if (!is.null(opt$src_url)) {
  src_url <- opt$src_url
} else {
  message("Using default src URL: ", src_url)
}

if (!is.null(opt$dest_url)) {
  dest_url <- opt$dest_url
} else {
  message("Using default dest URL: ", dest_url)
}

cfgfile <- system.file("exec/media.conf", package = "biotools")
if (!interactive()) cfgfile <- "media.conf"
#if (interactive()) cfgfile <- "media.conf"
setwd("exec")
if (file.exists(cfgfile)) {
  message("Found config file, ", cfgfile, " overriding and using settings therein")
  cfg <- get_config(cfgfile)
  db_url <- cfg$db_url
  db_pass <- cfg$db_pass
  db_user <- cfg$db_user
  src_url <- cfg$src_url
  dest_url <- cfg$dest_url
}

# Processing media metadata from database, 
# migrating data to new mediaserver

db_port <- as.numeric(str_replace(str_extract(db_url, ":(\\d)+"), ":", ""))
if (is.na(db_port)) db_port = 3306
message("Using db port ", db_port)

db_hostname <- str_replace(db_url, "(.*?):.*", "\\1")
message("Using db hostname ", db_hostname)

my_db <- connectMediaDB(db_hostname, db_user, db_port, db_pass)
media <- getMediaDetailsFromDB(my_db)
link <- getMediaLinksFromDB(my_db)

uuids <- data.frame(link)
taxa_uuids <- unique(uuids$taxon_uuid)

# Retrieve all media metadata associated with the unique 
# taxon identifier provided
getMediaUuidsForTaxonUuidFromDB <- function(uuid) {
  taxon_link <- data.frame(filter(link, 
    taxon_uuid == uuid))
  taxon_media <- subset(data.frame(media), 
    media_uuid %in% taxon_link$media_uuid)
  tm <- unique(taxon_media)  
  # TODO: tm must be returned with media uuids ordered in the 
  # right way, according to this order   media_ids <- paste(collapse = ", ", 
  # unique(subset(uuids, taxon_uuid == t))$media_uuid)  
  tm_sorted <- join(taxon_link, tm, by = c("media_uuid"), type = "inner")
  return (unique(tm_sorted))
}


uploadMedia3 <- function(url, df) {
  
  # Download all the files
  #   media_files <- daply(df, c("sortOrder"), 
  #     function(x) downloadMedia(x$url, x$mime))
  
  df <- unique(df)
  
  res <- ddply(df, c("sortOrder", "fileName"), 
               function(x) downloadMedia(x$url, x$mime))
  
  media_files <- res$V1
  
  res$fileNameWExt <- res$V1
  res$V1 <- NULL
  
  u <- join(df, res, by = c("fileName", "sortOrder"))
  u$fileName <- NULL
  u <- plyr::rename(u, replace = 
    c("title" = "legend", "copyrightOwner" = "owner", "fileNameWExt" = "fileName"))  
  u$access <- "public"
  u$tags <- "origin:Naturforskaren"
  
  Encoding(u$owner) <- "latin1"
  Encoding(u$legend) <- "latin1"
  Encoding(u$tags) <- "latin1"
  
  u <- unique(u)
    
  res <- ddply(u, c("sortOrder", "fileName"), 
    function(x) postMedia2(url, 
      owner = x$owner, access = x$access, tags = x$tags, legend = x$legend,
      language = x$language, licenseType = as.character(x$license),
      fileName = as.character(x$fileName), export = FALSE, alt = x$legend))
  
  new_media_ids <- res$V1
  names(new_media_ids) <- res$fileName
  
  # Delete temporary downloads
  unlink(u$fileName)
  
  return (new_media_ids)
}  

postMedia2 <- function(url, 
  owner = "Unknown owner",
  access = "Unknown access", 
  tags = "Unknown tags",
  legend = "Unknown legend",
  language = "Unknown language",
  licenseType = "Unknown license type", 
  fileName = "Unknown filename", 
  export = FALSE, 
  alt = "Unknown alt text") {

  require("httr")
  require("caTools")

  # TODO: shouldn't this use the url parameter?
  destination <- paste0(
    "http://ingimar-hp-elitebook-8470p.nrm.se",
    ":8080", 
    "/MediaServerResteasy",
    #  "/media/post/json/utf8")
    "/media/upload-file/base64")

  destination <- "http://localhost:8080/MediaServerResteasy/media/upload-file/base64"

  b64 <- system(command = paste("base64", fileName), intern = TRUE)
  b64 <- paste(collapse = "", b64)

  body_json <- RJSONIO::toJSON(list(
    owner = owner,
    access = access,
    tags = tags,
    legend = legend,
    language = language,
    licenseType = licenseType,
    fileName = fileName,
    fileDataBase64 = b64,
    export = export,
    alt = alt)
  )

  res <- POST(destination, body = body_json, 
    add_headers("Content-Type" = "application/json"))

  return (content(res, as = "text"))
}

linkTaxonToMedias2 <- function(url, taxon_uuid, media_ids) {

  library(httr)
  
  destination <- paste0(url, "media/postJSONWithLIsta")
  
  body_json <- rjson::toJSON(list(
    typeOfSystem = "NF_TAXON",
    taxonUUID = taxon_uuid,
    nameOfSystem = "NF_SYSTEM",
    systemURL = src_url,
    mediaList = media_ids
  ))
  
  res <- POST(destination, body = body_json, 
              add_headers("Content-Type" = "application/json"))
    
  if (status_code(res) == 200) {
    res <- "OK (STATUS 200)"
  } else {
    res <- NULL
  }
  
  return(res)
}

# Function that for a given a taxon identifier,
# takes care of uploading the related media metadata
# (media details and links) to the new media server over the web
processTaxonUuidFromDB <- function(taxon_uuid) {

  message("Processing taxon ", taxon_uuid)

  current_media <- getMediaUuidsForTaxonUuidFromDB(taxon_uuid)
  
  current_mime <- sapply(str_split(current_media$media_type, "/"), "[", 2)  
  
  current_url <- paste0(src_url, "/media/get-stream/", 
    current_media$media_uuid)
  
  current_df <- data.frame(
    copyrightOwner = current_media$copyright_owner,
    license = current_media$license_abbrev,
    title = current_media$legend,
    fileName = current_media$media_uuid,
    url = current_url, 
    mime = current_mime,
    language = current_media$language,
    sortOrder = current_media$sort_order)

  # Upload the medias to the media server

  # FIXME! note that the upload function takes a parameter df 
  # which requires a df with certain columns to be present
  # not very pretty, should be changed
   
  media_ids <- paste(collapse = ", ", current_media$media_uuid)  
  
  message("Found nf media identifier(s) linked to this taxon: ", 
       media_ids)  
  
  failed_upload <- paste0("Failed uploading media for taxon ",
    taxon_uuid)  
  mkUpload <- dplyr::failwith(NULL, uploadMedia3) 
  uploaded_media_ids <- mkUpload(dest_url, current_df)  
    
  if (is.null(uploaded_media_ids)) 
    return(failed_upload)

  
  # TODO! unicode characters
  # TODO! sortorder works if only one language, but what if multiple language...
  
  u <- uploaded_media_ids
  
  # this is a named character vector with the sortOrder as
  # the name attribute, we rearrange this to get the right order
  
  media_ids <- as.character(u)  # as.character(u[order(as.integer(names(u)))])
  
  message("Uploading related media for taxon ", taxon_uuid, 
       "gave new media identifier(s) ", ": ", media_ids)
  
  # Link the taxon to the new media ids returned from the media server
  
  failed_link <- paste0("Failed linking taxon with identifier ", 
    taxon_uuid, " to associated new media identifiers: ", media_ids)
  mkLink <- dplyr::failwith(NULL, linkTaxonToMedias2)
  res <- mkLink(dest_url, taxon_uuid, as.list(media_ids))
    
  if (is.null(res))
    return(failed_link)
  
  message("Linking completed")
  message("Linked taxon with identifier ", taxon_uuid, 
       " to related media: ", res)
  
  return (res)
}  

# To debug with a restricted list of taxa, 
# uncomment the lines below

taxa_uuids <- c(
# "fcffabdc-501e-489a-88af-fbd19d758dce",
# "66118ad5-919b-4347-8981-e0944e6646f7"  
# "3301bf9b-68c8-453f-b32d-8e99ba423290",
# "2d5a6de0-e1c5-44cf-be16-044645d2a1c7"
  "d8d88968-3282-4efc-9614-28e59a13ee69",
  "6e18dcd9-a549-469d-8d9b-ee0f336b545a",
  "e373547f-6f0d-42c4-9b40-7f7215e2c25f",
  "56a83aa5-ac19-4d79-a1ad-32eb3aaf506e",
  "263f8ef2-e085-408f-90b1-e6e4decfc4cb",
  "1e90b156-00f9-4e73-a018-8ab67db956f5"
)

#boho <- getMediaUuidsForTaxonUuidFromDB(taxa_uuids)
#print(boho)

message("Processing all media for all taxa")
p <- plyr::failwith(NULL, processTaxonUuidFromDB, quiet = TRUE)
res <- adply(taxa_uuids, 1, p, .progress = "text", .inform = TRUE)

message("Done.")

log <- data.frame(taxon = taxa_uuids, status = res$V1)
message("Writing summary log to log.csv")
write.csv(log, "log.csv", row.names = FALSE)

if (!interactive()) q(status = 0)