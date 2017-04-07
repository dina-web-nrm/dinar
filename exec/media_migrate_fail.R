#!/usr/bin/Rscript

require("biotools")

pkgs <- c(
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

# this is the database server with the media metadata
db_url <- "dina-species.nrm.se:3306"

# this is the database server with the media metadata
src_url <- "http://naturforskaren.se"

# this is Ingimars media server in beta
dest_url <- "http://172.16.23.28:8080/MediaServerResteasy/"

if (!is.null(opt$db_url)) {
  db_url <- opt$db_url
} else {
  ncat("Using default db URL: ", db_url)
}

if (!is.null(opt$src_url)) {
  src_url <- opt$src_url
} else {
  ncat("Using default src URL: ", src_url)
}

if (!is.null(opt$dest_url)) {
  dest_url <- opt$dest_url
} else {
  ncat("Using default dest URL: ", dest_url)
}

# Processing media metadata from database, 
# migrating data to new mediaserver

db_port <- as.numeric(str_replace(str_extract(db_url, ":(\\d)+"), ":", ""))
if (is.na(db_port)) db_port = 3306
ncat("Using db port ", db_port)

db_hostname <- str_replace(db_url, "(.*?):.*", "\\1")
ncat("Using db hostname", db_hostname)

my_db <- connectMediaDB(db_hostname, db_port, db_pass = NA)
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
  return (unique(taxon_media))
}

# Function that for a given a taxon identifier,
# takes care of uploading the related media metadata
# (media details and links) to the new media server over the web
processTaxonUuidFromDB <- function(taxon_uuid) {

  ncat("Processing taxon ", taxon_uuid)

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
    mime = current_mime)
  
  media_ids <- paste(collapse = "", sep = ", ", current_media$media_uuid)
  ncat("Found the following media for this taxon: ", media_ids)

  # Upload the medias to the media server
  # FIXME! note that the upload function takes a parameter df 
  # which requires a df with certain columns to be present
  # not very pretty, should be changed
  
  failed_upload <- paste0("Failed uploading media for taxon ",
    taxon_uuid)
  
  mkUpload <- dplyr::failwith(NULL, uploadMedia)

  uploaded_media_ids <- mkUpload(dest_url, current_df)  

  media_ids <- as.character(uploaded_media_ids)
  
  ncat("Uploading related media for taxon with identifier ", 
       taxon_uuid, ": ", media_ids)
  
  if (is.null(uploaded_media_ids)) 
    return(failed_upload)
  
  # Link the taxon to the new media ids returned from the media server

  failed_link <- paste0("Failed linking taxon with identifier ", 
    taxon_uuid, " to associated media: ", media_ids)
  
  mkLink <- dplyr::failwith(NULL, linkTaxonToMedias)
  
  res <- mkLink(dest_url, taxon_uuid, as.list(media_ids))

  ncat("Linking taxon with identifier ", taxon_uuid, 
       " to related media: ", res)
  
  if (is.null(res))
    return(failed_link)

  # Uploading and linking succeeded
  
  return (res)
}

fail_taxon <- "3301bf9b-68c8-453f-b32d-8e99ba423290"
#fail_taxon <- "2d5a6de0-e1c5-44cf-be16-044645d2a1c7"

ncat("Processing media with id", fail_taxon)
res <- processTaxonUuidFromDB(fail_taxon)

ncat("Summary with complete log:")
print(res)

ncatn("Done")
if (!interactive()) q(status = 0)