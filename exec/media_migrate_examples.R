#!/usr/bin/Rscript

require("biotools")

pkgs <- c(
#  "jsonlite",
  "R.utils",
#  "dplyr",  # for quick data frame operations
  "httr",  # for issuing HTTP requests
  "stringr"  # for regexp parsing functions
)

import(pkgs)

# set global option to not use factors
# this has side-effect on read.csv
options(stringsAsFactors = FALSE)
options(row.names = FALSE)

# this defines the command line options
args <- "taxon, t, 2, character, taxon uuid from The Naturalist
src_url, s, 2, numeric, source URL for service to read the media from
dest_url, d, 2, character, destination URL for service to write the media to"

# defaults

opt <- get_args(args)

# this is the taxon uuid for Citronfjäril (Gonepteryx rhamni)
taxon <- "8bae741a-2b69-4f23-838c-6c7c10114d21"  

# this is the development server
#src_url <- "http://test.naturforskaren.se:8080"
#src_url <- "https://beta-naturforskaren.dina-web.net/nf-naturalist/"
src_url <- "https://dina-web.net/naturalist"

# this is Ingimars media server in beta
dest_url <- "http://ingimar-HP-EliteBook-8470p:8080/MediaServerResteasy/"  

if (!is.null(opt$media)) {
  taxon <- opt$taxon
} else {
  ncat("Using default media: ", taxon)
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

target <- paste0(src_url, "/species/", taxon)
if (!http_ping(target)) {
  ncat("Aborting, did not get HTTP status OK when pinging ", target)
  if (!interactive()) q(status = 1)
}

dest <- paste0(dest_url)
if (!http_ping(dest)) {
  ncat("Aborting, did not get HTTP status OK when pinging ", dest)
  if (!interactive()) q(status = 1)
}

# Example usage - get all image urls given a taxon uuid

taxon <- "8bae741a-2b69-4f23-838c-6c7c10114d21"  # citronfjäril
getMediaURLsForTaxonId(src_url, taxon)

taxon <- "fcffabdc-501e-489a-88af-fbd19d758dce"  # sorgmantel
getMediaURLsForTaxonId(src_url, taxon)

# Example usage - get all image urls given a vernacular name

vernacular <- "Citronfjäril"
id <- getTaxonUuidFromVernacularName(src_url, vernacular)
getMediaURLsForTaxonId(src_url, id)

vernacular <- "Sorgmantel"
id <- getTaxonUuidFromVernacularName(src_url, vernacular)
getMediaURLsForTaxonId(src_url, id)

getMediaURLsForVernacularName(src_url, "Citronfjäril")
getMediaURLsForVernacularName(src_url, "Sorgmantel")

# In order to use the API, we need some way to get the 
# scientific name for a taxon, we can scrape and
# create such a function

latin <- getScientificNameForTaxonId(src_url, 
  "8bae741a-2b69-4f23-838c-6c7c10114d21")
print(latin)

# Now can use the API which uses the latin name
df <- getMediaForLatinName(src_url, latin)
print(df)

# Now time to upload data to the new media server

# However, first we need to be able to download
# the media files that we later want to upload

# Download an image from the server and inspect it
imagefile <- downloadMedia("http://test.naturforskaren.se:8080/media/get-stream/d6690a46-a6a1-49c9-ab42-20895f08ff0c", "image/jpeg")
fileURL <- paste0("file:///", getwd(), "/", imagefile)
browseURL(fileURL)

# Upload media to the mediaserver
latin <- getScientificNameForTaxonId(src_url, 
  "8bae741a-2b69-4f23-838c-6c7c10114d21")
df <- getMediaForLatinName(src_url, latin)
print(df)

# The data frame df contains all media for a taxon
# Demonstration of uploading / posting one media to the service
r <- df[1, ]  # current record is only the first row of the data frame
fileName <- downloadMedia(r$url, r$mime)
media_id <- postMedia(dest_url, r$copyrightOwner, "public", 
  "country:Sweden", "CC BY-SA", r$title, "sv_SE", fileName)
new_media_ids <- uploadMedia(dest_url, df)

# Example usage - linking serveral media objects with one taxon object
ids <- as.character(new_media_ids)
print(ids)
res <- linkTaxonToMedias(dest_url, "8bae741a-2b69-4f23-838c-6c7c10114d21", ids)

# Example usage uploading media for a list of vernacular names
vernaculars <- c("Citronfjäril", "Sorgmantel")
uploadMediaForVernacularNames(src_url, dest_url, vernaculars)

# Example showing uploading of media data from database to new
# mediaserver for just one taxon
my_db <- connectMediaDB()
media <- getMediaDetailsFromDB(my_db)
link <- getMediaLinksFromDB(my_db)

uuids <- data.frame(link)
taxa_uuids <- unique(uuids$taxon_uuid)
print(head(taxa_uuids))

# This is the single taxon we want to show how to upload into
# the mediaserver
taxon_uuid <- taxa_uuids[1]
print(taxon_uuid)

# Retrieve all media metadata associated with the unique 
# taxon identifier provided
getMediaUuidsForTaxonUuidFromDB <- function(uuid) {
  taxon_link <- as.data.frame(filter(link, 
    taxon_uuid == uuid))
  taxon_media <- subset(as.data.frame(media), 
    media_uuid %in% taxon_link$media_uuid)
  return (taxon_media)
}

# Example usage of the function above
getMediaUuidsForTaxonUuidFromDB(taxon_uuid)

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

  # Upload the medias to the media server
  # FIXME! note that the upload function takes a parameter df 
  # which requires a df with certain columns to be present
  # not very pretty, should be changed
  
  failed_upload <- paste0("Failed uploading medias for taxon",
    taxon_uuid)
  
  mkUpload <- failwith(failed_upload, 
    uploadMedia(dest_url, current_df))

  uploaded_media_ids <- mkUpload(dest_url, current_df)
  
  ncat("Uploading related media for taxon with identifier ", 
       taxon_uuid, ": ", uploaded_media_ids)
  
  # Link the taxon to the new media ids returned from the media server
  media_ids <- as.character(uploaded_media_ids)
  
  failed_link <- paste0("Failed linking taxon with identifier", 
    taxon_uuid, " to associated media: ", media_ids)
  
  mkLink <- failwith(failed_link,
    linkTaxonToMedias(dest_url, taxon_uuid, media_ids))
  
  res <- mkLink(dest_url, taxon_uuid, media_ids)
  
  ncat("Linking taxon with identifier ", taxon_uuid, 
    " to related media: ", res)
  
  return (res)
}

# Example processing all media for one taxon
res <- processTaxonUuidFromDB(taxon_uuid)
print(res)

# Example processing all media for six first taxa in the database
res <- aaply(head(taxa_uuids), 1, processTaxonUuidFromDB, 
  .progress = "text")
print(res)

# Example processing all media for all taxa in the database
res <- aaply(taxa_uuids, 1, processTaxonUuidFromDB, 
  .progress = "text")
print(res)

ncatn("Done")
q(status = 0)