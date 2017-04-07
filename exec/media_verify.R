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
args <- "tgt_url, t, 2, character, source URL for db with media metadata
src_url, s, 2, character, source URL for db with media metadata"

# defaults
opt <- get_args(args)

tgt_url <- "dina-db.nrm.se:3306"
src_url <- "ingimar-hp-elitebook-8470p:3306"

if (!is.null(opt$tgt_url)) {
  tgt_url <- opt$tgt_url
} else {
  ncat("Using default db URL: ", tgt_url)
}

if (!is.null(opt$src_url)) {
  src_url <- opt$src_url
} else {
  ncat("Using default src URL: ", src_url)
}

cfgfile <- system.file("exec/media.conf", package = "biotools")
if (!interactive()) cfgfile <- "media.conf"
ap
if (file.exists(cfgfile)) {
  message("Found config file, ", cfgfile, " overriding and using settings therein")
  cfg <- get_config(cfgfile)
  db_url <- cfg$db_url
  db_pass <- cfg$db_pass
  db_user <- cfg$db_user
  src_url <- cfg$src_url
  tgt_url <- cfg$dest_url  # NB! dest_url is the .conf setting
}

# Processing media metadata from database, 
# migrating data to new mediaserver

tgt_port <- as.numeric(str_replace(str_extract(tgt_url, ":(\\d)+"), ":", ""))
if (is.na(tgt_port)) tgt_port = 3306
ncat("Using tgt port ", tgt_port)

src_port <- as.numeric(str_replace(str_extract(src_url, ":(\\d)+"), ":", ""))
if (is.na(src_port)) src_port = 3306
ncat("Using src port ", src_port)

tgt_host<- str_replace(tgt_url, "(.*?):.*", "\\1")
ncat("Using tgt host ", tgt_host)

src_host<- str_replace(src_url, "(.*?):.*", "\\1")
ncat("Using src host ", src_host)

nf_db <- connectMediaDB(tgt_host, tgt_port, db_pass = NA)
m <- unique(data.frame(getMediaDetailsFromDB(nf_db)))
l <- unique(data.frame(getMediaLinksFromDB(nf_db)))

pw <- 
  c("SQADkoKO2u8/+i80U7WsL0E+J67fj4zUJtibFuHN/cPjw5avxrFNzGhyVl2nYXdH", 
    "5ZLYjwbvzSauDBoz+dXILUIiRICNhY6rAj2cQfEouUUA/mNvUBmpE5iJHlHF54CJ", 
    "yV+Wo4ioIe2JkC3XwD/2w5iUR8ckpSHzCFocyMIZFPWLbnhSqN+AExRxi2oPONTv", 
    "HQm8iUxggpMmuoq7tybNqFrV3RFks72v0yT7fs9ieX/2GfkdHDPUt6+KlfNoFpbM", 
    "+9xBDZE8hFyD3YkF/OXQAFuWKkNFlFKMFbDjdcc6Nyo8zMlTU6KnoyvtIROwOpdH", 
    "XcRtPDqWo4wBpwB7QF/pgQ==")
    
media_db <- src_mysql(host = src_host, port = src_port,
  dbname = "nrm_media", user = "media", 
  password = gpg_decrypt(pw))

dets <- "select TAG_VALUE from DETERMINATION"
medias <- "select filename as mime, left(FILENAME,36) as uuid  from MEDIA"

d <- unique(data.frame(tbl(media_db, sql(dets))))
i <- unique(data.frame(tbl(media_db, sql(medias))))

# Which taxon identifiers are in nf but not in media servers?
ids <- setdiff(t$taxon_uuid, d$TAG_VALUE)
delta <- subset(t, taxon_uuid %in% ids)

if (nrow(delta) > 0) delta$url <- "http://naturforskaren.se/species/"

ncat("This is the delta between the source", 
  "(nf) and the target (media server)")
print(delta)


ncat("These are the relevant taxa")
cat(paste(collapse = "\r\n", unique(delta$taxon_uuid)))

ncat("These are the relevant taxa URLs")
cat(paste(collapse = "\r\n", unique(paste(sep = "", delta$url, delta$taxon_uuid))))
    
md <- setdiff(m$media_uuid, i$uuid)
mdelta <- subset(m, media_uuid %in% md)

ext <- sapply(strsplit(mdelta$media_type, "/"), "[[", 2)
murl <- paste(sep = "", "http://naturforskaren.se/media/get-stream/", 
      mdelta$media_uuid, ".", ext)

ncat("These are the relevant media")
cat(paste(collapse = "\n", murl))

ncat("Those media are linked to these taxa")
cat(paste(collapse = "\n", subset(l, media_uuid %in% mdelta$media_uuid)$taxon_uuid))
