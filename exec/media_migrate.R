#!/usr/bin/Rscript

library(RMySQL)
library(pool)
library(tidyverse)
library(tibble)
library(stringr)
library(magrittr)  

#if (interactive()) 
#  Sys.setenv(DINAPASS = askForPassword())

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "taxonpages_v2",
  host = "127.0.0.1",
  port = 3306,
  username = "naturalist",
  password = rstudioapi::askForPassword() #   password = Sys.getenv("DINAPASS")
)

con <- poolCheckout(pool)
#my_db <- connectMediaDB(db_hostname, db_user, db_port, db_pass)
#my_db <- poolCheckout(pool)
dbGetQuery(con,'SET NAMES utf8')

media_query <- 
"select 
  uuid as media_uuid, 
  media_type,
  copyright_owner,
  comment,
  copyright_owner_link,
  original_title,
  original_title_link,
  l.id as license_id,
  l.abbrev as license_abbrev, 
  l.issuer as license_short, 
  l.uri as license_url, 
  l.name as license_type,
  tml.legend,
  tml.language
from 
  media m 
  left join licence l on m.licence_id = l.id
  left join taxonpage_media tm on tm.media_uuid = m.uuid
  left join taxonpage_media_legend tml on tml.taxonpage_media_id = tm.id
where 
  media_type <> 'text/html'
order by
  media_uuid asc
"

media <- as_tibble(collect(dbGetQuery(con, media_query)))

link_query <- 
"select 
  tm.taxonpage_id as page_id,
  s.name as page_status,
  tp.taxon_uuid,
  tm.sort_order,
  tm.media_uuid
from
  taxonpage_media tm 
  left join taxonpage tp on tp.id = tm.taxonpage_id
  left join status_type s on tp.status_id = s.id
  inner join (select max(taxonpage.id) as page_id from taxonpage
              left join taxonpage_media on taxonpage.id = taxonpage_media.taxonpage_id
              group by taxon_uuid, language) tt on tt.page_id = tp.id
order by
  page_id asc, taxon_uuid asc, sort_order asc"


link <- as_tibble(collect(dbGetQuery(con, link_query)))
poolClose(pool)

uuids <- link
taxa_uuids <- unique(uuids$taxon_uuid)

media_from_taxon <- function(id) {

  ids <- 
    link %>% 
    filter(taxon_uuid %in% id) %>%
    .$media_uuid
  
  media_df <- 
    media %>%
    filter(media_uuid %in% ids) %>%
    distinct()
    
  tm <- 
    link %>% 
    filter(taxon_uuid %in% id) %>%
    inner_join(media_df) %>%
    distinct()
  
  return(tm)
}

mt <- 
  media_from_taxon(uuids$taxon_uuid) %>%
  mutate(mime = sapply(str_split(media_type, fixed("/")), "[", 2)) %>%
  mutate(url = paste0("https://dina-web.net/naturalist/media/get-stream/", media_uuid)) %>%
  select(
    taxon_uuid, media_uuid,
    copyrightOwner = copyright_owner,
    license = license_abbrev,
    title = legend,
    fileName = media_uuid,
    url,
    mime,
    language = language,
    sortOrder = sort_order,
    copyright_owner_link,
    original_title,
    original_title_link
  )

# taxa with no "copyright_owner_link" ordered by their sort order
#  select uuid, original_title_link from media where original_title_link is not null group by uuid, original_title_link;

# uuids2 <- 
#   mt %>% 
#   filter(nchar(copyright_owner_link) == 0) %>% 
#   arrange(taxon_uuid, sortOrder) %>%
#   select(taxon_uuid) %>% .$taxon_uuid
# 
# birds <-   
#   mt %>% 
#   filter(taxon_uuid %in% uuids2) %>% 
#   arrange(taxon_uuid, sortOrder)
# 
# View(birds)
# 
# birds_uuids <- unique(birds$taxon_uuid)

taxa <- 
  mt %>% rename(
    legend = original_title, 
    owner = copyrightOwner, 
    fileNameWExt = fileName
  ) %>%
  mutate(
    access = "public",
    tags = "origin:Naturforskaren"
  ) %>% distinct()

# downloadMedia(birds$url, birds$mime)
#birds %>% head(5)

media_download <- function(df, .pb = NULL) {
  if (.pb$i < .pb$n) .pb$tick()$print()
  fileName <- file.path(tempdir(), paste0(df$fileNameWExt, ".", df$mime))
  res <- map2_int(df$url, fileName, function(x, y) 
    download.file(url = x, method = "curl", destfile = y, quiet = TRUE))  
  tibble(success = res == 0, path = fileName)
}

message("Count of distinct taxa: ",
  df %>% summarize(count = n_distinct(taxon_uuid))
)

message("Count of distinct media: ",
  df %>% summarize(count = n_distinct(fileNameWExt))
)

# if not using all taxa, here the list of taxa can be changed
# given uuids taken from a file
checklist <- readr::read_csv("~/.cache/R/lost_taxa.csv")
df <- taxa %>% filter(taxon_uuid %in% checklist$uuid)
pb <- progress_estimated(nrow(df), 0)
dls <- media_download(df, .pb = pb)

dls <- 
  dls %>% select(fileName = path) %>%
  mutate(fileNameWExt = gsub("/tmp/Rtmpm2qVY2/(.*?)([.].*)", "\\1", fileName))


df <- taxa %>% inner_join(dls)

#browseURL(dls$path[3])

dest_url <- "https://beta-media.dina-web.net/MediaServerResteasy/media/upload-file/base64"

library(httr)
library(caTools)

media_post <- function(destination, 
  owner = "Unknown owner",
  access = "Unknown access", 
  tags = "Unknown tags",
  legend = "Unknown legend",
  language = "Unknown language",
  licenseType = "Unknown license type", 
  fileName = "Unknown filename", 
  export = FALSE, 
  alt = "Unknown alt text",
  copyright_owner_link = "Unknown owner link",
  original_title = "Unknown title",
  original_title_link = "Unknown title link"
  ) {

  b64 <- base64encode(read_file_raw(fileName))

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
    alt = alt,
    copyrightOwnerLink = copyright_owner_link,
    originalTitle = original_title,
    originalTitleLink = original_title_link
  ))
  
  message("POST ", fileName, " ", original_title, " to ", destination)
  
  res <- POST(destination, body = body_json, 
    add_headers("Content-Type" = "application/json"))

  if (status_code(res) != 201)
    message("ERROR, POST result:", str(res))
  
  return (content(res, as = "text", encoding = "UTF-8"))
}

# post all the images to the image server
res <- 
  df %>%
  rowwise() %>%
  mutate(new_media_id = media_post(destination = dest_url,
    owner = owner, access = access, tags = tags, legend = title,
    language = language, licenseType = as.character(license),
    fileName = as.character(fileName), export = FALSE, alt = title, 
    copyright_owner_link = copyright_owner_link, 
    original_title_link = original_title_link, 
    original_title = legend))

media_taxon_link <- function(src_url, taxon_uuid, media_ids) {

  destination <- paste0(
    "https://beta-media.dina-web.net/MediaServerResteasy/", 
    "media/postJSONWithLIsta"
  )
  
  body_json <- RJSONIO::toJSON(x = list(
    typeOfSystem = "NF_TAXON",
    taxonUUID = taxon_uuid,
    nameOfSystem = "NF_SYSTEM",
    systemURL = src_url,
    mediaList = I(media_ids)
  ))
  message(body_json)
  res <- POST(destination, body = body_json, 
    add_headers("Content-Type" = "application/json"))
    
  if (status_code(res) == 200) {
    res <- "OK (STATUS 200)"
  } else {
    message("ERROR: ", str(res))
    res <- "ERROR"
  }
  
  return(res)
}

# link taxon uuids to new media uuids in media server

mtl <- function(x) list(
  taxon_uuid = x, 
  media_ids = res %>% filter(taxon_uuid == x) %>% select(new_media_id) %>% .$new_media_id
)

linkz <- map(unique(res$taxon_uuid), mtl)

linkz_log <- map(linkz, function(x) media_taxon_link(
  src_url = "https://naturforskaren.se",
  taxon_uuid = x$taxon_uuid,
  media_ids = x$media_ids
  )
)

# errorz <- linkz[which(linkz_log == "ERROR")]
#saveRDS(linkz, file = "~/repos/dina-web/naturalist-docker/linkz.Rda")

# fix errors in RESTFUL_STREAM fields in MEDIA table (doubled sla)

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "nf_media",
  host = "127.0.0.1",
  port = 13306,
  username = "mediaserver",
  password = rstudioapi::askForPassword() #   password = Sys.getenv("DINAPASS")
)

con <- poolCheckout(pool)
dbGetQuery(con,'SET NAMES utf8')
query_fix <- "update MEDIA
set RESTFUL_STREAM = REPLACE(RESTFUL_STREAM, 'media/stream//', 'media/stream/')
where RESTFUL_STREAM LIKE 'media/stream//%';"
dbGetQuery(con, query_fix)
poolClose(pool)

if (!interactive()) q(status = 0)