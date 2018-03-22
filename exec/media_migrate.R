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

birds <-   
  mt %>% filter(nchar(copyright_owner_link) > 0) %>% 
  arrange(taxon_uuid, url, sortOrder) #%>%
#  select(url, taxon_uuid, copyright_owner_link, original_title, original_title_link)

#birds %>% distinct(copyright_owner_link)

#browseURL(birds$url[1])  

#  select uuid, original_title_link from media where original_title_link is not null group by uuid, original_title_link;

birds <- 
  birds %>% rename(
    legend = original_title, 
    owner = copyrightOwner, 
    fileNameWExt = fileName
  ) %>%
  mutate(
    access = "public",
    tags = "origin:Naturforskaren"
  ) %>% distinct()

# downloadMedia(birds$url, birds$mime)


media_download <- function(df, .pb = NULL) {
  if (.pb$i < .pb$n) .pb$tick()$print()
  fileName <- file.path(tempdir(), paste0(df$fileNameWExt, ".", df$mime))
  res <- map2_int(df$url, fileName, function(x, y) 
    download.file(url = x, method = "curl", destfile = y, quiet = TRUE))  
  tibble(success = res == 0, path = fileName)
}

df <- birds #%>% slice(1:3)
pb <- progress_estimated(nrow(df), 0)
dls <- media_download(df, .pb = pb)

#dls <- 
#  tibble(fileName = dir("/tmp/RtmpbXvZlI", full.names = TRUE)) %>%
#  mutate(fileNameWExt = gsub("/tmp/RtmpbXvZlI/(.*?)([.].*)", "\\1", fileName))

dls2 <- 
  dls %>% select(fileName = path) %>%
  mutate(fileNameWExt = gsub("/tmp/RtmpnsflCr/(.*?)([.].*)", "\\1", fileName))


df <- birds %>% inner_join(dls2)

#browseURL(dls$path[3])

dest_url <- "https://beta-media.dina-web.net/MediaServerResteasy/media/upload-file/base64"
#dest_url2 <- "http://thinkpad.nrm.se:8080/MediaServerResteasy/media/upload-file/base64"

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

#  birds %>% filter(taxon_uuid == "004e6a45-c480-4420-9fcb-8225279094c0")

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

#saveRDS(res, "~/repos/dina-web/naturalist-docker/res.Rda")

#rest <- res %>% slice(499:593)
#rest <- res %>% slice(498)
#final_res <- res %>% slice(1:498) %>% bind_rows(res2)
#View(final_res)

# clean dupes in mediadb looking at HASH

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
media_query <- "select * from MEDIA;"
dupes <- as_tibble(collect(dbGetQuery(con, media_query)))
poolClose(pool)

temp <- 
  dupes %>% group_by(HASH) %>% summarize(count = n()) %>% filter(count > 2) %>%
  inner_join(dupes) %>% filter(count < 10) %>% arrange(desc(count)) %>%
  filter(OWNER != "Bert Gustafsson") %>%
  filter(!UUID %in% c("519aea58-fe73-4fef-9379-0978c2f668f4", "79851449-26af-4c40-a57f-9cd73f751fc5"))

delete_det <- paste0("delete from DETERMINATION where MEDIA_UUID='", temp$UUID, "';")
delete_med <- paste0("delete from MEDIA where UUID='", temp$UUID, "';")
delete_lic <- paste0("delete from MEDIA_X_LIC where MEDIA_ID='", temp$UUID, "';")
delete_img <- paste0("delete from IMAGE where UUID='", temp$UUID, "';")
delete_tag <- paste0("delete from TAGS where MEDIA_UUID='", temp$UUID, "';")

con <- poolCheckout(pool)
dbGetQuery(con,'SET NAMES utf8')
map(delete_det, function(x) dbGetQuery(con, x))
map(delete_lic, function(x) dbGetQuery(con, x))
map(delete_img, function(x) dbGetQuery(con, x))
map(delete_tag, function(x) dbGetQuery(con, x))
map(delete_med, function(x) dbGetQuery(con, x))
poolClose(pool)

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

#dupes %>% #filter(grepl("/tmp/", FILENAME)) %>% #slice(1) %>% t()
#  mutate(uuid = gsub("/tmp/RtmpbXvZlI/(.*?)[.].*", "\\1", FILENAME)) %>%
#  filter(HASH == "00cc9486cffcc1eee650b541a9ad66bb")

#dupes %>% filter(grepl("/tmp/", FILENAME)) %>% group_by(HASH) %>% summarize(count = n()) %>% filter(count > 1)

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

# are all with just one media_id actually not linked?
# errorz <- linkz[which(linkz_log == "ERROR")]
# 
# errorz_linkz_log <- map(errorz[2:22], function(x) media_taxon_link(
#   src_url = "https://naturforskaren.se",
#   taxon_uuid = x$taxon_uuid,
#   media_ids = x$media_ids
#   )
# )

# sanity checks... inspecting a few taxon uuids

browse_uuids <- paste0( # gräsand! # björn!
  "https://beta-naturforskaren.dina-web.net/nf-naturalist/species/", c(
  "fcffabdc-501e-489a-88af-fbd19d758dce", # Sorgmantel
  "66118ad5-919b-4347-8981-e0944e6646f7", # Sävvårtbitare
  "3301bf9b-68c8-453f-b32d-8e99ba423290", # Mörkspräcklig hornmal
  "2d5a6de0-e1c5-44cf-be16-044645d2a1c7", # Kohornsmal
  "d8d88968-3282-4efc-9614-28e59a13ee69", # musslor
  "6e18dcd9-a549-469d-8d9b-ee0f336b545a", # mård
  "e373547f-6f0d-42c4-9b40-7f7215e2c25f", # hopprätvingar
  "56a83aa5-ac19-4d79-a1ad-32eb3aaf506e", # björn
  "263f8ef2-e085-408f-90b1-e6e4decfc4cb", # fjällämmel
  "1e90b156-00f9-4e73-a018-8ab67db956f5" # hasselmus
))

map(browse_uuids, browseURL)

if (!interactive()) q(status = 0)