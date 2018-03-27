#!/usr/bin/Rscript

library(RMySQL)
library(pool)
library(tidyverse)

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
  filter(!UUID %in% c("519aea58-fe73-4fef-9379-0978c2f668f4", "79851449-26af-4c40-a57f-9cd73f751fc5")) #%>%
#  filter(grepl(/tmp/RtmpQXI7yh/)

View(temp)

# What should be done about these dupes? Which should be deleted?

delete_dupes <- function() {
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
}

#dupes %>% #filter(grepl("/tmp/", FILENAME)) %>% #slice(1) %>% t()
#  mutate(uuid = gsub("/tmp/RtmpbXvZlI/(.*?)[.].*", "\\1", FILENAME)) %>%
#  filter(HASH == "00cc9486cffcc1eee650b541a9ad66bb")

#dupes %>% filter(grepl("/tmp/", FILENAME)) %>% group_by(HASH) %>% summarize(count = n()) %>% filter(count > 1)


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

# update MEDIA
# set RESTFUL_STREAM = REPLACE(RESTFUL_STREAM, 'media/stream//', 'media/stream/')
# where RESTFUL_STREAM LIKE 'media/stream//%';

media_db <- con
dets <- "select TAG_VALUE from DETERMINATION"
#medias <- "select filename as mime, right(FILENAME,36) as uuid  from MEDIA"
medias <- "select filename as mime from MEDIA"



d <- unique(data.frame(tbl(media_db, sql(dets))))

i <- 
  unique(collect(tbl(media_db, sql(medias)))) %>% 
  mutate(uuid = gsub("(/tmp/RtmpQXI7yh/)*(.*?)([.].*)", "\\2", mime))


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
  
t <- link
m <- media

# Which taxon identifiers are in nf but not in media server?
ids <- setdiff(t$taxon_uuid, d$TAG_VALUE)
delta <- subset(t, taxon_uuid %in% ids)

taxa_diff <- 
  mt %>% 
  filter(taxon_uuid %in% ids) %>% 
  group_by(taxon_uuid) %>%
  distinct(taxon_uuid) %>%
  mutate(vernaculars = NA) %>%
  rename(uuid = taxon_uuid) %>%
  select(vernaculars, uuid)

write_excel_csv(taxa_diff, "~/.cache/R/lost_taxa.csv")

if (nrow(delta) > 0) delta$url <- "http://naturforskaren.se/species/"

message("These are the relevant taxa: ", paste(collpase = "\n", unique(delta$taxon_uuid)))

if (!interactive()) q(status = 0)
