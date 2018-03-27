lost_birds <- 
"Alfågel - (naturforskaren = 3 bilder och 1 ljud)
Blå kärrhök - (naturforskaren = 2 bilder, inget ljud)
Blåmes - (naturforskaren = 3 bilder och 1 ljud)
Bläsand - (naturforskaren = 2 bilder och 1 ljud)
Bofink - (naturforskaren = 3 bilder och 1 ljud)
Brunand - (naturforskaren = 3 bilder och 1 ljud)
Brushane - (naturforskaren = 3 bilder, inget ljud)
Dalripa - (naturforskaren = 3 bilder och 1 ljud)
Domherre - (naturforskaren = 3 bilder och 1 ljud)
Ejder - (naturforskaren = 3 bilder och 1 ljud)
Fasan - (naturforskaren = 2 bilder och 1 ljud)
Fjällripa - (naturforskaren = 2 bilder och 1 ljud)
Gök - (naturforskaren = 1 bild och 1 ljud)
Grågås - (naturforskaren = 3 bilder och 1 ljud)
Gråhakedopping - (naturforskaren = 3 bilder och 1 ljud)
Gräsand - (naturforskaren = 3 bilder och 1 ljud)
Gravand - (naturforskaren = 3 bilder och 1 ljud)
Havsörn - (naturforskaren = 3 bilder och 1 ljud)
Järpe - (naturforskaren = 2 bilder och 1 ljud)
Kanadagås - (naturforskaren = 3 bilder och 1 ljud)
Knölsvan - (naturforskaren = 3 bilder och 1 ljud)
Koltrast - (naturforskaren = 3 bilder och 1 ljud)
Korp - (naturforskaren = 2 bilder och 1 ljud)
Kricka - (naturforskaren = 2 bilder och 1 ljud)
Orre - (naturforskaren = 3 bilder och 1 ljud)
Rapphöna - (naturforskaren = 2 bilder och 1 ljud)
Ringduva - (naturforskaren = 3 bilder och 1 ljud)
Rörhöna - (naturforskaren = 2 bilder och 1 ljud)
Sädesärla - (naturforskaren = 3 bilder och 1 ljud)
Sädgås - (naturforskaren = 1 bilder och 1 ljud)
Salskrake - (naturforskaren = 3 bilder)
Sångsvan - (naturforskaren = 3 bilder och 1 ljud)
Skäggdopping - (naturforskaren = 3 bilder och 1 ljud)
Skata - (naturforskaren = 3 bilder och 1 ljud)
Skedand - (naturforskaren = 2 bilder och 1 ljud)
Småskrake - (naturforskaren = 3 bilder och 1 ljud)
Snatterand - (naturforskaren = 3 bilder och 1 ljud)
Storskrake - (naturforskaren = 3 bilder och 1 ljud)
Svärta - (naturforskaren = 2 bilder och 1 ljud)
Svarthakedopping - (naturforskaren = 3 bilder och 1 ljud)
Talgoxe - (naturforskaren = 3 bilder och 1 ljud)
Tamduva - (naturforskaren = 3 bilder och 1 ljud)
Tjäder - (naturforskaren = 2 bilder och 1 ljud)
Tornseglare - (naturforskaren = 2 bilder och 1 ljud)
Turkduva - (naturforskaren = 2 bilder och 1 ljud)
Vigg - (naturforskaren = 3 bilder och 1 ljud)"

library(readr)

vernaculars <- 
  read_lines(lost_birds) %>%
  str_extract("\\w+")

library(httr)

taxon_from_vernacular <- function(vernacular) {
  src_url <-"https://dina-web.net/naturalist"
  target <- paste0(src_url, "/search/", URLencode(vernacular))
  search <- paste0("/species/.*", vernacular, ".*")
  req <- GET(target)
  uuid <- str_extract(content(req, as = "text"), search)
  uuid <- str_extract(uuid, ".{8}-.{4}-.{4}-.{4}-.{12}")
  return (uuid)
}

uuid <- map_chr(vernaculars, taxon_from_vernacular)

lost_birds <- tibble(vernaculars, uuid)

# exception for Lyrurus tetrix and Blå kärrhök
idx <- which(lost_birds$vernaculars == "Orre")
lost_birds[idx, ]$uuid <- "95417f73-caf1-4146-b2b9-b481d6bb852e"
lost_birds[2, ]$vernaculars <- "Blå kärrhök"
lost_birds[2, ]$uuid <- taxon_from_vernacular("Blå kärrhök")

write_excel_csv(lost_birds, "~/.cache/R/lost_birds.csv")
