library(rvest)
library(httr)
library(tibble)
library(readr)

# are the taxon uuids with images that we want to move?

birds <-  content(GET("https://birds.dina-web.net/species/"))
uuids <-
  html_nodes("ul li a") %>%
  html_attr("href") %>% 
  head(200) %>%
  
species <-
  html_nodes("ul li a") %>%
  html_text %>% 
  head(200)

latin <-
  html_nodes("ul li a") %>%
  html_nodes("em") %>%
  html_text %>% 
  head(200)

res <- tibble(species, uuids)
write_tsv(res, "~/toves-taxa.tsv")


