library("httr")
library("dplyr")
library("rvest")
library("magrittr")


# build urls to fetch pages and identifiers from
# production and test servers

PBASE <- "https://dina-web.net/naturalist/"
TBASE <- "http://test.naturforskaren.se/nf-naturalist/"

# pages - their urls given an identifier

prod_page <- function(id) 
  paste0(PBASE, "species/", id)

test_page <- function(id) 
  paste0(TBASE, "species/", id)

# identifiers - finding children using a given node identifier

build_prod_identifier <- function(id, locale = "sv_SE") 
  paste0(PBASE, "speciestaxonchildren/", id, "/", locale, "/1")

build_test_identifier <- function(id, locale = "sv_SE")
  paste0(TBASE, "speciestaxonchildren/", id, "/", locale, "/1")


re <- ".*(.{8}-.{4}-.{4}-.{4}-.{12}).*"

extract_uuids <- function(x) 
  gsub(re, "\\1", x, perl = TRUE)       

find_uuids <- function(x) 
  grep(re, x, perl = TRUE, value = TRUE)       

children_identifiers <- function(url) {
  res <- 
    read_html(url) %>%
    html_nodes("li") %>%
    html_attr("data-taxonid")
  return (res)
}


sample_identifiers <- function(root_id, sample_ids, n_atleast = 5) {
  res <- 
    root_id %>%
    build_prod_identifier %>%
    children_identifiers %>%
    data_frame(id = .)
  
  res <- rbind.data.frame(res, sample_ids)
  message("rows in res: ", nrow(res))
  
  if (nrow(res) < n_atleast)
    res <- sample_identifiers(res$id, sample_ids, n_atleast)
  
  return (res)  
}

diff_children <- function(id) {

  prod_ids <- 
    id %>%
    build_prod_identifier %>%
    children_identifiers
  
  test_ids <- 
    id %>%
    build_test_identifier %>%
    children_identifiers
  
  setdiff(prod_ids, test_ids)
    
}


img_src <- function(url) {
  res <- 
    read_html(url) %>%
    html_nodes("img") %>%
    html_attr("src")
  return (res)
}

filter_re <- function(x, re) 
  grep(re, x, value = TRUE, perl = TRUE)

filter_fixed <- function(x, fixed) 
  grep(fixed, x, value = TRUE, fixed = TRUE)

replace_re <- function(x, re) 
  gsub(re, "\\1", x, perl = TRUE)


diff_cc <- function(id) {
  
  cc <- function(x) x %>%
    img_src %>%
    filter_fixed("img/cc") %>%
    replace_re(".*(img/cc.*)") %>%
    unique

  prod <- id %>% prod_page %>% cc
  message("cc in prod: ", prod)
  
  test <- id %>% test_page %>% cc
  message("cc in test: ", test)

  res <- setdiff(prod, test)
  return (res)
}


is_last_image_map <- function(id) {
  
  is_last <- function(x)   
    x %>% img_src %>%
    filter_re(re) %>%
    replace_re(paste0(".*(", re, ".*)")) %>%
    unique %>%
    tail(1)
  
  prod <- id %>% prod_page %>% is_last
  message("prod last image: ", prod)
  
  test <- id %>% test_page %>% is_last
  message("test last image: ", prod)
  
  if (grep("raster", prod) > 0 & grep("raster", test) > 0)
    return (TRUE)
  
}


diff_image_count <- function(id) {

  image_count <- function(x) {
    res <- x %>% img_src %>% find_uuids %>% 
      extract_uuids %>% unique %>% length
    return (res)
  }
  
  p_count <- image_count(prod_page(id))
  message(id, ": img count in prod: ", p_count)
  
  t_count <- image_count(test_page(id))
  message(id, ": img count in test: ", t_count)
  
  if (p_count != t_count)
    warning("Please investigate ", 
      prod_page(id), " / ", test_page(id))
  
  return (p_count - t_count)
  
} 


remove_whitespace <- function(x)
  gsub("\\s{2,}", " ", x, perl = TRUE)

diff_image_labels <- function(id) {

  labels <- function(url) 
    read_html(url) %>%     
    html_nodes("figcaption") %>%
    html_text() %>% 
    remove_whitespace() %>%
    unique() %>%
    trimws()
  
  prod <- labels(id %>% prod_page)
  message("labels in prod: ", prod)
  
  test <- labels(id %>% test_page)
  message("labels in test: ", test)
  
  res <- setdiff(prod, test)
  return (res)
    
}




