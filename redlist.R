#!/usr/bin/Rscript

#devtools::install_github("RcppCore/Rcpp")
#devtools::install_github("rstats-db/DBI")
#devtools::install_github("rstats-db/RMySQL")
#devtools::install_github("hadley/dplyr")

#setwd("~/repos/dina-web/rdina")

#install.packages("RMySQL")
#install.packages("dplyr")
#install.packages("DBI")

#remove.packages("RMySQL")
#remove.packages("DBI")

library("RMySQL")
library("dplyr")

# HACK: This command quickly removes all namespaces
# from the XML file (in order to simplify xml parsing)
cmd <- "sed -r 's/(a:|s:|i:|xmlns=\".*?\")//g' \\
redlistinfo.xml > rli.xml"
message("Processing redlist XML dump (removing namespaces)")
system(cmd)

# HACK: for quick XML parsing using an XML C library fr Python
message("Parsing XML, extracting relevant fields from rli.xml")
message("and converting to rli.csv")
cmd <- "python redlist-xml2csv.py"
system(cmd)

message("Creating lookup table for status_descr")
# src: https://en.wikipedia.org/wiki/Conservation_status
lookup <- read.csv2(stringsAsFactors = FALSE,
  header = FALSE, sep = ",",  file = textConnection(object =  
"Nationellt utdöd, RE, Extinct
Akut hotad, CR, Critically Endangered
Starkt hotad, EN, Endangered
Sårbar, VU, Vulnerable
Nära hotad, NT, Near Threatened
Livskraftig, LC, Least Concern
Kunskapsbrist, DD, Data Deficient,
Ej utvärderad, NE, Not Evaluated,
Ej tillämplig, NA, Not Applicable"
)) 
cols <-  c("status_swe", "status_abbrev", "status_eng")
names(lookup) <- cols

# Trim all white space from all columns
lookup <- 
  lookup %>% mutate_each(funs(trimws))

# Load the redlist data from csv file
redlist <- tbl_df(read.csv(file = "rli.csv",  
  na.strings = "", stringsAsFactors = FALSE))

# Recode some abbreviations 
# TODO: what does these mean?
abbrevs <- c("EN°", "LC°", "NT°", "VU°")
redlist$status_abbrev <- 
  with(redlist, plyr::mapvalues(status_abbrev, 
    from = abbrevs, to = substr(abbrevs, 1, 2)))

# Decorate redlist with info from lookup
res <- 
  redlist %>% 
  left_join(lookup, by = "status_abbrev")

message("Summary of data: ")
res_tally <-
  res %>% 
  group_by(status_abbrev) %>% 
  summarize(n()) %>%
  left_join(lookup, by = "status_abbrev")
print(res_tally)

message("Some dyntaxa ids had missing redlist values: ")
missing_ids <-
  res %>% 
  filter(is.na(status_abbrev)) %>%
  distinct(dyntaxa_id)
message(paste0(collapse = ", ", missing_ids$dyntaxa_id))

out <- "redlist-processed.csv"
message("Outputting ", out)
write.csv(res, out, row.names = FALSE)

# HACK: due to dplyr not allowing utf8 mysql db connections,
# we create this special function to work around that
src_mysql_nf <- function(dbname = "taxonpages_v2", 
  host = "127.0.0.1", port = 0L, user = "root", password = "", ...) {
  
  if (!requireNamespace("RMySQL", quietly = TRUE)) {
    stop("RMySQL package required to connect to mysql/mariadb", 
         call. = FALSE)
  }
  con <- dbConnect(RMySQL::MySQL(), dbname = dbname, host = host, 
    port = port, username = user, password = password, ...)
  message("Setting character set for connection")
#  res <- dbGetQuery(con, statement = "SET collation_connection = utf8_general_ci;")
  res <- dbGetQuery(con, statement = "SET NAMES utf8;")
  info <- dbGetInfo(con)
  src_sql("mysql", con, 
    info = info, disco = dplyr:::db_disconnector(con, "mysql"))  
}

message("Adding related data from db: latinname, vernacular name, uuid etc")

get_config <- function(config_file, section) {
  config_file <- "redlist-credentials.cfg"
  section <- "[Naturalist]"
  config <- readLines(config_file)
  beg <- grep(section, config, fixed = TRUE) + 1
  eoc <- length(config)
  nxt <- grep("[.*?]", config[beg:eoc])
  nxt <- ifelse(length(nxt) > 0, nxt[1], eoc)
  end <- ifelse((nxt - 1 > beg && nxt < eoc), nxt - 1, eoc)
  cfg <- grep("=", config[beg:end], value = TRUE)
  pairs <- strsplit(cfg, "=")
  namez <- trimws(sapply(pairs, "[[", 1))
  valuez <- trimws(sapply(pairs, "[[", 2))
  df <- as.data.frame(rbind(valuez), stringsAsFactors = FALSE)
  names(df) <- namez
  return (df)
}

# Make sure that cfg file is chmod 600
cfg <- get_config("redlist-credentials.cfg", "[Naturalist]")
#cfg <- get_config("redlist-credentials.cfg", "[Artdatabanken]")

if (is.null(cfg$port)) cfg$port <- 3306

con_nf <- src_mysql_nf(dbname = cfg$dbname, port = as.integer(cfg$port),
  host = cfg$host, user = cfg$user, password = cfg$password)

#con_nf <- dbConnect(drv = RMySQL::MySQL(), dbname = cfg$dbname, 
#  host = cfg$host, user = cfg$user, password = cfg$password)
                    
redlist_nf <- tbl(con_nf, "redlist")
related_taxonomy <- tbl(con_nf, "related_taxonomy")

rt <- 
  related_taxonomy %>%
  select(theirs_id, taxon_uuid) %>%
  rename(dyntaxa_id = theirs_id)

rt <- tbl_df(data.frame(rt))
rt$dyntaxa_id <- as.numeric(rt$dyntaxa_id)

latin <- 
  tbl_df(data.frame(tbl(con_nf, "taxon"))) %>% 
  select(uuid, fullname) %>%
  rename(taxon_uuid = uuid)

commonname <- 
  tbl_df(data.frame(tbl(con_nf, "commonname"))) %>% 
  filter(preferred == 1, language == "sv_SE") %>%
  select(-id, -preferred, -language) %>%
  rename(svenskt = name)

new_redlist <-
  res %>%
  left_join(rt, by = "dyntaxa_id", copy = TRUE) %>%
  left_join(latin, by = "taxon_uuid", copy = TRUE) %>%
  left_join(commonname, by = "taxon_uuid", copy = TRUE) %>%
  mutate(web_url = taxon_uuid, 
    pdf_url = paste0("//artfakta.artdatabanken.se/artfaktablad/", dyntaxa_id)) %>%
  rename(latin = fullname, id = dyntaxa_id) %>%
  mutate(status = paste0(status_swe, " (", status_abbrev, ")")) %>%
  select(id, latin, svenskt, pdf_url, web_url, status, 
         status_abbrev, status_eng, status_swe)

# Tally a summary
new_redlist %>% 
group_by(status) %>% 
summarise(count = n()) %>%
arrange(-desc(count))

# Exclude missing values
upload <- 
  new_redlist %>% 
  filter(status != "NA (NA)") %>%
  filter(!is.na(latin))

tbldf_enc <- function(tbl_df, encoding = "latin1") {
  res <- data.frame(stringsAsFactors = FALSE,
    apply(tbl_df, MARGIN = 2,
        function(x) iconv(x, to = encoding)))
  return (tbl_df(res))
}

upsert_redlist_oops <- function(tbl_df, dplyr_con, table) {
  message("Uploading to dbtable ", table)
  con <- dplyr_con$con
  df <- tbldf_enc(tbl_df, encoding = "latin1")
  if (dbExistsTable(con, name = table))
      dbRemoveTable(con, name = table)
  res <- copy_to(dplyr_con, tbl_df, table, 
           temporary = FALSE, overwrite = TRUE)
  return (res)
}

#upsert_redlist_oops(upload, con_nf, "redlist_2015")

# HACK: this is a nasty workaround to upload data to 
# mysql db (latin1 db) with the correct character set
upsert_redlist <- function(csvfile, dplyr_con, tablename) {
  message("Uploading to dbtable ", tablename)
  con <- dplyr_con$con
  res <- dbWriteTable(con, tablename, csvfile, overwrite = TRUE)
  return (res)
}

# HACK: manual step: due to bug in RMySQL library, you need to check
# the character set / encoding of the mysql database you are uploading
# the data to... Depending on whether it is  "utf8" or "latin1" 
# you need to use the corresponding write.csv command below (uncommment!) 
out <- "redlist_2015.csv"
message("Outputting redlist data into csv file ", out)
#write.csv(tbldf_enc(upload), out, row.names = FALSE, fileEncoding = "latin1")
write.csv(tbldf_enc(upload), out, row.names = FALSE, fileEncoding = "utf8")

dbtable <- "redlist_2015"
message("Uploading data (using nasty trick) into db table ", dbtable)
res <- upsert_redlist(out, con_nf, dbtable)

# Bear and wolf
# upload %>% filter(svenskt == "björn")
# upload %>% filter(svenskt == "varg")

missing_links <- 
  new_redlist %>% 
  filter(is.na(latin)) %>% 
  distinct(id) %>%
  select(id, status)

out <- "redlist-broken-links.csv"
message("Listing non-matching taxon_uuid <-> dyntaxa_id pairs from nf db")
message("in file ", out)
write.csv(missing_links, out, row.names = FALSE)

