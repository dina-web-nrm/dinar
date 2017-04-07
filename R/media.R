#require("httr")
#require("R.utils")
#require("dplyr")

#' Scrapes the given The Naturalist url for the given taxon
#' and returns any media urls discovered
#'@param url a The Naturalist base url
#'@param taxon a The Naturalist taxon identifier
#'@return character vector with media urls
#'@export 
#'@examples
#'getMediaURLsForTaxonId("http://dina-web.net/naturalist", "/species/8bae741a-2b69-4f23-838c-6c7c10114d21")
getMediaURLsForTaxonId <- function(url, taxon) {
  
  src_url <- url
  target <- paste0(src_url, "/species/", taxon)
  req <- GET(target)
  
  media_hrefs <- unlist(str_extract_all(content(req, as = "text"), 
    "href=\"/media/get-stream/.*?\""))
  
  media_urls <- str_replace_all(media_hrefs, 
    "href=\"(/media/get-stream/.*?)\"", "\\1")                
  
  res <- paste0(src_url, media_urls)
  return(res)
}

#' Uses the search function of The Naturalist to find the 
#' unique taxon identifier from the vernacular name
#'@param url a The Naturalist base url
#'@param vernacular name
#'@return taxon identifier
#'@export
#'@examples
#'getTaxonUuidFromVernacularName("http://dina-web.net/naturalist", "Citronfjäril")
getTaxonUuidFromVernacularName <- function(url, vernacular) {
  src_url <- url
  target <- paste0(src_url, "/search/", URLencode(vernacular))
  req <- GET(target)    
  uuid <- str_extract(req$url, "/species/.*")
  uuid <- str_extract(uuid, ".{8}-.{4}-.{4}-.{4}-.{12}")
  return (uuid)
}

#' Find The Naturalist media urls using vernacular name
#'@param url a The Naturalist base url
#'@param vernacular name
#'@return character vector of media urls
#'@export
#'@examples
#'getMediaURLsForVernacularName("http://dina-web.net/naturalist", "Citronfjäril")
getMediaURLsForVernacularName <- function(url, vernacular) {
  id <- getTaxonUuidFromVernacularName(url, vernacular)
  res <- getMediaURLsForTaxonId(url, id)
  return(res)
}

#' Get the scientific name from The Naturalist for a given taxon identifier
#'@param url a The Naturalist base url
#'@param taxon_uuid a unique taxon identifier
#'@return latin name
#'@export
#'@examples
#'getScientificNameForTaxonId("http://dina-web.net/naturalist", "8bae741a-2b69-4f23-838c-6c7c10114d21")
getScientificNameForTaxonId <- function(url, taxon_uuid) {
  src_url <- url
  target <- paste0(src_url, "/species/", URLencode(taxon_uuid))
  req <- GET(target)    
  html <- content(req, as = "text")
  # We look for something like <span class="taxon_latin">Gonepteryx rhamni </span>
  latin <- str_extract(html, "<span class=\"taxon_latin\">.*?</span>")
  latin <- str_replace(latin, "<span class=\"taxon_latin\">(.*?)</span>", "\\1")
  res <- str_trim(latin)
  return (res)  
}

#' Get media from the API at The Naturalist for a given latin name
#'@param url a The Naturalist base url
#'@param latin a latin name identifier
#'@return character vector of media urls
#'@export
#'@examples
#'getMediaForLatinName("http://dina-web.net/naturalist", "Gonepteryx rhamni")
getMediaForLatinName <- function(url, latin, locale = "?locale=sv_SE") {
  target <- paste0(url, "/api/get/", URLencode(latin), locale)
  res <- content(GET(target))
  require("plyr")
  medias <- ldply(res$media, data.frame)
  return (medias)
}

#' Download media from The Naturalist and persist to disk
#'@param url a The Naturalist media url
#'@param mime the MIME type of the media
#'@return character vector of file name for the downloaded file
#'@export
#'@examples
#'\dontrun{
#'downloadMedia("http://dina-web.net/naturalist/media/get-stream/d6690a46-a6a1-49c9-ab42-20895f08ff0c", "image/jpeg")
#'}
downloadMedia <- function(url, mime) {
  medianame <- ldply(str_split(url, "/"), rev)$V1
  extension <- ldply(str_split(mime, "/"), rev)$V1
  fileName <- paste0(medianame, ".", extension)
  res <- download.file(url, method = "curl", destfile = fileName, quiet = TRUE)  
  return(fileName)
}

#' Upload one media to the media server and return the resulting response
#'@param url the media server url to be used
#'@param owner the media owner
#'@param access the access level ("private" or "public")
#'@param tags the tags for the media
#'@param license the license applicable for the media
#'@param legend the media legend / description
#'@param language the language for the media
#'@param fileName the 
#'@return response from media server
#'@export
#'@examples
#'\dontrun{
#'latin <- "Gonepteryx rhamni"
#'df <- getMediaForLatinName(latin)
#'r <- df[1, ]  # current record
#'fileName <- downloadMedia(r$url, r$mime)
#'media_id <- postMedia(r$copyrightOwner, "public", 
#'  "country:Sweden", "CC BY-SA", r$title, "sv_SE", fileName)
#'}
postMedia <- function(url, owner, access, tags, 
                      license, legend, language, fileName) {
  
  dest_url <- url
  destination <- paste0(dest_url, "media/upload-file")  
  valid_license <- license %in% c("CC BY", "CC BY-NC", "CC BY-NC-ND",
                                  "CC BY-NC-SA", "CC BY-ND", "CC BY-SA")
  
  if (!valid_license)
    stop("Not valid license string, you used", license)
  
  res <- POST(destination, body = list(
    owner = owner,
    access = access,
    tags = tags,
    legend = legend,
    licenseType = license,
    #    language = language,
    legendLanguage = language,
    selectedFile = upload_file(fileName),
    fileName = fileName))
  
  return(content(res))
  
}

#'Upload several media to the media server and return the resulting response
#'@param url the media server url to be used
#'@param df the data frame specifying the media to be uploaded 
#'@return response with identifiers from media server
#'@export
#'@examples
#'\dontrun{
#'df <- getMediaForLatinName(latin)
#'media_ids <- uploadMedia(df)
#'}
uploadMedia <- function(url, df) {
  
  # Download all the files
  media_files <- daply(df, c("sortOrder"), 
    function(x) downloadMedia(x$url, x$mime))
  
  upload <- data.frame(
    sortOrder = as.character(df$sortOrder),
    owner = as.character(df$copyrightOwner), 
    access = "public",
    tags = "origin:Naturforskaren",
    license = as.character(df$license),
    legend = as.character(df$title), 
    language = as.character(df$language), 
    fileName = as.character(media_files))
  
  # Upload all the files
  new_media_ids <- daply(upload, c("sortOrder"),  
    function(x) postMedia(url, 
      x$owner, x$access, x$tags, as.character(x$license),
      x$legend, x$language, x$fileName))  
  
  # Delete temporary downloads
  unlink(upload$fileName)
  
  return (new_media_ids)
}  

#'Upload several media to the media server given a vector of vernacular names
#'@param src_url a The Naturalist server base url to be used for downloading
#'@param dest_url a media server base url to be used for uploading
#'@param vernaculars a character vector of vernacular names to process 
#'@return response with identifiers from media server
#'@export
#'@examples
#'\dontrun{
#'vernaculars <- c("Citronfjäril", "Sorgmantel")
#'uploadMediaForVernacularNames(vernaculars)
#'}
uploadMediaForVernacularNames <- function(src_url, dest_url, vernaculars) {
  
  taxon_uuids <- laply(vernaculars, getTaxonUuidFromVernacularName(url = src_url))
  latin_names <- laply(taxon_uuids, getScientificNameForTaxonId(url = src_url))
  
  l <- data.frame(taxon_uuid = taxon_uuids, latin_name = latin_names)
  
  uploadAndLink <- function(taxon_uuid, latin_name) {
    df <- getMediaForLatinName(src_url, latin_name)
    new_media_ids <- uploadMedia(dest_url, df)
    ids <- as.character(new_media_ids)
    res <- linkTaxonToMedias(dest_url, taxon_uuid, ids)
    return (res)
  }
  
  out <- ddply(l, c("latin_name"), function(x) 
    uploadAndLink(x$taxon_uuid, x$latin_name), .progress="text")
  
  return (out)
  
}

#'Link a taxon in the media server to associated media
#'@param url the media server base url to be used
#'@param taxon_uuid the taxon identifier 
#'@param media_ids the media identifiers to associate with the taxon 
#'@return response with identifiers from media server
#'@export
#'@examples
#'\dontrun{
#'citron_ids <- as.character(citron_media_ids)
#'res <- linkTaxonToMedias("8bae741a-2b69-4f23-838c-6c7c10114d21", ids)
#'}
linkTaxonToMedias <- function(url, taxon_uuid, media_ids) {
  
  destination <- paste0(url, "media/postJSONWithLIsta")
    
  body_json <- rjson::toJSON(list(
    typeOfSystem = "NF_TAXON",
    taxonUUID = taxon_uuid,
    nameOfSystem = "NF_SYSTEM",
    systemURL = src_url,
    mediaList = media_ids
  ))
  
  res <- POST(destination, body = body_json, 
              add_headers("Content-Type" = "application/json"))
  
  return(content(res, as = "text"))
}

#'Connect to Media database directly
#'@param db_hostname a database hostname
#'@return a dplyr database src connection
#'@export
#'@examples
#'my_db <- connectMediaDB()
connectMediaDB <- function(db_hostname = "localhost", db_user = "root", 
                           db_port = 3306, db_pass = NA) {
  
  # Generate encrypted password
  # cat(gpg_inline("pw", gpg_encrypt("secretpasshere!"))
  pw <- 
    c("KFJcyML/mmeLTO0LlA2DdGst1V3RjIyISoxW/eMzcRrmB/B6wn+RmToITZWeDp+W", 
      "4YQmXDDv3Y2RT5gXy6wEc5FGXzmKlwIfTw+JZ8VJFEhE0sIvvvj8IYwkCe/KAvJb", 
      "Z5B9YZvV9zaWpYp+buCilcipugLxV8ah8pzc1JFk3Awu4GUPpWCQ2LarAMLiDI6F", 
      "HTqcilhuEBppb/BBfMcFKKZosXbaKqWCj9uoir8H5ZdLXw4Ehq6ilRqTj934/29v", 
      "z/MyfYQaVTbdEeJ16It1tbMFhCxk/WtlvkAmxF1oWlcX0HulRPegMgCsJDwwUddy", 
      "wMLhZioCg5qgOXGUfDw1dA==")

  pass <- ifelse(is.na(db_pass), gpg_decrypt(pw), db_pass)
  
  # Make db connection
  my_db <- src_mysql(host = db_hostname, port = db_port,
                     dbname = "taxonpages_v2", user = db_user, 
                     password = pass)
  
  return (my_db)
}

#'Get media metadata (details) directly from database
#'@param src_db a dplyr database src connection
#'@return a dplyr tbl
#'@export
#'@examples
#'my_db <- connectMediaDB("localhost")
#'media <- getMediaDetailsFromDB(my_db)
getMediaDetailsFromDB <- function(src_db) {
  
  media_query <- "select 
      uuid as media_uuid, 
      media_type,
      copyright_owner,
      comment,
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
  
  media <- tbl(src_db, sql(media_query))
  
  return(media)
}

#'Get media metadata (links) directly from database
#'@param src_db a dplyr database src connection
#'@return a dplyr tbl
#'@export
#'@examples
#'my_db <- connectMediaDB("localhost")
#'link <- getMediaLinksFromDB(my_db)
getMediaLinksFromDB <- function(src_db) {
  
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
    
  link <- tbl(src_db, sql(link_query))
  
  return (link)
}