#library(httr)
#library(jsonlite)
#library(xml2)
#library(dplyr)

#' This function provides DINA-Web API url used
#' @importFrom httr GET
#' @export
#' @examples 
#' DW_API_BASE_URL <- dw_api()
dw_api <- function( 
  base_url = "https://beta-api.dina-web.net/collections", 
  version = "v0", path) {
  res <- paste0(base_url, "/", version, "/")
  # TODO: override if provided from configuration file
  if (!missing(path)) 
    res <- paste0(res, path, "/")
  return (res)
}

#' This function makes an API call to get data
#' @param url The API url where it runs, defaults to the 
#' dw_api function's result
#' @param ua The user agent string to use, defaults to the repo location
#' for this package
#' @importFrom dplyr as_data_frame
#' @importFrom httr user_agent
#' @importFrom httr content
#' @importFrom httr status_code
#' @importFrom httr add_headers
#' @export
dw_get <- function(url = dw_api(), token = dw_pat(),
  ua = user_agent("http://github.com/DINA-Web/dinar")) {
  
  res <- GET(url, ua, add_headers(Authorization = paste("Bearer", token)))
  
  # if (http_type(resp) != "application/json") {
  #   stop("API did not return json", call. = FALSE)
  # }
  
  parsed <- 
    content(res, "text")
  
  if (status_code(res) != 200) {
    stop(
      sprintf(
        "DINA-Web API request failed [%s]\n%s\n<%s>", 
        status_code(res),
        parsed,
        parsed
      ),
      call. = FALSE
    )
  }  
  
  df <- dplyr::as_data_frame(jsonlite::fromJSON(parsed,
    # coerce JSON arrays containing only primitives into an atomic vector    
    simplifyVector = TRUE)) 
    
  response <- structure(list(
      content = df,
      path = url,
      response = res
    ),
    class = "dw_api"
  )
  
  return (response)
}

#' Function to print a response from a call to a DINA-Web API
#' @param x the response provided from dw_get
#' @export
print.dw_api <- function(x, ...) {
  cat("<DINA-Web ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

dw_pat <- function(force = FALSE) {
  env <- Sys.getenv('DW_PAT')
  if (!identical(env, "") && !force) return(env)
  
  if (!interactive()) {
    stop("Please set env var DW_PAT to your DINA-Web personal access token",
         call. = FALSE)
  }
  
  message("Couldn't find env var DW_PAT. See ?dw_pat for more details.")
  message("Please enter your PAT and press enter:")
  pat <- readline(": ")
  
  if (identical(pat, "")) {
    stop("DINA-Web personal access token entry failed", call. = FALSE)
  }
  
  message("Updating DW_PAT env var to PAT")
  Sys.setenv(DW_PAT = pat)
  
  pat
}

#' This function logs in to the API
#' @param user The username 
#' @param pass The password
#' @importFrom httr content
#' @importFrom httr status_code
#' @importFrom httr POST
#' @return TRUE if login succeeds
#' @export

dw_login <- function(user, pass) {
  
  sso <- "https://beta-sso.dina-web.net"

  q <- list(grant_type = "password", 
            client_id = "dina-rest", 
            username = user, 
            password = pass)
  
  res <- POST(modify_url(sso, 
      path = "auth/realms/dina/protocol/openid-connect/token"), 
    body = q, encode = "form")
  
  parsed <- content(res)
  
  if (status_code(res) != 200) {
    Sys.setenv(DW_PAT = "")
    stop(
      sprintf(
        "DINA-Web API login failed [%s]\n%s\n<%s>", 
        status_code(res),
        parsed$error,
        parsed$error_description
      ),
      call. = FALSE
    )
  }  
  
  message("OK, setting personal access token in env")
  pat <- content(res)$access_token
  Sys.setenv(DW_PAT = pat)
  return (TRUE)
}

# dw_auth <- function(pat = dw_pat()) {
#   authenticate(pat, "x-oauth-basic", "basic")
# }

dw_check <- function(req) {
  if (req$status_code < 400) return(invisible())
  
  message <- dw_parse(req)$message
  stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}

dw_parse <- function(req) {
  text <- content(req, as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

dw_pat <- function() {
  Sys.getenv('DW_PAT')
}

has_pat <- function() !identical(dw_pat(), "")


#' Get a data frame from DINA-Web APIs with any resource
#' @param resource_name the table or resource name to use
#' @importFrom httr modify_url
#' @export
dw_get_resource <- function(resource_name, params) {
  
  url <- dw_api(path = resource_name)
  if (!missing(params)) {
    url <- modify_url(dw_api(path = resource_name), query = params)
  }
  
  res <- dw_get(url)
  return (res$content)
}

#' Get a data frame from DINA-Web APIs with institutions
#' @export
dw_get_institutions <- function() {
  dw_get_resource("institution")
}

#' Get a data frame from DINA-Web APIs with agents
#' @export
dw_get_agents <- function() {
  dw_get_resource("agent")
}

#' Get a data frame from DINA-Web APIs with collections
#' @export
dw_get_collections<- function() {
  dw_get_resource("collection")
}

#' Get a data frame from DINA-Web APIs with disciplines
#' @export
dw_get_disciplines <- function() {
  dw_get_resource("discipline")
}

#' Get a data frame from DINA-Web APIs with divisions
#' @export
dw_get_division <- function() {
  dw_get_resource("division")
}


#' This function makes an API call to create data
#' @param url The API url where it runs, defaults to the 
#' dw_api function's result
#' @param json The JSON string represenation of the data
#' @param token The required authentication token provided from dw_login()
#' @param ua The user agent string to use, defaults to the repo location
#' for this package
#' @importFrom dplyr as_data_frame
#' @importFrom httr user_agent
#' @importFrom httr content
#' @importFrom httr status_code
#' @importFrom httr add_headers
#' @importFrom httr content_type_json
#' @importFrom jsonlite validate
#' @export
dw_post <- function(url = dw_api(), json, 
  token = dw_pat(), ua = user_agent("http://github.com/DINA-Web/dinar")) {
  
  if (!validate(json))
    stop("Invalid JSON provided: ", json, call. = FALSE)
  
  res <- POST(url, ua, add_headers(Authorization = paste("Bearer", token)),
    content_type_json(), body = json)
  
  parsed <- 
    content(res, "text")
  
  if (status_code(res) != 200) {
    stop(
      sprintf(
        "DINA-Web API request failed [%s]\n%s\n<%s>", 
        status_code(res),
        parsed,
        parsed
      ),
      call. = FALSE
    )
  }  
  
  response <- structure(list(
    content = parsed,
    path = url,
    response = res
  ),
  class = "dw_api"
  )
  
  return (response)
}
