"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

sort_names <- function(x) x[order(names(x))]

nonce <- function(length = 10) {
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE),
        collapse = "")
}

#' Parse RScript command line options in C-style from inline 
#' text specification
#'@param args is a inline text representation of 5 comma
#'separated values, where the 1st column is the long argument
#'name, the 2nd col is the short argument name, the 3rd col
#'is an enum flag (0 = no arg, 1 = required, 2 = optional), the
#'4th is type (logical, integer, double, complex, character), 
#'the 5th is a short description
#'@return an object with the arguments, if available, or ""
#'@export
#'@examples
#'opt <- get_args("search, s, 2, character, search term to use")
get_args <- function(args) {

  header <- "long, short, enum, type, description, default"
  help <- "help, h, 0, logical, usage description"
  specification <- paste(sep = "\n", header, help, args)
  s <- gsub(", ", ",", specification)
  df <- read.csv2(textConnection(s), sep = ",", quote = "")
  m <- as.matrix.data.frame(df[ ,1:5])
  opt <- getopt(m)
  # if help was asked for print a friendly message
  # and exit with a non-zero error code
  
  if (!is.null(opt$help)) {
    ncat(getopt(m, usage = TRUE))
    q(status = 1)
  }
  return(opt)
}

#' Parse text config file, with Name = Value pairs, optionally grouped in [section]s
#'@param config_file is a character string with a path to the config file
#'@return a data frame with the settings
#'@export
#'@examples
#'cfgfile <- "media-migrate-example.cfg"
#'writeLines("[MediaServer]\nURL=https://localhost/media\nlevel=0", cfgfile)
#'cfg <- get_config(cfgfile, "[MediaServer]")
#'unlink(cfgfile)
get_config <- function(config_file, section) {

  if (any(missing(config_file), !file.exists(config_file)))
    stop("No config file given or config file not found")
  
  config <- readLines(config_file)
  eoc <- length(config)
  beg <- 1
  end <- eoc
  
  if (!missing(section) && grepl(section, config)) {
    beg <- grep(section, config, fixed = TRUE) + 1
    nxt <- grep("[.*?]", config[beg:eoc])
    nxt <- ifelse(length(nxt) > 0, nxt[1], eoc)
    end <- ifelse((nxt - 1 > beg && nxt < eoc), nxt - 1, eoc)
  } else {
    message("No config file section or section not found in file")
  }
  
  cfg <- grep("#", config[beg:end], value = TRUE, invert = TRUE)
  cfg <- grep("=", cfg, value = TRUE)
  pairs <- strsplit(cfg, "=")
  namez <- trimws(sapply(pairs, "[[", 1))
  valuez <- trimws(sapply(pairs, "[[", 2))
  df <- as.data.frame(rbind(valuez), stringsAsFactors = FALSE)
  names(df) <- namez
  return (df)
}

#'Import a package silently, for use from RScript
#'@param packages a vector of the required packages
#'@export
#'@examples
#'import(c("plyr", "stringr"))
#'# Check status of imported required packages
#'imported <- import(c("plyr", "stringr", "RSQLite"))
#'print(imported)
import <- function(packages) {
  # Installs missing R package(s), if required from SUNET repo    
  kSUNET <- "http://ftp.sunet.se/pub/lang/CRAN/"  
  
  getPackage <- function(package) {
    available <- suppressPackageStartupMessages(
      suppressMessages(suppressWarnings(
        require(package, character.only = TRUE, 
                quietly = TRUE, warn.conflicts = FALSE))))
    if (!available) {
      # the package is not available, so try to install and load it
      res <- tryCatch({ 
        install.packages(package, repos = kSUNET)
        loaded <- suppressPackageStartupMessages(
          suppressMessages(suppressWarnings(
            require(package, character.only = TRUE, 
                    quietly = TRUE, warn.conflicts = FALSE))))
      }, error = function(e) {
          loaded <- FALSE
      })
    } else loaded <- TRUE
    return(loaded)
  }  
  res <- sapply(packages, getPackage)  
}

#' Load a persisted .rda file (R object on disk) into memory
#'@param x variable to load the object into
#'@param file the filename where the object is persisted
#'@export
loadx <- function(x, file) {  
  x <- load(file)
  get(x)
}

#'Adds a newline after cat
#'@param x object to send to cat
#'@export
#'@examples
#'catn("text to be printed with a newline at the end")
catn <- function (x, ...) {
  cat(x, ..., "\n")
}

#'Adds a newline before cat
#'@export
#'@examples
#'ncat("text to be printed with a newline at the start")
ncat <- function (x, ...) cat("\n", x, ...)
  
#'Adds a newline before and after cat
#'@export
#'@examples
#'ncatn("text padded with one newline above and below")
ncatn <- function (x, ...) cat("\n", x, ..., "\n\n")

#'Generates base64 encoded encrypted secret 
#'given plaintext string
#'@param plaintext string with the secret
#'@param keyfile location of .pem keyfile
#'@return cyphertext base64 encoded
#'@export
#'@examples
#'secret <- gpg_encrypt("password")
#'print(secret)
gpg_encrypt <- function(plaintext, 
                        keyfile = "~/.ssh/build_pub.pem") {
  
  if (Sys.which("openssl") == "") 
    stop ("System doesn't have openssl installed.")
  
  stopifnot(file.exists(keyfile))
  
  enc <- paste0("echo ", shQuote(plaintext), 
                " | openssl pkeyutl -encrypt ",
                "-pubin -inkey ", keyfile, 
                " | openssl enc -base64")
  
  b64 <- system(intern = TRUE, enc)  
  return(b64)
}

#' Generate inline R code which can be included in R scripts
#' given a variable name and a base64 encoded encrypted secret
#'@param variablename string with variable name
#'@param b64 string with encrypted secret
#'@return R code snippet with a variable containing the 
#'encrypted plaintext
#'@export
#'@examples
#'gpg_inline("pwd", gpg_encrypt("password"))
gpg_inline <- function(variablename, b64) {
  
  lines <- paste0(b64, collapse = "\", \n\t\"")
  code <- paste0(variablename, " <- \n\tc(\"", lines, "\")\n", 
                 collapse = "")
  cat(code)
}

#' Decrypt a base64 encoded encrypted secret
#'@param cypher base64-encoded string with encrypted secret
#'@param keyfile file location pointing to a valid private .pem key
#'@return the decrypted plaintextt
#'@export
#'@examples
#'gpg_decrypt(gpg_encrypt("password"))
gpg_decrypt <- function(cypher, keyfile = "~/.ssh/build.pem") {

  if (Sys.which("openssl") == "") 
    stop ("System doesn't have openssl installed.")
  
  stopifnot(file.exists(keyfile))

  b64 <- paste0(cypher, collapse = "\n")
  dec <- paste0("echo ", shQuote(b64), " | openssl enc -base64 -d | ",
                "openssl pkeyutl -decrypt -inkey ",
                keyfile)
  txt <- system(intern = TRUE, dec)  
  return(txt)
}


#' Transfer files with scp to naturforskaren test server
#'@param files a vector of filenames to transfer
#'@param keyfile location of .pem keyfile
#'@param user username to use at remote server
#'@param server hostname or ip of remote server
#'@param dest destination directory at remote server
#'@return character vector with console output of scp command
#'@export
ul2cloud <- function(files, keyfile = "~/.ssh/build.pem", 
                     user = "ubuntu", server = "test.naturforskaren.se", 
                     dest = "/home/ubuntu") {

  if (Sys.which("scp") == "") 
    stop("System doesn't have scp installed.")
  
  cmd <- paste0("scp -i ", keyfile, " -r ", files, " ", 
                      user, "@", server, ":", dest)
  
  system(cmd, intern = TRUE)
  
}


#' Ping a web site
#'@param url
#'@return TRUE if web site is up, FALSE otherwise
#'@export
#'@examples
#'http_ping("http://www.nrm.se")
http_ping <- function(url, timeout = 1) {
  req <- withTimeout({
    GET(url)
  }, timeout = timeout, onTimeout = "warning")
  
  if (is.null(req)) return (FALSE)
  return(req$status_code == 200)
}
