#' @importFrom stringr str_dup
#' @import crayon
.onAttach <- function(libname, pkgname) {
  
  # http://www.asciiset.com/figletserver.html (chunky)
  # toilet -f circle dinar
  
  banner <- "ⓓⓘⓝⓐⓡ"
  
  `%+%` <- crayon::`%+%`
  r <- stringr::str_dup
  
  g <- crayon::green $ bgWhite
  b <- crayon::blue $ bgWhite
  s <- crayon::silver $ bgWhite
  
  styled_banner <- 
    g("Welcome to ...") %+% s(r(" ", 9)) %+%
    b(banner) %+%
    s("\nhttps://github.com/DINA-Web/") %+% b("dinar") %+%
    g("\nSee the vignette for a tutorial...")
  
  suppressWarnings(packageStartupMessage(styled_banner))
}
