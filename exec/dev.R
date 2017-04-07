install_github("devtools")
require("devtools")

options(devtools.name = "Markus Skyttner")
options(devtools.author = '"Markus Skyttner <markus.skyttner@nrm.se> [aut,cre]"', 
        devtools.license = "GPL-3")
setwd("~/biotools")
wd()
create_description("~/biotools")
create("~/biotools")

build()
load_all()
document()

library("httr")
library("opencpu")
opencpu$stop()
opencpu$start(12345);
uri <- paste0(opencpu$url(), "/library/biotools/R/motd/json")
res <- POST(uri)
motd <- unlist(content(res, encoding = "utf-8"))
cat(motd)
cat(res$url)
#opencpu$view('/test')
opencpu$stop()
