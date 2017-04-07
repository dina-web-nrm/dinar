<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/DINA-Web/dinar.svg)](https://travis-ci.org/ropensci/rfishbase)

Welcome to `dinar`. This package provides an R interface to the DINA-Web APIs.

Installation
------------

``` r
#install.packages("dinar", 
#  repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"),  type="source")
```

``` r
library(dinar)
```

Getting started
---------------

[DINA-Web](http://dina-project.net) is a Free and Open-Source digital Natural History Collections management system for the Web. It provides REST APIs providing access to collections data and the `dinar` R-package provides an R client or interface to these APIs.

### Getting data

Here is an example of getting "institution" data from the API and printing the response:

``` r
library(dinar)

res <- dw_get(dw_api(path = "institution"))
print(res)
```

    <DINA-Web https://beta-api.dina-web.net/collections/v0/institution/>
    Classes 'tbl_df', 'tbl' and 'data.frame':   1 obs. of  38 variables:
     $ id                         : chr "1"
     $ timestampCreated           : num 1.25e+12
     $ timestampModified          : num 1.3e+12
     $ userGroupScopeId           : int 1
     $ altName                    : chr "Swedish Museum of Natural History"
     $ code                       : chr "NRM"
     $ copyright                  : logi NA
     $ description                : logi NA
     $ disclaimer                 : logi NA
     $ hasBeenAsked               : logi TRUE
     $ iconURI                    : logi NA
     $ institutionId              : int 1
     $ ipr                        : logi NA
     $ isAccessionsGlobal         : logi FALSE
     $ isAnonymous                : logi FALSE
     $ isSecurityOn               : logi TRUE
     $ isServerBased              : logi FALSE
     $ isSingleGeographyTree      : logi FALSE
     $ isSharingLocalities        : logi FALSE
     $ license                    : logi NA
     $ lsidAuthority              : logi NA
     $ name                       : chr "Naturhistoriska riksmuseet"
     $ regNumber                  : chr "1248215418.3"
     $ remarks                    : logi NA
     $ termsOfUse                 : logi NA
     $ uri                        : logi NA
     $ currentManagedRelVersion   : logi NA
     $ currentManagedSchemaVersion: logi NA
     $ isReleaseManagedGlobally   : logi FALSE
     $ minimumPwdLength           : int 0
     $ guid                       : chr "e0af6066-096f-11e3-b717-0050568c2f15"
     $ storageTreeDefID           : chr "1"
     $ addressID                  : chr "1"
     $ modifiedByAgentID          : chr "1"
     $ createdByAgentID           : logi NA
     $ entityId                   : int 1
     $ version                    : int 9
     $ uuid                       : chr "https://www.dina-web.nrm/dina-service/dina/v0/Institution/1"

Here are the collections and an example of printing the names of those.

``` r
collections <- dw_get(dw_api(path = "collection"))
collections$content$collectionName
```

     [1] "NRM Entomology Collection Objects"                 
     [2] "NRM Entomology Collection Inventory"               
     [3] "Swedish Malaise Trap Project (SMTP) Collection Obj"
     [4] "Swedish Malaise Trap Project (SMTP) Observations"  
     [5] "NRM Ornithology"                                   
     [6] "NRM Ichthyology"                                   
     [7] "NRM Mammology"                                     
     [8] "NRM Herpetology"                                   
     [9] "GNM Entomology"                                    
    [10] "NRM Paleozoology"                                  
    [11] "NRM Paleobotany"                                   
    [12] "NRM Mineralogy"                                    
    [13] "NRM Fishes"                                        
    [14] "DNA Extractions"                                   
    [15] "Swedish Malaise Trap Project (SMTP) Species Lists" 
    [16] "Swedish Malaise Trap Project (SMTP) Specimens"     
    [17] "NRM Isotope Geology"                               
    [18] "NRM Nodules"
