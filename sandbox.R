if (!require("pacman")) install.packages("pacman")
library("pacman")

# load a few packages

p_load(
    "RCurl"
  , "jsonlite"
  , "SPARQL"
  , "stringr"
  , "tidyverse"
  , "bibtex"
  , "rorcid")


#' exports orcid as bibtex
#' example:
#' writeLines(orcid2bib(orcid_id("0000-0002-7528-738X")), "eisen_mb.bib")
orcid2bib <- function(orcid) {
  bib <- works(orcid)
  bib$data$`work-citation.citation` %>%
    na.omit()
}


#' finds wikidata entity of a given DOI
#' comment:
#' assuming wikidata DOIs are all upper case
#' example:
#' doc2entity("10.1002/hep.28769")
doi2entity <- function(doi,
  endpoint = "https://query.wikidata.org/bigdata/namespace/wdq/sparql") {
  doi <- str_to_upper(doi)
  query <- str_c("SELECT ?item WHERE{?item wdt:P356 ?\"", doi, "\"}")
  qd <- SPARQL(endpoint, query)
  ifelse(ncol(qd$results) < 2,
         NA_character_,
         str_extract(qd$results[1, 2], "Q[0-9]+"))
}



#' extract the json of a wikidata code or entity
#' example:
#' wdjson <- entity2json("Q3308179")
entity2json <- function(code, call_handle = handle) {
  str_c("https://www.wikidata.org/w/api.php?action=wbgetentities&ids=",
        code, "&format=json") %>%
    httpGET(curl = call_handle) %>%
    fromJSON()
}

