#' @title Brazilian Fauna Taxonomic Catalog
#'
#' @description A dataset containing the most relevant taxonomic
#'   information of the animals stored in
#'   the [Taxonomic Catalog of the Brazilian Fauna](http://fauna.jbrj.gov.br/fauna/listaBrasil) 
#'   backbone. It includes all taxonomic levels (i.e. infra-species,
#'   species, genus, family and so on).\cr\cr The original backbone
#'   was slightly edited aiming to standardize the notation across all
#'   backbones in `plantRdata`. Many columns in the original source
#'   that were not essential for the process of taxon name checking
#'   were removed and most of them were renamed. Duplicated scientific
#'   names (i.e. taxon name + author) whose taxon status were not
#'   accepted were also removed to avoid possible problems in the
#'   taxon name matching process.
#'
#' @evalRd .readScript("data-raw/ctfb/last_update.txt", 
#'  "Last update/change of the downloaded backbone (day month year):")
#' @format An object of class \code{data.frame} with 17 columns and
#'   over 187 thousand rows.
#' @evalRd .readScript("data-raw/ctfb/citation.txt", "", "references")
#' @source \url{https://ipt.jbrj.gov.br/jbrj/archive.do?r=catalogo_taxonomico_da_fauna_do_brasil}
#' @usage data(ctfbNames)
#'  
"ctfbNames"