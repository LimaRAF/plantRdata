#' @title Brazilian Vascular Plant Taxonomy
#'
#' @description A dataset containing the most relevant taxonomic
#'   information of the vascular plants (i.e. Tracheophyta) stored in
#'   the [Brazilian Flora 2020](https://floradobrasil.jbrj.gov.br/consulta/) 
#'   taxonomic backbone, a.k.a. the Flora and Funga of Brazil. It
#'   includes all taxonomic levels (i.e. infra-species, species,
#'   genus, family and so on).\cr\cr The original backbone was
#'   slightly edited aiming to standardize the notation across all
#'   backbones in `plantRdata`. Many columns in the original source
#'   that were not essential for the process of taxon name checking
#'   were removed and most of them were renamed. Duplicated scientific
#'   names (i.e. taxon name + author) whose taxon status were not
#'   accepted were also removed to avoid possible problems in the
#'   taxon name matching process.
#'
#' @evalRd .readScript("data-raw/bfo/last_update.txt", 
#'  "Last update/change of the downloaded backbone (day month year):")
#' @format An object of class \code{data.frame} with 14 columns and
#'   over 125 thousand rows.
#' @evalRd .readScript("data-raw/bfo/citation.txt", "", "references")
#' @source \url{https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil}
#' @usage data(bfoNamesTracheophyta)
#'  
"bfoNamesTracheophyta"

#' @title Brazilian Fungi Taxonomy
#'
#' @description A dataset containing the most relevant taxonomic
#'   information of the kingdom Fungi stored in the 
#'   [Brazilian Flora 2020](https://floradobrasil.jbrj.gov.br/consulta/)
#'   taxonomic backbone, a.k.a. the Flora and Funga of Brazil. It
#'   includes all taxonomic levels (i.e. infra-species, species,
#'   genus, family and so on).\cr\cr The original backbone was
#'   slightly edited aiming to standardize the notation across all
#'   backbones in `plantRdata`. Many columns in the original
#'   source that were not essential for the process of taxon name
#'   checking were removed and most of them were renamed. Duplicated
#'   scientific names (i.e. taxon name + author) whose taxon status
#'   were not accepted were also removed to avoid possible problems in
#'   the taxon name matching process.
#'
#' @evalRd .readScript("data-raw/bfo/last_update.txt", 
#'  "Last update/change of the downloaded backbone (day month year):")
#' @format An object of class \code{data.frame} with 14 columns and
#'   almost 12 thousand rows.
#' @evalRd .readScript("data-raw/bfo/citation.txt", "", "references")
#' @source \url{https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil}
#' @usage data(bfoNamesFungi)
#'  
"bfoNamesFungi"

#' @title Brazilian Algae Taxonomy
#'
#' @description A dataset containing the most relevant taxonomic
#'   information of Algae stored in
#'   the [Brazilian Flora 2020](https://floradobrasil.jbrj.gov.br/consulta/) 
#'   taxonomic backbone, a.k.a. the Flora and Funga of Brazil. It
#'   includes all taxonomic levels (i.e. infra-species, species,
#'   genus, family and so on).\cr\cr The original backbone was
#'   slightly edited aiming to standardize the notation across all
#'   backbones in `plantRdata`. Many columns in the original source
#'   that were not essential for the process of taxon name checking
#'   were removed and most of them were renamed. Duplicated scientific
#'   names (i.e. taxon name + author) whose taxon status were not
#'   accepted were also removed to avoid possible problems in the
#'   taxon name matching process.
#'
#' @evalRd .readScript("data-raw/bfo/last_update.txt", 
#'  "Last update/change of the downloaded backbone (day month year):")
#' @format An object of class \code{data.frame} with 14 columns and
#'   about one thousand rows.
#' @evalRd .readScript("data-raw/bfo/citation.txt", "", "references")
#' @source \url{https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil}
#' @usage data(bfoNamesAlgae)
#' 
"bfoNamesAlgae"

#' @title Brazilian Bryophyte Taxonomy
#'
#' @description A dataset containing the most relevant taxonomic
#'   information of Bryophyta stored in
#'   the [Brazilian Flora 2020](https://floradobrasil.jbrj.gov.br/consulta/) 
#'   taxonomic backbone, a.k.a. the Flora and Funga of Brazil. It
#'   includes all taxonomic levels (i.e. infra-species, species,
#'   genus, family and so on).\cr\cr The original backbone was
#'   slightly edited aiming to standardize the notation across all
#'   backbones in `plantRdata`. Many columns in the original source
#'   that were not essential for the process of taxon name checking
#'   were removed and most of them were renamed. Duplicated scientific
#'   names (i.e. taxon name + author) whose taxon status were not
#'   accepted were also removed to avoid possible problems in the
#'   taxon name matching process.
#'
#' @evalRd .readScript("data-raw/bfo/last_update.txt", 
#'  "Last update/change of the downloaded backbone (day month year):")
#' @format An object of class \code{data.frame} with 14 columns and
#'   about 5 thousand rows.
#' @evalRd .readScript("data-raw/bfo/citation.txt", "", "references")
#' @source \url{https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil}
#' @usage data(bfoNamesBryophyta)
#' 
"bfoNamesBryophyta"