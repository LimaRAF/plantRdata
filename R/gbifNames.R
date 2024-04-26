#' @title GBIF Plant Taxonomy
#'
#' @description A dataset containing the most relevant taxonomic
#'   information of the kingdom Plantae stored in the 
#'   [Global Biodiversity Information Facility](https://www.gbif.org/),
#'   taxonomic backbone, including all taxonomic levels (i.e.
#'   infra-species, species, genus, family and so on).\cr\cr The
#'   original backbone was slightly edited aiming to standardize the
#'   notation across backbones provided in `plantRdata`. Many columns
#'   in the original source that were not essential for the process of
#'   taxon name checking were removed and most of them were renamed.
#'   Duplicated scientific names (i.e. taxon name + author) whose
#'   taxon status were not accepted were also removed to avoid
#'   possible problems in the taxon name matching process.
#'
#' @keywords datasets
#' @name gbifNamesPlantae
#' @usage data(gbifNamesPlantae)
#' @source \url{https://hosted-datasets.gbif.org/datasets/backbone/current/}
#' @evalRd .readScript("data-raw/gbif/last_update.txt", 
#'  "Last update/change of the downloaded backbone (year-month-day):")
#' @evalRd .readScript("data-raw/gbif/df_dim_Plantae.txt", 
#'  "A data frame with:", "format")
#' @evalRd .readScript("data-raw/gbif/citation.txt", "", "references")
#'  
"gbifNamesPlantae"

#' @title GBIF Fungi Taxonomy
#'
#' @description A dataset containing the most relevant taxonomic
#'   information of the kingdom Fungi stored in the 
#'   [Global Biodiversity Information Facility](https://www.gbif.org/),
#'   taxonomic backbone, including all taxonomic levels (i.e.
#'   infra-species, species, genus, family and so on).\cr\cr The
#'   original backbone was slightly edited aiming to standardize the
#'   notation across backbones provided in `plantRdata`. Many columns
#'   in the original source that were not essential for the process of
#'   taxon name checking were removed and most of them were renamed.
#'   Duplicated scientific names (i.e. taxon name + author) whose
#'   taxon status were not accepted were also removed to avoid
#'   possible problems in the taxon name matching process.
#'
#' @keywords datasets
#' @name gbifNamesFungi
#' @usage data(gbifNamesFungi)
#' @source \url{https://hosted-datasets.gbif.org/datasets/backbone/current/}
#' @evalRd .readScript("data-raw/gbif/last_update.txt", 
#'  "Last update/change of the downloaded backbone (year-month-day):")
#' @evalRd .readScript("data-raw/gbif/df_dim_Fungi.txt", 
#'  "A data frame with:", "format")
#' @evalRd .readScript("data-raw/gbif/citation.txt", "", "references")
#'  
"gbifNamesFungi"

#' @title GBIF Animal Taxonomy
#'
#' @description A dataset containing the most relevant taxonomic
#'   information of the kingdom Animalia stored in the 
#'   [Global Biodiversity Information Facility](https://www.gbif.org/),
#'   taxonomic backbone, including all taxonomic levels (i.e.
#'   infra-species, species, genus, family and so on).\cr\cr The
#'   original backbone was slightly edited aiming to standardize the
#'   notation across backbones provided in `plantRdata`. Many columns
#'   in the original source that were not essential for the process of
#'   taxon name checking were removed and most of them were renamed.
#'   Duplicated scientific names (i.e. taxon name + author) whose
#'   taxon status were not accepted were also removed to avoid
#'   possible problems in the taxon name matching process.
#'
#' @keywords datasets
#' @name gbifNamesAnimalia
#' @usage data(gbifNamesAnimalia)
#' @source \url{https://hosted-datasets.gbif.org/datasets/backbone/current/}
#' @evalRd .readScript("data-raw/gbif/last_update.txt", 
#'  "Last update/change of the downloaded backbone (year-month-day):")
#' @evalRd .readScript("data-raw/gbif/df_dim_Animalia.txt", 
#'  "A data frame with:", "format")
#' @evalRd .readScript("data-raw/gbif/citation.txt", "", "references")
#'  
"gbifNamesAnimalia"
