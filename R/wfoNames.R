#' @title World Flora Online Taxonomy
#'
#' @description A dataset containing the most relevant taxonomic
#'   information of plant names stored in the [World Flora Online](https://www.worldfloraonline.org/), 
#'   taxonomic backbone including bryophytes and vascular plants (i.e.
#'   licophytes, ferns, gimnosperms and angiosperms) and all taxonomic
#'   levels (i.e. infra-species, species, genus, family and so
#'   on).\cr\cr The original backbone was slightly edited aiming to
#'   standardize the notation across backbones provided in
#'   `plantRdata`. Many columns in the original source that were not
#'   essential for the process of taxon name checking were removed and
#'   most of them were renamed. Duplicated scientific names (i.e.
#'   taxon name + author) whose taxon status were not accepted were
#'   also removed to avoid possible problems in the taxon name
#'   matching process.
#'
#' @keywords datasets
#' @name wfoNames
#' @usage data(wfoNames)
#' @source \url{https://files.worldfloraonline.org/files/WFO_Backbone/_WFOCompleteBackbone/}
#' @evalRd .readScript("data-raw/wfo/last_update.txt", 
#'  "Last update/change of the downloaded backbone (year-month-day):")
#' @evalRd .readScript("data-raw/wfo/df_dim.txt", 
#'  "A data frame with:", "format")
#' @references 
#'   Borsch, T., Berendsohn, W., Dalcin, E., et al. (2020). World
#'   Flora Online: Placing taxonomists at the heart of a definitive
#'   and comprehensive global resource on the world’s plants. Taxon,
#'   69(6): 1311–1341. https://doi.org/10.1002/tax.12373
#' 
"wfoNames"
