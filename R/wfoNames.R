#' @title World Flora Online Taxonomy
#'
#' @description A dataset containing the most relevant taxonomic
#'   information of plant names stored in the [World Flora Online](https://www.worldfloraonline.org/), 
#'   taxonomic backbone including bryophytes and vascular plants (i.e.
#'   licophytes, ferns, gymnosperms and angiosperms) and all taxonomic
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
#' @source \url{https://files.worldfloraonline.org/files/WFO_Backbone/_WFOCompleteBackbone/}
#' @evalRd .readScript("data-raw/wfo/last_update.txt", 
#'  "Last update of the downloaded backbone (month/day/year):")
#' @format An object of class \code{data.frame} with 13 columns and
#'   over 1.5 million rows.
#' @references 
#'   Borsch, T., Berendsohn, W., Dalcin, E., et al. (2020). World
#'   Flora Online: Placing taxonomists at the heart of a definitive
#'   and comprehensive global resource on the world's plants. Taxon
#'   69: 1311-1341. https://doi.org/10.1002/tax.12373
#' @usage data(wfoNames)
#' 
"wfoNames"
