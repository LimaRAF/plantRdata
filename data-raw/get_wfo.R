#' @title Get World Flora Online Data
#'
#' @description This script download and standardize the taxonomic
#'   backbone of the [World Flora Online](https://www.worldfloraonline.org/).
#'
#' @author Renato A. Ferreira de Lima
#' 
#' @keywords internal
#' 
#' @noRd
#'
#' @family taxonomy
# Downloading data --------------------------------------------------
## Set up path to save the taxonomic backbone
backbone <- "wfo"
zip <- paste0(toupper(backbone), "_Backbone.zip")
path <- file.path(here::here(), "data-raw", backbone, zip)

## download the latest taxonomic backbone (in browser or using the commented code below)
## download takes about 20 min and
url <- "https://files.worldfloraonline.org/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip"
options(timeout = 1600)
utils::download.file(url = url, destfile = path, mode = "wb")

## unzipping the data
file <- head(utils::unzip(path, list = TRUE)$Name, 1)
temp <- tempfile()
data <- data.table::fread(unzip(path, files = file, exdir = temp))
unlink(temp)

## date of last modification
last_update <- max(data$modified)

# Editing data --------------------------------------------------
## filtering and standardizing important column names
cols <- c("taxonID", "family", "scientificName", "scientificNameAuthorship",
          "taxonRank", "nomenclaturalStatus", "taxonomicStatus", 
          "acceptedNameUsageID") 
data <- as.data.frame(data)[, cols]
names(data) <- c("id", "family", "name", "authorship", "taxon.rank",
                 "name.status", "taxon.status", "accepted.id")

## obtaining the scientific.name (taxon names + authors)
data$scientific.name <- paste(data$name, data$authorship)

## obtaining the accepted.name column
data1 <- data[, c("id", "name", "authorship", "taxon.rank")]
names(data1)[1] <- "accepted.id" 
tmp <- dplyr::left_join(data, data1, by = "accepted.id")
identical(tmp$id, data$id) # should be TRUE
rep_these <- !data$accepted.id %in% c("", " ", NA, "NA")
data$accepted.name <- NA_character_
data$accepted.taxon.rank <- NA_character_
data$accepted.name[rep_these] <- paste(tmp$name.x[rep_these],
                                       tmp$authorship.x[rep_these])
data$accepted.taxon.rank[rep_these] <- tmp$taxon.rank.x[rep_these]

## Organizing fields
cols1 <- c("id",
           "family", # "genus", "specific.epiteth", "infra.epiteth",
           "name", # genus + epiteth + infra.epiteth
           "authorship", # name author
           "scientific.name", # name + authors
           "taxon.rank", # species, genus, family, order, etc.
           "taxon.status", # accepted or synonym
           "name.status", # correct, ilegitimate, legitimate, but incorrect, orthographical variant, missapplied, not validly published, rejected
           "accepted.name",  #accepted binomial + authors             
           "accepted.taxon.rank") 
data <- data[, cols1]

## Basic standardization of notation
data$taxon.rank <- tolower(data$taxon.rank)
data$taxon.status <- tolower(data$taxon.status)
data$name.status <- tolower(data$name.status)

# Saving ------------------------------------------------------------
## Cleaning
data <- data[order(data$taxon.status), ]
data <- data[!duplicated(data$scientific.name), ]

## Saving
rds <- paste0(backbone, "_data.rds")
path_to_save <- file.path(here::here(), "data-raw", backbone, rds)
saveRDS(data, path_to_save, compress = "xz")
path_to_save <- file.path(here::here(), "data-raw", backbone, 
                          "last_update.txt")
write(last_update, path_to_save)
unlink(path)
