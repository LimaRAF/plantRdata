#' @title Get GBIF Data
#'
#' @description This script download and standardize the taxonomic
#'   backbone of the [World Checklist of Vascular Plants](https://powo.science.kew.org/).
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
backbone <- "gbif"
zip <- paste0(backbone, ".zip")
path <- file.path(here::here(), "data-raw", backbone, zip)

## download the latest taxonomic backbone (in browser or using the commented code below)
url <- "https://hosted-datasets.gbif.org/datasets/backbone/current/backbone.zip"
options(timeout = max(300, getOption("timeout")))
utils::download.file(url = url, destfile = path, mode = "wb")

## unzipping the data
all_files <- utils::unzip(path, list = TRUE)$Name
file <- all_files[grepl("Taxon", all_files)]
temp <- tempfile()
data <- data.table::fread(unzip(path, files = file, exdir = temp))
unlink(temp)

## date of last modification
file1 <- all_files[grepl("eml.xml", all_files)]
temp <- tempfile()
metadata <- readLines(unzip(path, files = file1, exdir = temp))
citation <- plantR:::squish(metadata[grepl("citation", metadata)])
citation <- (gsub('<citation>|</citation>', "", citation, perl = TRUE))

last_update <- plantR:::squish(metadata[grepl("dateStamp", metadata)])
last_update <- gsub('<dateStamp>|</dateStamp>', "", last_update, perl = TRUE)
last_update <- as.character(gsub("T.*", "", last_update))
unlink(temp)


# Editing data --------------------------------------------------
## filtering and standardizing important column names
cols <- c("taxonID", "family", "scientificName","canonicalName", 
          "scientificNameAuthorship", "taxonRank", 
          "nomenclaturalStatus", "taxonomicStatus", 
          "acceptedNameUsageID", "kingdom") 
data <- as.data.frame(data)[, cols]
names(data) <- c("id", "family", "scientific.name", "name", "authorship", 
                 "taxon.rank", "name.status", "taxon.status", 
                 "accepted.id", "kingdom")

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
           "kingdom",
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
data$accepted.taxon.rank <- tolower(data$accepted.taxon.rank)

# Saving ------------------------------------------------------------
reinos <- c("Plantae", "Fungi", "Animalia")

## Cleaning
data <- data[!data$name %in% c("", NA, " ", "NA", reinos), ]
data <- data[!grepl("? ", data$name, fixed = TRUE), ]
data <- data[!grepl(" ?", data$name, fixed = TRUE), ]
data <- data[!grepl("?$", data$name, perl = TRUE), ]
data <- data[order(data$taxon.status), ]
data <- data[!duplicated(paste0(data$kingdom, data$scientific.name)), ]

## Saving
data_split <- split(data, data$kingdom)
data_split <- data_split[names(data_split) %in% reinos]
for (i in seq_along(reinos)) {
  rds <- paste0(backbone, "_", tolower(reinos[i]),"_data.rds")
  path_to_save <- file.path(here::here(), "data-raw", backbone, rds)
  saveRDS(data_split[[reinos[i]]], path_to_save, compress = "xz")
}
path_to_save <- file.path(here::here(), "data-raw", backbone, 
                          "last_update.txt")
write(last_update, path_to_save)
path_to_save <- file.path(here::here(), "data-raw", backbone, 
                          "citation.txt")
write(citation, path_to_save)
unlink(path)

