#' @title Get World Checklist of Vascular Plants Data
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
#' 
#' @date 2024/04/26
# Downloading data --------------------------------------------------
## Set up path to save the taxonomic backbone
backbone <- "wcvp"
zip <- paste0(backbone, ".zip")
path <- file.path(here::here(), "data-raw", backbone, zip)

## Check if there is any new version available
url0 <- "https://sftp.kew.org/pub/data-repositories/WCVP/"
last_updated <- .getUpdates(url0, backbone, zip)
last_download <- readLines(file.path(here::here(), "data-raw", backbone, 
                                     "last_update.txt"))

## Obtaining the most up-to-date version, if necessary
if (last_updated != last_download) {
  ## download the latest taxonomic backbone (in browser or using the code below)
  url <- paste0(url0, zip)
  options(timeout = max(300, getOption("timeout")))
  utils::download.file(url = url, destfile = path, mode = "wb")
  
  ## unzipping the data
  all_files <- utils::unzip(path, list = TRUE)$Name
  file <- all_files[grepl("names", all_files)]
  temp <- tempfile()
  data <- data.table::fread(unzip(path, files = file, exdir = temp))
  unlink(temp)
  
  ## last update of current version of backbone
  last_update <- last_updated
  
  ## database version
  file1 <- all_files[grepl("README", all_files)]
  temp <- tempfile()
  metadata <- readxl::read_xlsx(unzip(path, files = file1, exdir = temp))
  version <- metadata[[1]][grepl("ersion", metadata[[1]])]
  version <- tolower(.squish(gsub('\"', "", version, perl = TRUE)))
  unlink(temp)
  
  # Editing data --------------------------------------------------
  ## filtering and standardizing important column names
  cols <- c("plant_name_id", "family", "taxon_name", "taxon_authors",
            "taxon_rank", "nomenclatural_remarks", "taxon_status", 
            "accepted_plant_name_id", "powo_id") 

  data <- as.data.frame(data)[, cols]
  names(data) <- c("id", "family", "name", "authorship", "taxon.rank",
                   "name.status", "taxon.status", "accepted.id", "id.powo")
  
  ## obtaining the scientific.name (taxon names + authors)
  data$scientific.name <- 
    .buildName(data, col.names = c("name", "authorship"))
  
  ## obtaining the accepted.name column
  rep_these <- data$id == data$accepted.id
  rep_these[is.na(rep_these)] <- FALSE
  data1 <- data[rep_these, 
                c("id", "name", "authorship", 
                  "taxon.rank", "name.status")]
  names(data1)[1] <- "accepted.id"
  tmp <- dplyr::left_join(data, data1, by = "accepted.id")
  identical(tmp$id, data$id) # should be TRUE
  data$accepted.name <- NA_character_
  data$accepted.authorship <- NA_character_
  data$accepted.taxon.rank <- NA_character_
  data$accepted.name.status <- NA_character_
  data$accepted.name[!rep_these] <- tmp$name.y[!rep_these]
  data$accepted.authorship[!rep_these] <- tmp$authorship.y[!rep_these]
  data$accepted.taxon.rank[!rep_these] <- tmp$taxon.rank.y[!rep_these]
  data$accepted.name.status[!rep_these] <- tmp$name.status.y[!rep_these]
  
  ## Organizing fields
  cols1 <- c("id",
             "family", # "genus", "specific.epiteth", "infra.epiteth",
             "name", # genus + epiteth + infra.epiteth
             "authorship", # name author
             "scientific.name", # name + authors
             "taxon.rank", # species, genus, family, order, etc.
             "taxon.status", # accepted or synonym
             "name.status", # correct, ilegitimate, legitimate, but incorrect, orthographical variant, missapplied, not validly published, rejected
             "accepted.name",  #accepted canonical             
             "accepted.authorship",  #accepted authors             
             "accepted.taxon.rank",
             "accepted.name.status") 
  data <- data[, cols1]
  
  ## Basic standardization of notation
  data$taxon.rank <- tolower(data$taxon.rank)
  data$taxon.status <- tolower(data$taxon.status)
  data$name.status <- tolower(data$name.status)
  data$accepted.taxon.rank <- tolower(data$accepted.taxon.rank)
  data$accepted.name.status <- tolower(data$accepted.name.status)
  
  # further editing of name status
  status <- .squish(data$name.status)
  status <- gsub(", , ", ", ", status, perl = TRUE)
  status <- gsub(".*\\], ", "", status, perl = TRUE)
  status <- gsub("^, ", "", status, perl = TRUE)
  status <- gsub("\\.\\.", ".", status, perl = TRUE)
  status <- gsub(" type\\.$", " type", status, perl = TRUE)
  status <- gsub(" date\\.$", " date", status, perl = TRUE)
  status <- gsub(" page\\.$", " page", status, perl = TRUE)
  status <- gsub(" synonym\\.$", " synonym", status, perl = TRUE)
  status <- gsub(" indicated\\.$", " indicated", status, perl = TRUE)
  status[grepl("^\\[c", status) & 
           grepl("[0-9]\\]$", status)] <- ""
  
  status[status %in% c("nom. ille.", "nom. illeg.", "nom. illeg", 
                       "nom. illegit.", "nom. illeg,")] <- 
    "illegitimate"
  status[status %in% c("not validly publ.", "species not validly publ.", 
                       "not valifly publ.", "species name not validly publ.", 
                       "species name not validly published.", 
                       "species not validly published.",
                       "not validly published", "not validy publ.")] <- 
    "not validly published"
  status[grepl("orth. var.", status)] <- 
    "orthographical variant"
  status[grepl("^nom. rej", status) & !grepl("accept", status)] <- 
    "rejected"
  rep_these <- data$scientific.name == data$accepted.name & 
    status %in% c("", " ", NA, "NA")
  status[rep_these] <- "correct"
  status <- .squish(status)
  # tail(sort(table(status)), 30)
  # head(sort(unique(status)), 50)
  data$name.status <- status
  
  # Saving ------------------------------------------------------------
  ## Cleaning and re-ordering
  data <- data[order(data$taxon.status), ]
  data <- data[!duplicated(data$scientific.name), ]
  data <- data[order(data$id), ]
  
  ## Removing the combined name + authorship column
  data <- data[, -which(names(data) %in% "scientific.name")]

  ## Adding source acronym to the backbone ID
  data$id <- paste0(backbone, "-", data$id)
  
  ## How many columns and lines (in April 2024: 1,421,040; May 2024: 1,429,871)
  dimensions <- paste0(dim(data)[1], " rows and ", dim(data)[2], " columns")
  
  ## Saving
  # .storeData(data, source= backbone, name= paste0(backbone, "Names"))
  wcvpNames <- data
  usethis::use_data(wcvpNames, compress = "xz", overwrite=TRUE)
  
  data_folder <- "data-raw" # c("inst", "extdata")
  path_folder <- file.path(here::here(), 
                           paste0(data_folder, collapse = .Platform$file.sep),
                           backbone)
  write(last_update, file.path(path_folder, "last_update.txt"))
  write(version, file.path(path_folder, "version.txt"))
  write(dimensions, file.path(path_folder, "df_dim.txt"))
  unlink(path)
}  
rm(list = ls())
