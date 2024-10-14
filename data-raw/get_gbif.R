#' @title Get GBIF Data
#'
#' @description This script download and standardize the taxonomic
#'   backbone of the [Global Biodiversity Information Facility](https://www.gbif.org/).
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
backbone <- "gbif"
zip <- paste0("backbone", ".zip")
path <- file.path(here::here(), "data-raw", backbone, zip)

## Check if there is any new version available
url0 <- "https://hosted-datasets.gbif.org/datasets/backbone/current/"
last_updated <- .getUpdates(url0, backbone, zip)
last_download <- readLines(file.path(here::here(), "data-raw", backbone, 
                                     "last_update.txt"))

## Obtaining the most up-to-date version, if necessary
if (last_updated != last_download) {
  ## download the latest taxonomic backbone (in browser or using the code below)
  url <- paste0(url0, zip)
  options(timeout = max(900, getOption("timeout")))
  utils::download.file(url = url, destfile = path, mode = "wb")
  
  ## unzipping the data
  all_files <- utils::unzip(path, list = TRUE)$Name
  file <- all_files[grepl("Taxon", all_files)]
  temp <- tempfile()
  data <- data.table::fread(unzip(path, files = file, exdir = temp))
  unlink(temp)
  
  ## date of last modification
  last_update <- last_updated
  # last_update <- .squish(metadata[grepl("dateStamp", metadata)])
  # last_update <- gsub('<dateStamp>|</dateStamp>', "", last_update, perl = TRUE)
  # last_update <- as.character(gsub("T.*", "", last_update))
  
  file1 <- all_files[grepl("eml.xml", all_files)]
  temp <- tempfile()
  metadata <- readLines(unzip(path, files = file1, exdir = temp))
  citation <- .squish(metadata[grepl("citation", metadata)])
  citation <- (gsub('<citation>|</citation>', "", citation, perl = TRUE))
  unlink(temp)
  
  # Editing data --------------------------------------------------
  ## filtering and standardizing important column names
  cols <- c("taxonID", "phylum", "family", "scientificName", 
            "canonicalName", "scientificNameAuthorship", "taxonRank", 
            "nomenclaturalStatus", "taxonomicStatus", 
            "acceptedNameUsageID", "kingdom", "taxonRemarks") 
  data <- as.data.frame(data)[, cols]
  names(data) <- c("id", "phylum", "family", "scientific.name", 
                   "name", "authorship", "taxon.rank", 
                   "name.status", "taxon.status", 
                   "accepted.id", "kingdom", "taxon.remarks")
  
  ## fixing non-ASCII characters encoding to UTF-8 to avoid R CDM Check warnings
  Encoding(data$authorship) <- "UTF-8"
  data$authorship <- iconv(data$authorship, "UTF-8", "UTF-8")
  
  rep_these <- grepl("\u00d7", data$name) 
  if (any(rep_these)) {
    Encoding(data$name[rep_these]) <- "UTF-8"
    data$name[rep_these] <- iconv(data$name[rep_these], "UTF-8", "UTF-8")
  }
  
  rep_these <- grepl("\u00eb", data$name) | grepl("\u00fc", data$name)
  if (any(rep_these)) {
    Encoding(data$name[rep_these]) <- "UTF-8"
    data$name[rep_these] <- iconv(data$name[rep_these], "UTF-8", "UTF-8")
  }
  
  ## obtaining the scientific.name (taxon names + authors)
  data$scientific.name <- .squish(data$scientific.name)
  data$name <- .squish(data$name)
  
  ## Adding the ranks to the the canonical name
  check_these <- data$taxon.rank %in% "variety"
  data$name[check_these] <- 
    plantR:::addRank(data$name[check_these], "var.")

  check_these <- data$taxon.rank %in% "subspecies"
  data$name[check_these] <- 
    plantR:::addRank(data$name[check_these], "subsp.")
  
  check_these <- data$taxon.rank %in% "form"
  data$name[check_these] <- 
    plantR:::addRank(data$name[check_these], "f.")
  
  ## Standardizing taxon ranks
  # table(data$taxon.rank)

  ## Standardizing taxon ranks
  remarks <- data$taxon.remarks
  rep_these <- grepl("Originally found in sources as doubtful taxon within", remarks)
  remarks[rep_these] <- ""
  rep_these <- grepl("Originally found in sources as accepted taxon within", remarks)
  remarks[rep_these] <- ""
  rep_these <- grepl("Originally found in sources as synonym of", remarks)
  remarks[rep_these] <- ""
  rep_these <- grepl("Originally found in sources as heterotypic synonym of", remarks)
  remarks[rep_these] <- "heterotypic synonym"
  rep_these <- grepl("Originally found in sources as homotypic synonym of", remarks)
  remarks[rep_these] <- "homotypic synonym"
  rep_these <- grepl("Originally found in sources as proparte synonym of", remarks)
  remarks[rep_these] <- "proparte synonym"
  rep_these <- grepl("Possible variant of", remarks)
  remarks[rep_these] <- "possible orthographic variant"
  rep_these <- nchar(remarks) > 50
  remarks[rep_these] <- ""
  data$name.status <- remarks
  #tail(sort(table(remarks)), 20)
  
  ## Standardizing taxon status
  data$taxon.status_new <- tolower(data$taxon.status)
  patts <- c("doubtful", "heterotypic synonym", 
             "homotypic synonym", "proparte synonym")
  statuses <- c("unplaced", "synonym", "synonym", "synonym")

  for (i in seq_along(patts)) {
    rep_these <- data$taxon.status_new %in% patts[i]
    if (any(rep_these)) {
      empty <- data$name.status %in% ""
      data$taxon.status_new[rep_these] <- statuses[i]
      if (!patts[i] %in% c("doubtful")) {
        data$name.status[rep_these & empty] <- patts[i]
        #data$name.status[rep_these & !empty] <- patts[i]
      }    
    }
  }
  table(data$name.status, data$taxon.status_new)
  
  ## Final edits
  # rep_these <- data$name.status %in% "possible orthographic variant" &
  #   data$taxon.status %in% "doubtful"
  # data$name.status_new[rep_these] <- ""
  # 
  # check_these <- data$taxon.status_new %in% "accepted" & 
  #   data$name.status %in% ""
  # data$name.status[check_these] <- "valid"
  
  
  # check and replacing if all is good
  # table(data$taxon.status, data$taxon.status_new)
  data$taxon.status <- data$taxon.status_new

  ## obtaining the accepted.name column
  rep_these <- data$accepted.id %in% c("", " ", NA, "NA")
  data1 <- data[rep_these, c("id", "name", "authorship", 
                             "taxon.rank", "taxon.status", "name.status")]
  names(data1)[1] <- "accepted.id" 
  tmp <- dplyr::left_join(data, data1, by = "accepted.id")
  identical(tmp$id, data$id) # should be TRUE
  # rep_these <- !data$accepted.id %in% c("", " ", NA, "NA")
  data$accepted.name <- NA_character_
  data$accepted.authorship <- NA_character_
  data$accepted.taxon.rank <- NA_character_
  data$accepted.taxon.status <- NA_character_
  data$accepted.name.status <- NA_character_
  
  data$accepted.name[!rep_these] <- tmp$name.y[!rep_these]
  data$accepted.authorship[!rep_these] <- tmp$authorship.y[!rep_these]
  data$accepted.taxon.rank[!rep_these] <- tmp$taxon.rank.y[!rep_these]
  data$accepted.taxon.status[!rep_these] <- tmp$taxon.status.y[!rep_these]
  data$accepted.name.status[!rep_these] <- tmp$name.status.y[!rep_these]
  
  ## Organizing fields
  cols1 <- c("id",
             "kingdom",
             "phylum",
             "family", # "genus", "specific.epiteth", "infra.epiteth",
             "name", # genus + epiteth + infra.epiteth
             "authorship", # name author
             "scientific.name", # name + authors
             "taxon.rank", # species, genus, family, order, etc.
             "taxon.status", # accepted or synonym
             "name.status", # correct, ilegitimate, legitimate, but incorrect, orthographical variant, missapplied, not validly published, rejected
             "accepted.id",
             "accepted.name",  #accepted canonical             
             "accepted.authorship",  #accepted authors             
             "accepted.taxon.rank",
             "accepted.taxon.status",
             "accepted.name.status") 
  data <- data[, cols1]
  
  ## Basic standardization of notation
  data$taxon.rank <- tolower(data$taxon.rank)
  data$taxon.status <- tolower(data$taxon.status)
  data$name.status <- tolower(data$name.status)
  data$accepted.taxon.rank <- tolower(data$accepted.taxon.rank)
  data$accepted.taxon.status <- tolower(data$accepted.taxon.status)
  data$accepted.name.status <- tolower(data$accepted.name.status)

  # Saving ------------------------------------------------------------
  reinos <- c("Plantae", "Fungi", "Animalia")
  
  ## Cleaning and re-ordering
  data <- data[!data$name %in% c("", NA, " ", "NA", reinos), ]
  data <- data[!grepl("? ", data$name, fixed = TRUE), ]
  data <- data[!grepl(" ?", data$name, fixed = TRUE), ]
  data <- data[!grepl("\\?$", data$name, perl = TRUE), ]
  data <- data[order(data$taxon.status), ]
  data <- data[!duplicated(paste0(data$kingdom, data$scientific.name)), ]
  data <- data[order(data$id), ]
  
  ## Removing the combined name + authorship column
  data <- data[, -which(names(data) %in% "scientific.name")]

  ## Adding source acronym to the backbone ID
  data$id <- paste0(backbone, "-", data$id)
  rep_these <- !is.na(data$accepted.id)
  if (any(rep_these)) 
    data$accepted.id[rep_these] <- 
    paste0(backbone, "-", data$accepted.id[rep_these])
  
  ## Saving
  data_split <- split(data, data$kingdom)
  data_split <- data_split[names(data_split) %in% reinos]
  for (i in seq_along(reinos)) {
    dimensions <- paste0(dim(data_split[[reinos[i]]])[1], 
                         " rows and ", 
                         dim(data_split[[reinos[i]]])[2], 
                         " columns")
    path_to_save <- file.path(here::here(), "data-raw", backbone, 
                              paste0("df_dim_",reinos[i],".txt"))
    write(dimensions, path_to_save)
    
    if (reinos[i] == "Plantae") {
      gbifNamesPlantae <- data_split[[reinos[i]]]
      usethis::use_data(gbifNamesPlantae, compress = "xz", 
                        overwrite = TRUE)
    }
    
    if (reinos[i] == "Fungi") {
      gbifNamesFungi <- data_split[[reinos[i]]]
      usethis::use_data(gbifNamesFungi, compress = "xz", 
                        overwrite = TRUE)
    }
    
    if (reinos[i] == "Animalia") {
      gbifNamesAnimalia <- data_split[[reinos[i]]]
      usethis::use_data(gbifNamesAnimalia, compress = "xz", 
                        overwrite = TRUE)
    }
  }
  path_to_save <- file.path(here::here(), "data-raw", backbone, 
                            "last_update.txt")
  write(last_update, path_to_save)
  path_to_save <- file.path(here::here(), "data-raw", backbone, 
                            "citation.txt")
  write(citation, path_to_save)
  unlink(path)
}
rm(list = ls())

