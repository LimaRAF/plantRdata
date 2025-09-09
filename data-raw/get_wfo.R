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
#' 
#' @date 2024/04/26
# Downloading data --------------------------------------------------
## Set up path to the specific taxonomic backbone
backbone <- "wfo"
zip <- paste0(toupper(backbone), "_Backbone.zip")
path <- file.path(here::here(), "data-raw", backbone, zip)

## Check if there is any new version available
url0 <- "https://files.worldfloraonline.org/files/WFO_Backbone/_WFOCompleteBackbone/"
last_updated <- .getUpdates(url0, backbone, zip)
last_download <- readLines(file.path(here::here(), "data-raw", backbone, 
                                      "last_update.txt"))

## Obtaining the most up-to-date version, if necessary
if (last_updated != last_download) {
  ## download the latest taxonomic backbone (in browser or using the commented code below)
  ## download takes about 20 min and can reach R timeout...
  url <- paste0(url0, zip)
  options(timeout = 1600)
  utils::download.file(url = url, destfile = path, mode = "wb")
  
  ## unzipping the data
  all_files <- utils::unzip(path, list = TRUE)$Name
  file <- all_files[grepl("classification", all_files)]
  temp <- tempfile()
  data <- data.table::fread(unzip(path, files = file, exdir = temp))
  unlink(temp)
  
  ## last update of current version of backbone
  last_update <- last_updated
  
  # Editing data --------------------------------------------------
  ## filtering and standardizing important column names
  cols <- c("taxonID", "majorGroup", "family", "scientificName", 
            "scientificNameAuthorship", "taxonRank", 
            "nomenclaturalStatus", "taxonomicStatus", 
            "acceptedNameUsageID") 
  data <- as.data.frame(data)[, cols]
  names(data) <- c("id", "phylum", "family", "tax.name", "tax.authorship", 
                   "taxon.rank", "name.status", "taxon.status", 
                   "accepted.id")
  
  ## fixing non-ASCII characters encoding to UTF-8 to avoid R CDM Check warnings
  Encoding(data$tax.authorship) <- "UTF-8"
  data$tax.authorship <- iconv(data$tax.authorship, "UTF-8", "UTF-8")
  
  rep_these <- grepl("\u00d7", data$tax.name)
  if (any(rep_these)) {
    Encoding(data$tax.name[rep_these]) <- "UTF-8"
    data$tax.name[rep_these] <- iconv(data$tax.name[rep_these], "UTF-8", "UTF-8")
  }
  
  rep_these <- grepl("\u00eb", data$tax.name)  | grepl("\u00fc", data$tax.name)
  if (any(rep_these)) {
    Encoding(data$tax.name[rep_these]) <- "UTF-8"
    data$tax.name[rep_these] <- iconv(data$tax.name[rep_these], "UTF-8", "UTF-8")
  }

  ## obtaining the scientific.name (taxon names + authors)
  data$scientific.name <-
    .buildName(data, col.names = c("tax.name", "tax.authorship"))
  
  ## obtaining the accepted.name column
  rep_these <- data$accepted.id %in% c("", " ", NA, "NA")
  data1 <- data[rep_these, 
                c("id", "tax.name", "tax.authorship", 
                  "taxon.rank", "taxon.status", "name.status")]
  names(data1)[1] <- "accepted.id" 
  tmp <- dplyr::left_join(data, data1, by = "accepted.id")
  identical(tmp$id, data$id) # should be TRUE
  data$accepted.tax.name <- NA_character_
  data$accepted.tax.authorship <- NA_character_
  data$accepted.taxon.rank <- NA_character_
  data$accepted.taxon.status <- NA_character_
  data$accepted.name.status <- NA_character_
  
  data$accepted.tax.name[!rep_these] <- tmp$tax.name.y[!rep_these]
  data$accepted.tax.authorship[!rep_these] <- tmp$tax.authorship.y[!rep_these]
  data$accepted.taxon.rank[!rep_these] <- tmp$taxon.rank.y[!rep_these]
  data$accepted.taxon.status[!rep_these] <- tmp$taxon.status.y[!rep_these]
  data$accepted.name.status[!rep_these] <- tmp$name.status.y[!rep_these]
  
  ## Any missing accepted names?
  rep_these <- !data$accepted.id %in% c("", " ", NA) & 
                data$accepted.tax.name %in% c("", " ", NA)
  if (any(rep_these)) {
    tmp <- data[rep_these, "accepted.id", drop = FALSE]
    names(tmp)[1] <- "id"
    col2rep <- c("accepted.tax.name", "accepted.tax.authorship", 
                 "accepted.taxon.rank", "accepted.taxon.status", 
                 "accepted.name.status")
    data1 <- data[, c("id", "accepted.id", col2rep)]
    tmp1 <- dplyr::left_join(tmp, data1, by = "id")
    
    check_these <- is.na(tmp1$accepted.tax.name)
    if (any(check_these)) {
      
      check_ids <- tmp1$accepted.id[check_these]
      data1 <- data[match(check_ids, data$accepted.id),]
      data1 <- unique(data1[data1$taxon.status %in% "accepted", ])
      
      check_ids <- tmp1$id[check_these]
      data2 <- data[match(check_ids, data$accepted.id),]
      data2 <- unique(data2[data2$taxon.status %in% "accepted", ])
      data3 <- unique(rbind.data.frame(data1, data2))
      
      col2rep1 <- c("tax.name", "tax.authorship", "taxon.rank", 
                    "taxon.status", "name.status")
      tmp2 <- dplyr::left_join(tmp1[check_these,], 
                               data3[, c("accepted.id", col2rep1)], 
                               by = "accepted.id")
      
      tmp1[check_these, c("accepted.id", col2rep)] <- 
        tmp2[, c("id", col2rep1)] 
    }
    
    data[rep_these, c("accepted.id", col2rep)] <- 
      tmp1[, c("accepted.id", col2rep)] 
  }
  
  ## Organizing fields
  cols1 <- c("id",
             "phylum",
             "family",
             "tax.name", # genus + epiteth + infra.epiteth (canonical)
             "tax.authorship", # name author
             "scientific.name", # name + authors
             "taxon.rank", # species, genus, family, order, etc.
             "taxon.status", # accepted or synonym
             "name.status", # correct, ilegitimate, legitimate, etc
             "accepted.id",
             "accepted.tax.name",  #accepted canonical             
             "accepted.tax.authorship",  #accepted authors             
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
  data$phylum[data$phylum %in% "A"] <- "Magnoliophyta"
  
  rep_these <- data$accepted.id %in% c("", " ", NA)
  data$accepted.id[rep_these] <- NA
  
  # Saving ------------------------------------------------------------
  ## Cleaning and re-ordering
  data <- data[order(data$taxon.status), ]
  data <- data[!duplicated(data$scientific.name), ]
  data <- data[order(data$id), ]
  
  ## Removing the combined name + authorship column
  data <- data[, -which(names(data) %in% "scientific.name")]
  
  ## Replacing empty authors by NAs
  rep_these <- data[["tax.authorship"]] %in% c("", " ")
  if (any(rep_these))
    data[["tax.authorship"]][rep_these] <- NA
  
  rep_these <- data[["accepted.tax.authorship"]] %in% c("", " ")
  if (any(rep_these))
    data[["accepted.tax.authorship"]][rep_these] <- NA
  
  ## How many columns and lines (in April 2024: 1,572,151)
  dimensions <- 
    paste0(dim(data)[1], " rows and ", dim(data)[2], " columns")
  
  ## Saving
  wfoNames <- data
  usethis::use_data(wfoNames, compress = "xz", overwrite=TRUE)
  
  path_to_save <- file.path(here::here(), "data-raw", backbone, 
                            "last_update.txt")
  write(as.character(last_update), path_to_save)
  path_to_save <- file.path(here::here(), "data-raw", backbone, 
                            "df_dim.txt")
  write(dimensions, path_to_save)
  unlink(path)
}
rm(list = ls())
