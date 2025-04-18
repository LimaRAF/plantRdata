#' @title Get WCVPlants Data
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
  citation <- .squish(
    metadata[[1]][min(which(grepl("Govaerts", metadata[[1]])))])
  unlink(temp)
  
  # Editing data --------------------------------------------------
  ## filtering and standardizing important column names
  cols <- c("plant_name_id", "family", "taxon_name", "taxon_authors",
            "taxon_rank", "nomenclatural_remarks", "taxon_status", 
            "accepted_plant_name_id", "powo_id") 

  data <- as.data.frame(data)[, cols]
  names(data) <- c("id", "family", "tax.name", "tax.authorship", "taxon.rank",
                   "name.status", "taxon.status", "accepted.id", 
                   "id.powo")
  
  ## fixing non-ASCII characters encoding to UTF-8 to avoid R CDM Check warnings
  Encoding(data$tax.authorship) <- "UTF-8"
  data$tax.authorship <- iconv(data$tax.authorship, "UTF-8", "UTF-8")
  
  rep_these <- grepl("\u00d7", data$tax.name)
  if (any(rep_these)) {
    Encoding(data$tax.name[rep_these]) <- "UTF-8"
    data$tax.name[rep_these] <- iconv(data$tax.name[rep_these], "UTF-8", "UTF-8")
  }
  
  rep_these <- grepl("\u00eb", data$tax.name) | 
                grepl("\u00fc", data$tax.name) |
                grepl("\u00e8", data$tax.name) |
                grepl("\u00f6", data$tax.name)
  if (any(rep_these)) {
    Encoding(data$tax.name[rep_these]) <- "UTF-8"
    data$tax.name[rep_these] <- iconv(data$tax.name[rep_these], "UTF-8", "UTF-8")
  }

  rep_these <- grepl("\u0430", data$tax.name)
  if (any(rep_these))
    data$tax.name[rep_these] <- gsub("\u0430", "a", data$tax.name[rep_these])

  ## obtaining the scientific.name (taxon names + authors)
  data$scientific.name <- 
    .buildName(data, col.names = c("tax.name", "tax.authorship"))

  ## Editing taxon ranks
  patts <- c("agamosp.", "Convariety", "ecas.", "Form", "Genus", 
             "grex", "group", "lusus", "microf.", "microgène", 
             "micromorphe", "modif.", "monstr.", "mut.", "nid",
             "nothof.", "nothosubsp.", "nothovar.", "positio",
             "proles", "provar.", "psp.", "Species", "stirps", 
             "Subform", "sublusus", "subproles", "Subspecies",
             "subspecioid", "Subvariety", "Variety")
  names(patts) <- c("agamosp.", "convariety", "ecas.", "form", "genus", 
                    "grex", "group", "lusus", "microf.", "microgene", 
                    "micromorphe", "modif.", "monstr.", "mut.", "nid",
                    "nothof.", "nothosubsp.", "nothovar.", "positio",
                    "prole", "provar.", "psp.", "species", "stirps", 
                    "subform", "sublusus", "subprole", "subspecies",
                    "subspecioid", "subvariety", "variety")
  for(i in seq_along(patts)) {
    data$taxon.rank[data$taxon.rank %in% patts[i]] <- names(patts)[i]
  }
  
  ## As in Apr 2025, WCVP has no family or order names to be added to
  ## the backbone
  
  ## Editing of name status
  status <- tolower(.squish(data$name.status))
  status <- gsub(", , ", ", ", status, perl = TRUE)
  status <- gsub(".*\\], ", "", status, perl = TRUE)
  status <- gsub("^, ", "", status, perl = TRUE)
  status <- gsub("\\.\\.", ".", status, perl = TRUE)
  status <- gsub("^\\. ", "", status, perl = TRUE)
  status <- gsub(" type\\.$", " type", status, perl = TRUE)
  status <- gsub(" date\\.$", " date", status, perl = TRUE)
  status <- gsub(" page\\.$", " page", status, perl = TRUE)
  status <- gsub(" synonym\\.$", " synonym", status, perl = TRUE)
  status <- gsub(" indicated\\.$", " indicated", status, perl = TRUE)
  status[grepl("^\\[c", status, ignore.case = TRUE) & 
           grepl("[0-9]\\]$", status)] <- NA_character_
  
  status_conv <- as.data.frame(
    readxl::read_excel("data-raw/name_status_conversion.xlsx"))
  status_conv_bb <- status_conv[status_conv$backbone %in% backbone,
                                c("name.status", "replacement")]
  status_conv_bb <- unique(
    status_conv_bb[!is.na(status_conv_bb$name.status), ])

  DT <- data.table::data.table(oldName = status)
  DT[, ordem := .I]
  repl <- data.table::data.table(oldName = status_conv_bb$name.status, 
                            newName = status_conv_bb$replacement)
  data.table::setkey(DT, oldName)
  data.table::setkey(repl, oldName)
  
  DT[repl, oldName := newName]
  data.table::setkey(DT, ordem)
  status <- DT$oldName

  rep_these <- data$id == data$accepted.id & 
                status %in% c("", " ", NA, "NA") &
                  data$taxon.status %in% "Accepted"
  rep_these[is.na(rep_these)] <- FALSE
  status[rep_these] <- "valid"
  status <- .squish(status)
  #tail(sort(table(status)), 30)
  #head(sort(unique(status)), 50)
  #table(status, data$taxon.status)
  data$name.status_new <- status

  ## Standardizing taxon status
  data$taxon.status_new <- tolower(data$taxon.status)
  patts <- c("artificial hybrid", "illegitimate", "invalid", 
             "local biotype", "misapplied", "orthographic")
  statuses <- c("accepted", "", "", "accepted", "", "synonym")
  accepted_string <- c("", "basionym issues", "correct","valid",
                       "conserved", "name and orthography conserved",
                       "orthographic variant", "orthographic",
                       "orthography conserved",
                       "provisional", "type issues")
  
  for (i in seq_along(patts)) {
    rep_these <- data$taxon.status_new %in% patts[i]
    if (any(rep_these)) {
      empty <- data$name.status_new %in% accepted_string & 
                !is.na(data$name.status_new)
      data$taxon.status_new[rep_these & empty] <- statuses[i]
      data$name.status_new[rep_these & empty] <- patts[i]
      
      if (patts[i] %in% c("orthographic")) {
        data$taxon.status_new[rep_these & !empty] <- statuses[i]
      } else {
        data$taxon.status_new[rep_these & !empty] <- ""
      }
      data$name.status_new[rep_these & !empty] <- patts[i]
    }
  }
  # table(data$name.status_new, data$taxon.status_new)

  ## Final edits
  rep_these <- data$name.status_new %in% "orthographic"
  data$name.status_new[rep_these] <- "orthographic variant"
  
  rep_these <- tolower(data$taxon.status_new) %in% "accepted" &
                data$name.status_new %in% c("illegitimate", "mistaken",
                                            "nudum", "rejected", "subnudum",
                                            "superfluous", "suppressed")
  data$name.status_new[rep_these] <- ""

  rep_these <- data$name.status_new %in% "mistaken" & 
                data$taxon.status %in% "Synonym"
  data$name.status_new[rep_these] <- ""

  check_these <- data$taxon.status_new %in% "accepted" & 
                  data$name.status_new %in% ""
  data$name.status_new[check_these] <- "valid"
  
    
  # check and replacing if all is good
  # table(data$taxon.status, data$taxon.status_new)
  data$taxon.status <- data$taxon.status_new
  data$name.status <- data$name.status_new

  ## obtaining the accepted.name column
  rep_these <- data$id == data$accepted.id
  rep_these[is.na(rep_these)] <- FALSE
  data1 <- data[rep_these, 
                c("id", "tax.name", "tax.authorship", 
                  "taxon.rank", "taxon.status", "name.status")]
  table(data1$name.status, data1$taxon.status)
  names(data1)[1] <- "accepted.id"
  tmp <- dplyr::left_join(data, data1, by = "accepted.id")
  stopifnot(identical(tmp$id, data$id)) # should be TRUE
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
                  data$accepted.tax.name %in% c("", " ", NA) &
                    data$id != data$accepted.id
  if (any(rep_these)) {
    tmp <- data[rep_these, "accepted.id", drop = FALSE]
    names(tmp)[1] <- "id"
    
    col2rep <- c("tax.name", "tax.authorship", "taxon.rank", 
                  "taxon.status", "name.status")
    data1 <- data[, c("id", "accepted.id", col2rep)]
    tmp1 <- dplyr::left_join(tmp, data1, by = "id")
    
    data[rep_these, col2rep] <-  tmp1[, col2rep] 
  }

  ## Organizing fields
  cols1 <- c("id",
             "family", # "genus", "specific.epiteth", "infra.epiteth",
             "tax.name", # genus + epiteth + infra.epiteth
             "tax.authorship", # name author
             "scientific.name", # name + authors
             "taxon.rank", # species, genus, family, order, etc.
             "taxon.status", # accepted or synonym
             "name.status", # correct, ilegitimate, legitimate, but incorrect, orthographical variant, missapplied, not validly published, rejected
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

  ## Obtaining the taxon distribution column
  file <- all_files[grepl("dist", all_files)]
  temp <- tempfile()
  dist <- data.table::fread(unzip(path, files = file, exdir = temp))
  unlink(temp)
  
  dist$area <- gsub(" I\\.$", " island", dist$area)
  dist$area <- gsub(" Is\\.$", " Islands", dist$area)
  dist$area <- plantR::prepLoc(dist$area)
  
  # Download botanical countries (level3)
  url1 <- "https://github.com/tdwg/wgsrpd/raw/master/109-488-1-ED/2nd%20Edition/tblLevel3.txt"
  path1 <- gsub("\\.zip", "_dist.txt", path)
  utils::download.file(url = url1, destfile = path1, mode = "wb")
  level3 <- read.table(path1, sep = "*", fileEncoding = "Latin1", 
                     header = TRUE, stringsAsFactors = FALSE,
                     quote = "", fill = TRUE)
  
  # Download subdivision of botanical countries (level4)
  url2 <- "https://github.com/tdwg/wgsrpd/raw/master/109-488-1-ED/2nd%20Edition/tblLevel4.txt"
  path2 <- gsub("\\.zip", "_dist1.txt", path)
  utils::download.file(url = url2, destfile = path2, mode = "wb")
  level4 <- read.table(path2, sep = "*", fileEncoding = "Latin1", 
                       header = TRUE, stringsAsFactors = FALSE,
                       quote = "", fill = TRUE)

  # Merge both informations
  level_all <- dplyr::left_join(level4, level3[, c("L3.code", "L3.area")],
                                by = c("L3.code"))
  level_all1 <- aggregate(L4.code ~ L3.area + L4.country,
                          FUN = function(x) paste(unique(x), collapse = "|"),
                          data = level_all)
  names(level_all1) <- c("taxon.distribution.bc", "taxon.distribution.bru", "taxon.distribution.bru.code")
  
  # Kirgizstan case
  level_all1$taxon.distribution.bc <- 
    gsub("Kirgizistan", "Kirgizstan", level_all1$taxon.distribution.bc, fixed = FALSE)
  level_all1$taxon.distribution.bru <- 
    gsub("Kirgizistan", "Kirgizstan", level_all1$taxon.distribution.bru, fixed = FALSE)
  
  # Gambia case
  level_all1$taxon.distribution.bc <- 
    gsub("Gambia, The", "Gambia", level_all1$taxon.distribution.bc, fixed = FALSE)
  level_all1$taxon.distribution.bru <- 
    gsub("Gambia, The", "Gambia", level_all1$taxon.distribution.bru, fixed = FALSE)
  
  # Substitute 'i' by 'island' and 'is' by 'islands' to match plantR
  level_all1$taxon.distribution.bc <- 
    gsub(" I\\.$", " island", level_all1$taxon.distribution.bc, perl = TRUE)
  level_all1$taxon.distribution.bc <- 
    gsub(" Is\\.$", " islands", level_all1$taxon.distribution.bc, perl = TRUE)
  level_all1$taxon.distribution.bru <- 
    gsub(" I\\.$", " island", level_all1$taxon.distribution.bru, perl = TRUE)
  level_all1$taxon.distribution.bru <- 
    gsub(" Is\\.$", " islands", level_all1$taxon.distribution.bru, perl = TRUE)
  
  level_all1$taxon.distribution.bc <- 
    plantR::prepLoc(level_all1$taxon.distribution.bc)
  level_all1$taxon.distribution.bru <- plantR::prepLoc(level_all1$taxon.distribution.bru)
  
  
  # Edit the column to match wvcvp names exactly, that is, nchar max = 20
  level_all1$taxon.distribution.bc <- 
    substr(level_all1$taxon.distribution.bc, 1, 20)
  
  dist1 <- dplyr::left_join(dist, level_all1, 
                            by = c("area" = "taxon.distribution.bc"))

  dist2 <- aggregate(taxon.distribution.bru.code ~ plant_name_id,
                               FUN = function(x) paste(sort(unique(x)), collapse = "|"),
                               data = dist1)
  names(dist2) <- c("id", "taxon.distribution")
  
  tmp1 <- dplyr::left_join(data, dist2, by = "id")
  
  names(dist2) <- c("accepted.id", "taxon.distribution")
  tmp2 <- dplyr::left_join(data, dist2, by = "accepted.id")
  
  stopifnot(identical(tmp1$id, tmp2$id))
  
  rep_these <- !is.na(tmp2$taxon.distribution)
  tmp1$taxon.distribution[rep_these] <- 
    tmp2$taxon.distribution[rep_these] 
  
  stopifnot(identical(tmp1$id, data$id))
  
  data$taxon.distribution <- tmp1$taxon.distribution
  
  
  # Saving ------------------------------------------------------------
  ## Cleaning and re-ordering
  data <- data[order(data$taxon.status), ]
  data <- data[!duplicated(data$scientific.name), ]
  data <- data[order(data$id), ]
  
  ## Removing the combined name + authorship column
  data <- data[, -which(names(data) %in% "scientific.name")]

  ## Adding source acronym to the backbone ID
  data$id <- paste0(backbone, "-", data$id)
  rep_these <- !is.na(data$accepted.id)
  if (any(rep_these)) 
    data$accepted.id[rep_these] <- 
    paste0(backbone, "-", data$accepted.id[rep_these])
  
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
  write(citation, file.path(path_folder, "citation.txt"))
  unlink(path)
  unlink(path1)
  unlink(path2)
}  
rm(list = ls())
