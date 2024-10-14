#' @title Get Brazilian Flora Online Data
#'
#' @description This script download and standardize the taxonomic
#'   backbone of the [Brazilian Flora 2020](https://floradobrasil.jbrj.gov.br/consulta/)
#'   project, a.k.a. the Flora and Funga of Brazil.
#'
#' @author Renato A. Ferreira de Lima
#' 
#' @keywords internal
#' 
#' @noRd
#'
#' @family taxonomy
#' 
#' @date 2024/06/01
# Downloading data --------------------------------------------------
## Set up path to save the taxonomic backbone
backbone <- "bfo"
zip <- paste0(backbone, ".zip")
path <- file.path(here::here(), "data-raw", backbone, zip)

## Check if there is any new version available
url0 <- "https://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
last_updated <- .getUpdates(url0, backbone, zip)
last_download <- readLines(file.path(here::here(), "data-raw", backbone, 
                                     "last_update.txt"))

## Obtaining the most up-to-date version, if necessary
if (last_updated != last_download) {
  ## download the latest taxonomic backbone (in browser or using the code below)
  url <- url0
  options(timeout = max(300, getOption("timeout")))
  utils::download.file(url = url, destfile = path, mode = "wb")
  
  ## unzipping the data
  all_files <- utils::unzip(path, list = TRUE)$Name
  file <- all_files[grepl("taxon", all_files)]
  temp <- tempfile()
  data <- data.table::fread(unzip(path, files = file, exdir = temp))
  unlink(temp)
  
  ## last update of current version of backbone
  last_update <- last_updated
  
  ## database version
  file1 <- all_files[grepl("eml", all_files)]
  temp <- tempfile()
  metadata <- unzip(path, files = file1, exdir = temp)
  metadata <- xml2::as_list(xml2::read_xml(metadata))
  version <- attributes(metadata$eml)$packageId
  version <- gsub('.*/', "", version, perl = TRUE)
  citation <- .squish(metadata$eml$additionalMetadata$metadata$gbif$citation[[1]][1])
  unlink(temp)
  
  ## Creating the canonical name and adding the ranks
  data <- as.data.frame(data)
  data$taxon_name <- .buildName(data, c("genus", "specificEpithet", 
                                        "infraspecificEpithet"))
  check_these <- data$taxonRank %in% "VARIEDADE"
  data$taxon_name[check_these] <- 
    plantR:::addRank(data$taxon_name[check_these], "var.")
  
  check_these <- data$taxonRank %in% "SUB_ESPECIE"
  data$taxon_name[check_these] <- 
    plantR:::addRank(data$taxon_name[check_these], "subsp.")
  
  check_these <- data$taxonRank %in% "FORMA"
  data$taxon_name[check_these] <- 
    plantR:::addRank(data$taxon_name[check_these], "f.")
  
  ## Standardizing taxon ranks
  patts <- c("ORDEM", "FAMILIA", "GENERO", "ESPECIE", "VARIEDADE",
             "SUB_ESPECIE", "CLASSE", "TRIBO", "SUB_FAMILIA", 
             "DIVISAO", "FORMA")
  names(patts) <- c("order", "family", "genus", "species", "variety",
                    "subspecies", "class", "tribe", "subfamily", 
                    "phylum", "form")
  for(i in seq_along(patts)) {
    data$taxonRank[data$taxonRank %in% patts[i]] <- names(patts)[i]
  }

  ## Standardizing taxon status
  patts <- c("NOME_ACEITO", "SINONIMO", "")
  names(patts) <- c("accepted", "synonym", "unplaced")
  for(i in seq_along(patts)) {
    data$taxonomicStatus[data$taxonomicStatus %in% patts[i]] <- 
      names(patts)[i]
  }
  
  ## Standardizing name status
  check_these <- data$taxonomicStatus %in% "accepted" & 
                  data$nomenclaturalStatus %in% ""
  data$nomenclaturalStatus[check_these] <- "NOME_CORRETO"
  
  status_conv <- as.data.frame(
    readxl::read_excel("data-raw/name_status_conversion.xlsx"))
  status_conv_bb <- status_conv[status_conv$backbone %in% backbone,
                                c("name.status", "replacement")]
  status_conv_bb <- unique(
    status_conv_bb[!is.na(status_conv_bb$name.status), ])
  patts <- status_conv_bb$name.status
  names(patts) <- status_conv_bb$replacement
  for(i in seq_along(patts)) {
    data$nomenclaturalStatus[data$nomenclaturalStatus %in% patts[i]] <- 
      names(patts)[i]
  }
  
  
  # Editing data --------------------------------------------------
  ## filtering and standardizing important column names
  cols <- c("taxonID", "higherClassification" ,"phylum", "family", 
            "taxon_name", "scientificNameAuthorship",
            "taxonRank", "nomenclaturalStatus", "taxonomicStatus", 
            "acceptedNameUsageID", "kingdom", "scientificName") 

  data <- as.data.frame(data)[, cols]
  names(data) <- c("id", "higherClassification", "phylum", "family", 
                   "name", "authorship", 
                   "taxon.rank", "name.status", "taxon.status", 
                   "accepted.id", "kingdom", "scientific.name")
  
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
  
  ## obtaining the accepted.name column
  rep_these <- is.na(data$accepted.id)
  data1 <- data[rep_these, 
                c("id", "name", "authorship", 
                  "taxon.rank", "taxon.status", "name.status")]
  names(data1)[1] <- "accepted.id"
  tmp <- dplyr::left_join(data, data1, by = "accepted.id")
  stopifnot(identical(tmp$id, data$id)) # should be TRUE
  
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
             "higherClassification", 
             "kingdom",
             "phylum",
             "family", # "genus", "specific.epiteth", "infra.epiteth",
             "name", # genus + epiteth + infra.epiteth
             "authorship", # name author
             "scientific.name", # name + authors
             "taxon.rank", # species, genus, family, order, etc.
             "taxon.status", # accepted or synonym
             "name.status", # correct, ilegitimate, legitimate, but incorrect, orthographical variant, missapplied, not validly published, rejected
             "accepted.id",  #accepted canonical             
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
  
  ## Higher class editing
  data$higherClassification <- gsub("Flora e Funga;", "", data$higherClassification)
  data$higherClassification <- gsub(";.*", "", data$higherClassification)
  rep_these <- data$phylum %in% "Ascomycota" & 
                data$higherClassification %in% "Angiospermas"
  if (any(rep_these))
    data$phylum[rep_these] <- "Tracheophyta" 
  
  rep_these <- data$phylum %in% "Tracheophyta"
  data$higherClassification[rep_these] <- "Tracheophyta"   
  data$higherClassification[data$higherClassification %in% "Algas"] <- 
    "Algae"
  data$higherClassification[data$higherClassification %in% "Briófitas"] <- 
    "Bryophyta"
  data$higherClassification[data$higherClassification %in% "Fungos"] <- 
    "Fungi"
  table(data$higherClassification)
  
  ## Obtaining the taxon distribution column
  file <- all_files[grepl("dist", all_files)]
  temp <- tempfile()
  dist <- data.table::fread(unzip(path, files = file, exdir = temp))
  unlink(temp)
  
  dist <- dist[!dist$locationID %in% c("", " ", "NA"), ]
  # dist$locationID <- gsub("BR-", "", dist$locationID, fixed = TRUE) # puco ganho de tamanho
  dist <- aggregate(dist$locationID, list(dist$id), 
                    function(x) paste0(sort(unique(x)), collapse = "|"))
  names(dist) <- c("id", "taxon.distribution")
  dist$id <- as.integer(dist$id)
  
  tmp <- data
  tmp1 <- dplyr::left_join(tmp, dist, by = "id")
  
  names(dist) <- c("accepted.id", "taxon.distribution")
  tmp2 <- dplyr::left_join(tmp, dist, by = "accepted.id")

  stopifnot(identical(tmp1$id, tmp2$id))
  
  rep_these <- !is.na(tmp2$taxon.distribution)
  tmp1$taxon.distribution[rep_these] <- 
    tmp2$taxon.distribution[rep_these] 
  
  stopifnot(identical(tmp1$id, data$id))
  
  data$taxon.distribution <- tmp1$taxon.distribution
    
  # Saving ------------------------------------------------------------
  # reinos <- c("Plantae", "Fungi")
  classes <- c("Tracheophyta", "Algae", "Bryophyta", "Fungi")
  
  ## Cleaning and re-ordering
  data <- data[!data$name %in% c("", NA, " ", "NA"), ]
  data <- data[order(data$taxon.status), ]
  data <- data[!duplicated(paste0(data$kingdom, data$scientific.name)), ]
  data <- data[!duplicated(paste0(data$higherClassification, data$scientific.name)), ]
  data <- data[order(data$id), ]
  
  ## Removing the combined name + authorship column
  data <- data[, -which(names(data) %in% "scientific.name")]

  ## Adding source acronym to the backbone ID
  data$id <- paste0(backbone, "-", data$id)
  rep_these <- !is.na(data$accepted.id)
  if (any(rep_these)) 
    data$accepted.id[rep_these] <- 
      paste0(backbone, "-", data$accepted.id[rep_these])
  
  ## How many columns and lines (in May 2024: 153,089)
  # dimensions <- paste0(dim(data)[1], " rows and ", dim(data)[2], " columns")
  
  ## Saving
  data_split <- split(data, data$higherClassification)
  data_split <- data_split[names(data_split) %in% classes]
  for (i in seq_along(classes)) {
    
    if (classes[i] == "Tracheophyta") {
      bfoNamesTracheophyta <- data_split[[classes[i]]]
      bfoNamesTracheophyta$higherClassification <- NULL
      
      usethis::use_data(bfoNamesTracheophyta, compress = "xz", 
                        overwrite = TRUE)
    }
    
    if (classes[i] == "Algae") {
      bfoNamesAlgae <- data_split[[classes[i]]]
      bfoNamesAlgae$higherClassification <- NULL
      
      if (all(bfoNamesAlgae$taxon.distribution %in% c("", " ", NA)))
        bfoNamesAlgae$taxon.distribution <- NULL
      
      usethis::use_data(bfoNamesAlgae, compress = "xz", 
                        overwrite = TRUE)
    }
    
    if (classes[i] == "Bryophyta") {
      bfoNamesBryophyta <- data_split[[classes[i]]]
      bfoNamesBryophyta$higherClassification <- NULL
      
      if (all(bfoNamesBryophyta$taxon.distribution %in% c("", " ", NA)))
        bfoNamesBryophyta$taxon.distribution <- NULL
      
      usethis::use_data(bfoNamesBryophyta, compress = "xz", 
                        overwrite = TRUE)
    }
    
    if (classes[i] == "Fungi") {
      bfoNamesFungi <- data_split[[classes[i]]]
      bfoNamesFungi$higherClassification <- NULL
      
      if (all(bfoNamesFungi$taxon.distribution %in% c("", " ", NA)))
        bfoNamesFungi$taxon.distribution <- NULL
      
      usethis::use_data(bfoNamesFungi, compress = "xz", 
                        overwrite = TRUE)
    }
    
    dimensions <- paste0(dim(data_split[[classes[i]]])[1], 
                         " rows and ", 
                         dim(data_split[[classes[i]]])[2], 
                         " columns")
    path_to_save <- file.path(here::here(), "data-raw", backbone, 
                              paste0("df_dim_", classes[i],".txt"))
    write(dimensions, path_to_save)
    
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
