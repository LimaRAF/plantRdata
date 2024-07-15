#' Run plantRdata Data Creation Steps
#' 
#' @description 
#' A paragraph providing a full description of the project and
#' describing each step of the workflow.
#' 
#' @author Renato A. Ferreira de Lima \email{raflima@usp.br}
#' 
#' @date 2024/04/26
## Install Dependencies (listed in DESCRIPTION) ------------------
devtools::install_deps(upgrade = "never")

## Load Project Addins (R Functions and Packages) ----------------
devtools::load_all(here::here())

## Global Variables ----------------------------------------------
# You can list global variables here (or in a separate R script)

## Run Project ---------------------------------------------------
# Creating the WFO taxonomic backbone
source(here::here("data-raw", "get_wfo.R"))
# Creating the WCVP taxonomic backbone
source(here::here("data-raw", "get_wcvp.R"))
# Creating the GBIF taxonomic backbone
source(here::here("data-raw", "get_gbif.R"))
# Creating the BFO taxonomic backbone
source(here::here("data-raw", "get_bfo.R"))
# Creating the LCVP taxonomic backbone
# source(here::here("data-raw", "get_lcvp.R"))
# Creating the GDAM polygons
# source(here::here("data-raw", "get_gadm.R"))
# Building the map at global scale
# source(here::here("data-raw", "get_world_map.R"))
# Building the map for latin america (central/south america + caribbeans)
# source(here::here("data-raw", "get_latam_map.R"))

# Saving the last update info in the inst/testdata folder
source(here::here("data-raw", "raw2testdata.R"))

## Build/Updating Package Manuals ---------------------------------
devtools::document(here::here())

# block <- roxygen2::parse_file("./R/wfoNames.R")[[1]]
# block <- roxygen2::parse_file("./R/wfoNames.R")
# roxygen2::roclet(block)

