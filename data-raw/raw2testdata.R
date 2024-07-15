#' @title Copy Info to Test Data
#'
#' @author Renato A. Ferreira de Lima
#' 
#' @keywords internal
#' 
#' @noRd
#'
#' @date 2024/07/15
# Creating the directory -----------------------------------------
dir_path <- file.path("inst", "testdata")
if (!dir.exists(dir_path)) 
  dir.create(dir_path)

# Getting paths and files ----------------------------------------
dir_raw <- "data-raw"
bbs <- gsub("/","", gsub(dir_raw, "", list.dirs(dir_raw)))
bbs <- bbs[!bbs %in% ""]
for (i in seq_along(bbs)) {
  orig_path <- file.path("data-raw", bbs[i], "last_update.txt")
  if (file.exists(orig_path)) {
    save_path <- file.path(dir_path, paste0(bbs[i], "_last_update.txt"))
    file.copy(orig_path, save_path, overwrite = TRUE)
  }
}
rm(list = ls())
