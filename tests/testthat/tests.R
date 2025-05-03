#### plantRdata tests ####

## internal functions -----------------------------------------------
testthat::test_that("Testing .squish", {
  x <- .squish(" AA  bb ")
  testthat::expect_equal(x, "AA bb")
})

testthat::test_that("Testing .buildName", {
  df <- data.frame(genus = "AA", species = "bb")
  x <- .buildName(df)
  testthat::expect_equal(x, "AA bb")
})

testthat::test_that("Testing .readScript", {
  testthat::expect_error(.readScript())
  testthat::expect_warning(
  testthat::expect_error(.readScript("xuxu.txt"))
  )
})

testthat::test_that("Testing .getUpdates", {
  
  all_files <- list.files(
    system.file("testdata", package = "plantRdata"), 
    full.names = TRUE)
  
  url0 <- "http://files.worldfloraonline.org/files/WFO_Backbone/_WFOCompleteBackbone/"
  zip <- "WFO_Backbone.zip"
  bb <- "wfo"
  last_updated <- .getUpdates(url0, bb, zip)
  path <- all_files[grepl(bb, all_files)]
  data <- readLines(path)
  testthat::expect_equal(last_updated, data)
  
  url0 <- "http://sftp.kew.org/pub/data-repositories/WCVP/"
  zip <- "wcvp.zip"
  bb <- "wcvp"
  last_updated <- .getUpdates(url0, bb, zip)
  path <- all_files[grepl(bb, all_files)]
  data <- readLines(path)
  testthat::expect_equal(last_updated, data)
  
  url0 <- "http://hosted-datasets.gbif.org/datasets/backbone/current/"
  zip <- "backbone.zip"
  bb <- "gbif"
  last_updated <- .getUpdates(url0, bb, zip)
  path <- all_files[grepl(bb, all_files)]
  data <- readLines(path)
  testthat::expect_equal(last_updated, data)
  
  url0 <- "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
  zip <- "bfo.zip"
  bb <- "bfo"
  last_updated <- .getUpdates(url0, bb, zip)
  path <- all_files[grepl(bb, all_files)]
  data <- readLines(path)
  testthat::expect_equal(last_updated, data)
  
  # url0 <- ""
  # zip <- "lcvp.zip"
  # bb <- "lcvp"
  # last_updated <- .getUpdates(url0, bb, zip)
  # path <- all_files[grepl(bb, all_files)]
  # data <- readLines(path)
  # testthat::expect_equal(last_updated, data)
  
  
})
