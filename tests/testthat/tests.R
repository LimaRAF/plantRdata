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
  
  url0 <- "https://files.worldfloraonline.org/files/WFO_Backbone/_WFOCompleteBackbone/"
  zip <- "WFO_Backbone.zip"
  last_updated <- .getUpdates(url0, "wfo", zip)
  data <- readLines("data-raw/wfo/last_update.txt")
  testthat::expect_equal(last_updated, data)
  
  url0 <- "https://sftp.kew.org/pub/data-repositories/WCVP/"
  zip <- "wcvp.zip"
  last_updated <- .getUpdates(url0, "wcvp", zip)
  data <- readLines("data-raw/wcvp/last_update.txt")
  testthat::expect_equal(last_updated, data)

  url0 <- "https://hosted-datasets.gbif.org/datasets/backbone/current/"
  zip <- "backbone.zip"
  last_updated <- .getUpdates(url0, "gbif", zip)
  data <- readLines("data-raw/gbif/last_update.txt")
  testthat::expect_equal(last_updated, data)
  
  url0 <- "https://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
  zip <- "bfo.zip"
  last_updated <- .getUpdates(url0, "bfo", zip)
  data <- readLines("data-raw/bfo/last_update.txt")
  testthat::expect_equal(last_updated, data)

  # url0 <- ""
  # zip <- "lcvp.zip"
  # last_updated <- .getUpdates(url0, "lcvp", zip)
  # data <- readLines("data-raw/lcvp/last_update.txt")
  # testthat::expect_equal(last_updated, data)
  
  
})
