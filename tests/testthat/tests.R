#### plantRdata tests ####

## internal functions -----------------------------------------------
test_that("Testing .squish", {
  x <- .squish(" AA  bb ")
  testthat::expect_equal(x, "AA bb")
})

test_that("Testing .buildName", {
  df <- data.frame(genus = "AA", species = "bb")
  x <- .buildName(df)
  testthat::expect_equal(x, "AA bb")
})

test_that("Testing .readScript", {
  testthat::expect_error(.readScript())
  testthat::expect_warning(
  testthat::expect_error(.readScript("xuxu.txt"))
  )
})

test_that("Testing .getUpdates", {
  
  url0 <- "https://files.worldfloraonline.org/files/WFO_Backbone/_WFOCompleteBackbone/"
  zip <- "WFO_Backbone.zip"
  last_updated <- .getUpdates(url0, "wfo", zip)
  testthat::expect_equal(last_updated, "1/22/2024")
  
  url0 <- "https://sftp.kew.org/pub/data-repositories/WCVP/"
  zip <- "wcvp.zip"
  last_updated <- .getUpdates(url0, "wcvp", zip)
  testthat::expect_equal(last_updated, "2023-10-04")

  url0 <- "https://hosted-datasets.gbif.org/datasets/backbone/current/"
  zip <- "backbone.zip"
  last_updated <- .getUpdates(url0, "gbif", zip)
  testthat::expect_equal(last_updated, "2023-08-28")
  
})
