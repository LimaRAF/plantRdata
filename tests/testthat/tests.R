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
  x <- .readScript("data-raw/wfo/last_update.txt")
  testthat::expect_false(is.null(x), FALSE)
  testthat::expect_error(.readScript())
  testthat::expect_warning(
  testthat::expect_error(.readScript("data-raw/wfo/xuxu.txt"))
  )
})
