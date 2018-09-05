context("ISBN utilities")

test_that("gets ISBN", {
  skip_on_cran()
  skip_on_travis()
  skip_if_not(identical(Sys.getenv("USERNAME"), "hughp"))

  expect_equal(normalizePath(dropbox_path(), winslash = "/"),
               normalizePath("~/../Dropbox (Grattan Institute)", winslash = "/"))

  expect_gt(nrow(isbn_table()), 0)
  expect_true("ISBN" %in% names(isbn_table()))

})



