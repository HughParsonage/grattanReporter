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

test_that("Advises ISBN", {
  skip_if_not_installed("rlang")
  skip_if_not(identical(Sys.getenv("USERNAME"), "hughp"))
  skip_if_not(dir.exists("~/grattex/") && file.exists("~/grattex/Report.tex"))
  tempd <- tempfile()
  dir.create(tempd)
  expect_error(check_preamble("~/grattex/Report.tex",
                              pre_release = TRUE),
               regexp = "ISBN has already been used.")
  rlang::with_options({
    expect_output(tryCatch(check_preamble("~/grattex/Report.tex",
                                          pre_release = TRUE),
                           error = function(e) {
                             NULL
                           }),
                  regexp = "The next ISBN is ")
  },
  TeXCheckR.capture.output = TRUE)
})

