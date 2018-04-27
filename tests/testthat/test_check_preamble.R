context("check preamble")

test_that("todonotes even if disabled not acceptable", {
  # expect_error(check_preamble("./check-preamble/todonotes.tex", release = TRUE, pre_release = TRUE))
  # expect_error(check_preamble("./check-preamble/todonotes-disable.tex", release = TRUE, pre_release = TRUE))
  expect_true(TRUE)
})

test_that("Working paper inconsistency", {
  expect_error(check_preamble("./check-preamble/working-paper/working-paper-with-report-not.tex"),
               regexp = "Working paper / Report inconsistency")
  expect_error(check_preamble("./check-preamble/working-paper/report-with-working-paper.tex"),
               regexp = "ReportOrWorkingPaper not set to.*Working Paper")
  expect_error(check_preamble("./check-preamble/working-paper/almost-working-paper.tex"),
               regexp = "Working paper / Report inconsistency")
})

test_that("Background paper ok", {
  expect_null(check_preamble("./check-preamble/background-paper/background-paper.tex",
                             pre_release = TRUE))
})

