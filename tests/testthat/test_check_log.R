context("check log")

test_that("Errors when encountering a straddling page reference", {
  skip_on_travis()
  expect_error(checkGrattanReport("check-log", compile = TRUE, pre_release = TRUE),
               regexp = "Unstable Vref")
})
