context("check log")

test_that("Errors when encountering a straddling page reference", {
  expect_error(checkGrattanReport("check-log", compile = TRUE, pre_release = TRUE),
               regexp = "Unstable Vref")
})
