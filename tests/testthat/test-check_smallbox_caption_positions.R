context("Smallbox captions")

test_that("Errors when smallbox occurs near chapter", {
  expect_error(checkGrattanReport("check-smallbox", compile = TRUE),
               regexp = "Smallbox.*intrudes on chapter heading")
})