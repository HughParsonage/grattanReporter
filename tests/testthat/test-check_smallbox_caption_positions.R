context("Smallbox captions")

test_that("Errors when smallbox occurs near chapter", {
  skip_on_travis()
  expect_error(checkGrattanReport("./check-smallbox", compile = TRUE),
               regexp = "Smallbox.*intrudes on chapter heading")
})
