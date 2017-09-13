context("inputs")

test_that("Simple input works", {
  expect_null(checkGrattanReport("./SF-input"))
  if (file.exists("./SF-input/travis/grattanReport/md5/SchoolFunding.bib")) {
    file.remove("./SF-input/travis/grattanReport/md5/SchoolFunding.bib")
  }
})
