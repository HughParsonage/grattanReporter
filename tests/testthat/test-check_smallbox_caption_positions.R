context("Smallbox captions")

test_that("Errors when smallbox occurs near chapter", {
  skip_on_travis()
  expect_error(checkGrattanReport("./check-smallbox", compile = TRUE),
               regexp = "Smallbox.*intrudes on chapter heading")
})

if (file.exists("./check-smallbox/travis/grattanReport/md5/bib/Grattan-Master-Bibliography.bib")) {
  file.remove("./check-smallbox/travis/grattanReport/md5/bib/Grattan-Master-Bibliography.bib")
}
