context("Smallbox captions")

test_that("Errors when smallbox occurs near chapter", {
  skip_on_travis()

  expect_error(checkGrattanReport("./check-smallbox", compile = TRUE,
                                  update_grattan.cls = TRUE),
               regexp = "Smallbox.*intrudes on chapter heading")
})

if (dir.exists("check-smallbox/logos")) {
  vapply(dir(path = "check-smallbox/logos", full.names = TRUE),
         file.remove,
         TRUE)
}
if (file.exists("./check-smallbox/travis/grattanReport/md5/bib/Grattan-Master-Bibliography.bib")) {
  file.remove("./check-smallbox/travis/grattanReport/md5/bib/Grattan-Master-Bibliography.bib")
}


