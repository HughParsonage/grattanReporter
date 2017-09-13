context("check log")

test_that("Errors when encountering a straddling page reference", {
  skip_on_travis()
  expect_error(checkGrattanReport("check-log", compile = TRUE, pre_release = TRUE),
               regexp = "Unstable Vref")
})

if (file.exists("./check-log/travis/grattanReport/md5/bib/mini.bib")) {
  file.remove("./check-log/travis/grattanReport/md5/bib/mini.bib")
}
