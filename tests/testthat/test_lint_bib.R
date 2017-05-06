context("lint_bib")

test_that("Correctly tidies known input", {
  if (file.exists("lint_bib_out.bib")) file.remove("lint_bib_out.bib")
  lint_bib("lint_bib_in.bib", "lint_bib_out.bib")
  expect_identical(readLines("lint_bib_out.bib", encoding = "UTF-8"),
                   readLines("lint_bib_out_correct.bib", encoding = "UTF-8"))
})

test_that("Correctly changes entry type and removes bad fields", {
  if (file.exists("./lint-bib/entry-type-out.bib")) file.remove("./lint-bib/entry-type-out.bib")
  lint_bib(bib_file = "./lint-bib/entry-type-in.bib", outfile = "./lint-bib/entry-type-out.bib")
  expect_identical(readLines("./lint-bib/entry-type-correct.bib", encoding = "UTF-8"),
                   readLines("./lint-bib/entry-type-out.bib", encoding = "UTF-8"))
})

