context("fix_labels")

test_that("fix labels", {
  fix_labels("fix-labels/fix-labels-1.tex", "fix-labels/fix-labels-1-out.tex")
  result <- readr::read_lines("fix-labels/fix-labels-1-out.tex")
  expected <- readr::read_lines("fix-labels/fix-labels-1-expected.tex")
  
  expect_identical(fix_labels("fix-labels/fix-labels-1.tex", "fix-labels/fix-labels-1-out.tex"), expected)
})
