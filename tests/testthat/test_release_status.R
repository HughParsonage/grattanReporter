context("release status")

test_that("returns compile when present", {
  expect_equal(any(grepl("compile", release_status("release-status-compile.tex"))), TRUE)
})

test_that("returns pre_release when present", {
  expect_equal(any(grepl("pre_release", release_status("release-status-compile.tex"))), TRUE)
})

test_that("returns release when present", {
  expect_equal(any(grepl("^release", release_status("release-status-compile.tex"))), TRUE)
})

test_that("Release even if trailing whitespace", {
  expect_equal(release_status("release-status-compile-ws.tex"), "compile")
})

test_that("returns FALSE when no directive present", {
  expect_equal(release_status("release-status-no-directive.tex"), FALSE)
})






