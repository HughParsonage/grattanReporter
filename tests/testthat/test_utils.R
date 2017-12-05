context("utils")

test_that("Seq_union always length one", {
  expect_equal(Seq_union(1, 5), 1:5)
  expect_equal(Seq_union(1, c(5, 10)), 1:10)
  expect_equal(Seq_union(c(73, 100), c(83, 105)), c(73:83, 100:105))
  expect_equal(Seq_union(c(1, 10), c(9, 19)), 1:19)
})

test_that("nth_max and nth_min", {
  expect_equal(nth_max(sample(1:10), 3), 8)
  expect_equal(nth_min(sample(1:10), 4), 4)
})

test_that("insert", {
  expect_equal(insert(1:5, 3, -1L),
               c(1, 2, -1, 3, 4, 5))
})
