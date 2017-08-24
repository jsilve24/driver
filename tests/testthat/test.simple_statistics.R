context("simple statistics")

test_that("dmode returns correct results", {
  x <- c(1,2,3,3)
  expect_equal(dmode(x), 3)

  # Try with 2 modes
  x <- c(x, 2)
  expect_equal(dmode(x), c(2,3))
})
