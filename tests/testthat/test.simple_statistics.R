context("simple statistics")

test_that("mode returns correct results", {
  x <- c(1,2,3,3)
  expect_equal(mode(x), 3)

  # Try with 2 modes
  x <- c(x, 2)
  expect_equal(mode(x), c(2,3))
})
