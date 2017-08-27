context("tidy_shortcuts")

x <- matrix(rnorm(100), 10, 10)
test_that("summarise_posterior is correct", {
  d <- gather_array(x, val) %>%
    summarise_posterior(val)
  d2 <- gather_array(x, var) %>%
    summarise_posterior(var)
  expect_equivalent(d, d2)
})
