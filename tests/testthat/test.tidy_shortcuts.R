context("tidy_shortcuts")

x <- matrix(rnorm(100), 10, 10)
test_that("summarise_posterior is correct", {
  d <- gather_array(x, val) %>%
    summarise_posterior(val)
  d2 <- gather_array(x, var) %>%
    summarise_posterior(var)
  expect_equivalent(d, d2)
})

test_that("summarise_posterior is correct with grouping", {
  d <- data.frame("a" = c(1:10), "b"=rep(c(1,2), 5)) %>%
       group_by(b) %>%
       summarise_posterior(a) %>%
       ungroup()
  expect_equivalent(d$mean, c(5, 6))
})

