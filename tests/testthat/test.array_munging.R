context("array_munging")

test_that("gather_array and spread_array reverse eachother with defaults", {
  a <- array(1:100, dim=c(10, 5, 2))
  ga <- gather_array(a)
  expect_equivalent(a, spread_array(ga))
})
