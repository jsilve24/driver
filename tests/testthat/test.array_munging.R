context("array_munging")

test_that("gather_array and spread_array reverse eachother with defaults", {
  a <- array(1:100, dim=c(10, 5, 2))
  ga <- gather_array(a)
  expect_equivalent(a, spread_array(ga))
})

test_that("complete and any cases functions are correct", {
  a <- array(0, dim =c(2,2,2))
  a[,2,] <- NA

  expect_equal(any_cases_array(a), c(TRUE, TRUE))
  expect_equal(any_cases_array(a, margin=c(1,2)),
               cbind(c(TRUE, TRUE), c(FALSE, FALSE)))
  expect_equal(all_cases_array(a, margin=1), c(FALSE, FALSE))
  expect_equal(all_cases_array(a, margin=c(1,2)),
               cbind(c(TRUE, TRUE), c(FALSE, FALSE)))
})
