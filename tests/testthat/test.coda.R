library(abind)
context("CoDA Functions")

# test coda basic ---------------------------------------------------------
# Changed tolerance on values that I copied from terminal

x <- matrix(runif(300), 100, 3)
x <- miniclo(x)

test_that("miniclo sums to 1", {
  expect_equal(rowSums(x), rep(1, nrow(x)))
})

test_that("miniclo respects matrix names", {
  v <- matrix(c(7, 3, 18), nrow=1)
  colnames(v) <- c("a", "b", "c")
  expect_equal(colnames(v), colnames(miniclo(v)))
})

test_that("miniclo respects vector names", {
  v <- c("a" = 7, "b" = 3, "c" = 18)
  expect_equal(names(v), colnames(miniclo(v)))
})

test_that("alr correct", {
  a <- alr(c(1,2,3), 3)
  expect_equal(a, matrix(c(-1.0986123, -0.4054651), 1,2),
               tolerance=1e-7)
  a <- alr(c(1,2,3), 2)
  expect_equal(a, matrix(c(-0.6931472, 0.4054651), 1,2),
               tolerance=1e-7)
})

test_that("alr and alrInv reverse each-other", {
  expect_equal(alrInv(alr(x)), x)
})

test_that("ilr correct", {
  V <- rbind(c(-0.8164966, 0.0000000),
             c(0.4082483, -0.7071068),
             c(0.4082483, 0.7071068))
  colnames(V) <- c("n1", "n2")
  rownames(V) <- c("t1", "t2", "t3")
  ref <- matrix(c(0.7314828, 0.2867071), 1,2)
  expect_equal(unname(ilr(c(1,2,3), V)), ref, tolerance=1e-7)
  expect_equal(colnames(ilr(x, V)), colnames(V))
})

test_that("ilr and ilrInv referse each-other",{
  expect_equal(ilr(ilrInv(x)), x)
})






