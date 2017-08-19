library(abind)
context("CoDA Functions")

# test coda basic ---------------------------------------------------------
# Changed tolerance on values that I copied from terminal

x <- matrix(runif(300), 100, 3)
x <- miniclo(x)

test_that("miniclo sums to 1", {
  expect_equal(rowSums(x), rep(1, nrow(x)))
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


# test coda array ---------------------------------------------------------

a <- abind(x, x, along = 3)
a.perm <- aperm(a, c(1,3,2))

test_that("alr_array is correct", {
  a.alr <- alr_array(a, 2)
  a1.alr <- alr(a[,,1], 2)
  expect_equivalent(a.alr, abind(a1.alr, a1.alr, along=3))

  a.perm.alr <- alr_array(a.perm, 2, 1, 3)
  expect_equivalent(a.alr, a.perm.alr)
})

test_that("alrInv_array reverses alr_array", {
  a.alr <- alr_array(a, 2)
  expect_equivalent(alrInv_array(a.alr, 2), a)
})

test_that("ilr_array is correct and reverses with ilrInv_array", {
  V <- philr::buildilrBasep(philr::phylo2sbp(philr::named_rtree(3)), c(1,1,1))

  a.ilr <- ilr_array(a, V)
  a1.ilr <- ilr(a[,,1], V)
  expect_equivalent(a.ilr, abind(a1.ilr, a1.ilr, along=3))

  colnames(V) <- c("n1", "n2")
  rownames(V) <- c("t1", "t2", "t3")

  a.ilr <- ilr_array(a, V)
  a.after.inverse <- ilrInv_array(a.ilr, V)

  expect_equal(dimnames(a.ilr)[[2]], c("n1", "n2"))
  expect_equal(dimnames(a.after.inverse)[[2]], c("t1", "t2", "t3"))
  expect_equal(unname(a), unname(a.after.inverse))
})

test_that("clr_array is correct and reverses with clrInv_array", {
  a.clr <- clr_array(a)
  a1.clr <- clr(a[,,1])
  expect_equivalent(a.clr, abind(a1.clr, a1.clr, along=3))

  a.clr <- clr_array(a)
  a.after.inverse <- clrInv_array(a.clr)

  expect_equivalent(a, a.after.inverse)
})

test_that("miniclo_array gives correct results", {
  expect_equal(miniclo_array(5*a, samples=1, parts=2), a)
})







