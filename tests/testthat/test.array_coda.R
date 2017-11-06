context("Array CoDA Functions")

a <- array(runif(600), dim = c(100, 3, 2))

test_that("array_apply_1D_function correctness on Trivial Function", {

  a1 <- array_apply_1D_function(a, 1, function(x) matrix(rep(1, nrow(x)), nrow(x)))
  expect_equal(array(1, dim=c(1, 3, 2)), a1)

  a2 <- array_apply_1D_function(a, 2, function(x) matrix(rep(1, nrow(x)), nrow(x)))
  expect_equal(array(1, dim=c(100, 1, 2)), a2)

  a3 <- array_apply_1D_function(a, 3, function(x) matrix(rep(1, nrow(x)), nrow(x)))
  expect_equal(array(1, dim=c(100, 3, 1)), a3)
})

test_that("miniclo_array correctness", {
  d <- dim(a)

  # For dim_1 solve with for loops
  a1 <- array(0, dim=d)
  for (i in 1:d[2]){
    for (j in 1:d[3]){
      a1[,i,j] <- miniclo(a[,i,j])
    }
  }
  expect_equal(miniclo_array(a, 1) , a1)


  # For dim_2 solve with for loops
  a2 <- array(0, dim=d)
  for (i in 1:d[3]){
    a2[,,i] <- miniclo(a[,,i])
  }

  expect_equal(miniclo_array(a, 2) , a2)

  # For dim_3 solve with for loops
  a3 <- array(0, dim=d)
  for (i in 1:d[1]){
    a3[i,,] <- miniclo(a[i,,])
  }

  expect_equal(miniclo_array(a, 3) , a3)
})


test_that("alr_array and alrInv_array correctness", {
  d <- dim(a)

  # Create Compositions from a
  a1 <- array(0, dim=d)
  for (i in 1:d[2]){
    for (j in 1:d[3]){
      a1[,i,j] <- miniclo(a[,i,j])
    }
  }

  a2 <- array(0, dim=d)
  for (i in 1:d[3]){
    a2[,,i] <- miniclo(a[,,i])
  }

  a3 <- array(0, dim=d)
  for (i in 1:d[1]){
    a3[i,,] <- miniclo(a[i,,])
  }

  # Now test alr_array
  a1_alr <- array(0, dim=c(d[1]-1, d[2:3]))
  for (i in 1:d[2]){
    for (j in 1:d[3]){
      a1_alr[,i,j] <- alr(a1[,i,j])
    }
  }
  expect_equal(alr_array(a1, parts=1), a1_alr)
  expect_equal(alrInv_array(alr_array(a1, parts=1), coords=1), a1)


  a2_alr <- array(0, dim=c(d[1], d[2]-1, d[3]))
  for (i in 1:d[3]){
    a2_alr[,,i] <- alr(a2[,,i])
  }
  expect_equal(alr_array(a2, parts=2), a2_alr)
  expect_equal(alrInv_array(alr_array(a2, parts=2), coords=2), a2)

  a3_alr <- array(0, dim=c(d[1:2], d[3]-1))
  for (i in 1:d[1]){
    a3_alr[i,,] <- alr(a3[i,,])
  }
  expect_equal(alr_array(a3, parts=3), a3_alr)
  expect_equal(alrInv_array(alr_array(a3, parts=3), coords=3), a3)
})

test_that("clr_array and clrInv_array correctness", {
  d <- dim(a)

  # Create Compositions from a
  a1 <- array(0, dim=d)
  for (i in 1:d[2]){
    for (j in 1:d[3]){
      a1[,i,j] <- miniclo(a[,i,j])
    }
  }

  a2 <- array(0, dim=d)
  for (i in 1:d[3]){
    a2[,,i] <- miniclo(a[,,i])
  }

  a3 <- array(0, dim=d)
  for (i in 1:d[1]){
    a3[i,,] <- miniclo(a[i,,])
  }

  # Now test clr_array
  a1_clr <- array(0, dim=c(d[1], d[2:3]))
  for (i in 1:d[2]){
    for (j in 1:d[3]){
      a1_clr[,i,j] <- clr(a1[,i,j])
    }
  }
  expect_equal(clr_array(a1, parts=1), a1_clr)
  expect_equal(clrInv_array(clr_array(a1, parts=1), coords=1), a1)


  a2_clr <- array(0, dim=c(d[1], d[2], d[3]))
  for (i in 1:d[3]){
    a2_clr[,,i] <- clr(a2[,,i])
  }
  expect_equal(clr_array(a2, parts=2), a2_clr)
  expect_equal(clrInv_array(clr_array(a2, parts=2), coords=2), a2)

  a3_clr <- array(0, dim=c(d[1:2], d[3]))
  for (i in 1:d[1]){
    a3_clr[i,,] <- clr(a3[i,,])
  }
  expect_equal(clr_array(a3, parts=3), a3_clr)
  expect_equal(clrInv_array(clr_array(a3, parts=3), coords=3), a3)
})

test_that("ilr_array and ilrInv_array correctness", {
  d <- dim(a)

  # Create Compositions from a
  a1 <- array(0, dim=d)
  for (i in 1:d[2]){
    for (j in 1:d[3]){
      a1[,i,j] <- miniclo(a[,i,j])
    }
  }

  a2 <- array(0, dim=d)
  for (i in 1:d[3]){
    a2[,,i] <- miniclo(a[,,i])
  }

  a3 <- array(0, dim=d)
  for (i in 1:d[1]){
    a3[i,,] <- miniclo(a[i,,])
  }

  # Now test ilr_array
  a1_ilr <- array(0, dim=c(d[1]-1, d[2:3]))
  for (i in 1:d[2]){
    for (j in 1:d[3]){
      a1_ilr[,i,j] <- ilr(a1[,i,j])
    }
  }
  expect_equal(ilr_array(a1, parts=1), a1_ilr)
  expect_equal(ilrInv_array(ilr_array(a1, parts=1), coords=1), a1)


  a2_ilr <- array(0, dim=c(d[1], d[2]-1, d[3]))
  for (i in 1:d[3]){
    a2_ilr[,,i] <- ilr(a2[,,i])
  }
  expect_equal(ilr_array(a2, parts=2), a2_ilr)
  expect_equal(ilrInv_array(ilr_array(a2, parts=2), coords=2), a2)

  a3_ilr <- array(0, dim=c(d[1:2], d[3]-1))
  for (i in 1:d[1]){
    a3_ilr[i,,] <- ilr(a3[i,,])
  }
  expect_equal(ilr_array(a3, parts=3), a3_ilr)
  expect_equal(ilrInv_array(ilr_array(a3, parts=3), coords=3), a3)
})


