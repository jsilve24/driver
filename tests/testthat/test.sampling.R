context("sampling")


test_that("rUnifSimplex correct mean (also visual checks)", {
  z <- rUnifSimplex(10000, 3)
  # visual tests only... hard to test otherwise
  # t(z) %>% compositions::acomp() %>% compositions::plot.acomp()
  z <- t(z)
  expect_equal(c(driver::clrInv(colMeans(driver::clr(z)))), rep(1/3,3),
               tolerance=1e-2)
})
