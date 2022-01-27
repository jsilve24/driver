context("conjugate samplers")

set.seed(349)

test_that("clm gives correct results", {
      # simulate data
      beta <- 3.4
      sigma2 <- 1.5
      n <- 2000
      x <- rnorm(n)
      y <- sigma2 * rnorm(n)
      y <- beta*x + y
      fit <- clm(y, x, 0, 1, 2, 2, TRUE)
      expect(abs(fit$muN - beta) < 0.02, "muN too far from truth")
})
