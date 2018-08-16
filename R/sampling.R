#' Sample Uniformily from various objects centered at the origin
#'
#' @param D dimension of object
#' @param n_samples number of samples to produce
#' @return matrix of dimension D x n_samples
#' @name sample_uniform
NULL

#' @rdname sample_uniform
#' @export
#' @param radius radius of sphere to sample from
#' @param shell_only if TRUE samples from shell of object only, if false
#'   samples from entire region of object
rUnifSphere <- function(D, radius, n_samples=1, shell_only=FALSE){
  z <- matrix(rnorm(D*n_samples), D, n_samples)
  sf <- sqrt(colSums(z^2))
  if (shell_only) {
    ru <- radius
  } else {
    ru <- radius*runif(n_samples)^(1/D)
  }
  sweep(z, 2, ru/sf, FUN="*")
}

#' @rdname sample_uniform
#' @export
rUnifSimplex <- function(D,  n_samples=1){
  z <- matrix(rexp(D*n_samples), n_samples, D)
  t(miniclo(z))
}
