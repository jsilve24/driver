#' Sample Uniformily from various objects centered at the origin
#'
#' @param n number of samples to produce
#' @param D dimension of object
#' @return matrix of dimension D x n
#' @name sample_uniform
#' @examples
#' # Sample Uniformily from Sphere (Full)
#' x <- rUnifSphere(1000, 2, 2)
#' plot(t(x), asp=1)
#'
#' # Sample Uniformily from Sphere (Shell Only)
#' x <- rUnifSphere(1000, 2, 2, shell_only=TRUE)
#' plot(t(x), asp=1)
#'
#' # Sample Uniformily from Simplex
#' x <- rUnifSimplex(1000, 2)
#' plot(t(x), asp=1)
NULL

#' @rdname sample_uniform
#' @export
#' @param radius radius of sphere to sample from
#' @param shell_only if TRUE samples from shell of object only, if false
#'   samples from entire region of object
rUnifSphere <- function(n, D, radius=1, shell_only=FALSE){
  z <- matrix(rnorm(D*n), D, n)
  sf <- sqrt(colSums(z^2))
  if (shell_only) {
    ru <- radius
  } else {
    ru <- radius*runif(n)^(1/D)
  }
  sweep(z, 2, ru/sf, FUN="*")
}

#' @rdname sample_uniform
#' @export
rUnifSimplex <- function(n, D){
  z <- matrix(rexp(D*n), n, D)
  t(miniclo(z))
}
