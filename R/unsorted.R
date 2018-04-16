#' Create Block Diagonal Matrix from Passed Matricies
#'
#' Copied from similarly named function in dlm package
#'
#' @param ... matricies to create block diagonal matrix from
#'
#' @return matrix
#' @export
#'
#' @examples
#' x <- matrix(runif(10), 5, 2)
#' y <- matrix(runif(6), 3, 2)
#' bdiag(x,y)
bdiag <- function (...) {
  if (nargs() == 1)
    x <- as.list(...)
  else x <- list(...)
  n <- length(x)
  if (n == 0)
    return(NULL)
  x <- lapply(x, function(y) if (length(y))
    as.matrix(y)
    else stop("Zero-length component in x"))
  d <- array(unlist(lapply(x, dim)), c(2, n))
  rr <- d[1, ]
  cc <- d[2, ]
  rsum <- sum(rr)
  csum <- sum(cc)
  out <- array(0, c(rsum, csum))
  ind <- array(0, c(4, n))
  rcum <- cumsum(rr)
  ccum <- cumsum(cc)
  ind[1, -1] <- rcum[-n]
  ind[2, ] <- rcum
  ind[3, -1] <- ccum[-n]
  ind[4, ] <- ccum
  imat <- array(1:(rsum * csum), c(rsum, csum))
  iuse <- apply(ind, 2, function(y, imat) imat[(y[1] + 1):y[2],
                                               (y[3] + 1):y[4]], imat = imat)
  iuse <- as.vector(unlist(iuse))
  out[iuse] <- unlist(x)
  return(out)
}


#' Check if Vector and if so, Convert to Row Matrix
#'
#' @param x vector or matrix
#'
#' @return matrix
#' @export
#'
#' @examples
#' vec_to_mat(c(1,2,3))
#' vec_to_mat(rbind(c(1,2,3), c(1,2,3)))
vec_to_mat <- function(x){
  if (is.vector(x)) {
    n <- names(x)
    x <- matrix(x, nrow = 1)
    colnames(x) <- n
  }
  x
}


#' Center matrix (subtract row mean from each row)
#'
#' After centering, rowsums sum to zero. Particularly useful when working
#' with categorical data in log-space.
#'
#' @param x vector or matrix
#'
#' @return matrix with centered rows
#' @export
#'
#' @examples
#' x <- matrix(rnorm(100), 10, 10)
#' y <- center(x)
#' rowSums(y)
center <- function(x){
  sweep(vec_to_mat(x), 1, rowMeans(x), FUN=`-`)
}


#' One Hot Encoding of vector
#'
#' @param x vector with D possible categorical values (will be coerced to factor)
#'
#' @return matrix with D columns and same number of columns as length of x
#' @export
#'
#' @examples
#' onehot(1:5)
onehot <- function(x){
  model.matrix(~as.factor(x)-1)
}


#' Logit Transformation of Vector
#'
#' @param x vector
#'
#' @return vector
#' @export
#'
#' @examples
#' Logit(c(.2, .4, 1, 0))
#'
Logit <- function(x) {
  log(x/(1-x))
}

#' Inverse Logit Transformation of Vector
#'
#' @param x vector
#'
#' @return vector
#' @export
#'
#' @examples
#' invLogit(c(.2, .4, 1, 0))
invLogit <- function(x) {
  1/(1+exp(-x))
}


