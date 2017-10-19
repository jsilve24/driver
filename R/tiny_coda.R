# generic functions
#' Closure operator
#'
#' @param x vector or matrix (rows are samples, parts are columns) of data in simplex
#'
#' @return x with row entries divided by sum of row (converts vectors to row matricies)
#' @export
#'
#' @examples
#' x <- matrix(runif(30), 10, 3)
#' x <- miniclo(x)
miniclo <- function(x){
  if (is.vector(x)) x <- matrix(x, nrow = 1)
  (x/rowSums(x))
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


# BASE CODA ---------------------------------------------------------------

#' Log-Ratio Transformation
#'
#' \code{glr} is generic log-ratio transform, code used by other
#' transforms, can be called directly.
#'
#' @param x vector or matrix (rows are samples, parts are columns) of data in simplex
#' @param y matrix (rows are samples, coords are columns) of transformed data
#' @param V transformation matrix (defines transform)
#' @param d for ALR, which component (integer position) to take as reference
#' (default is ncol(x)) for alrInv corresponds to column position in untransformed
#' matrix.
#' @param inv for ALR and CLR, transformation matrix is different forward and inverse
#' @param D the number of parts (e.g., number of columns in untransformed data)
#' @return matrix (converts vectors to row matricies)
#' @details The implementation of the ILR transform here relies on the fact that
#' all the standard log-ratio transforms can be written in the following form
#' \deqn{y=\log(x)V} with inverse transform given by
#' \deqn{x=\mathcal{C}[exp(yV^t)]} where \eqn{\mathcal{C}[\cdot]} is the closure operator (\code{\link{miniclo}}). Note however that if \eqn{V} does not represent an orthonormal
#' basis in the Aitchison geometry then the \eqn{V} used for the log-ratio transform may be different
#' than the one used for the reverse transform (this is the case for the ALR and CLR transforms).
#' Default ILR base formed by Gram-Schmidt orthogonalization of an ALR basis.
#' @name base_lr_transforms
#' @examples
#' #ALR Transform
#' x <- matrix(runif(30), 10, 3)
#' x <- miniclo(x)
#' x.alr <- alr(x, 2)
#' x <- alrInv(x.alr, 2)
#'
#' # ILR
#' x.ilr <- ilr(x)
#' x <- ilrInv(x.ilr)
#'
#' # CLR
#' x.clr <- clr(x)
#' x <- clrInv(x.clr)
#'
#' # CUSTOM - Be careful if your custom matrix is not
#' # orthogonal the inverse transform may not be given by just the transpose!
#' # For example, this is the case for the ALR
#' V <- matrix(c(1, 1, -1), 3, 1)
#' x.custom <- glr(x, V)
NULL

#' @rdname base_lr_transforms
#' @export
glr <- function(x, V){
  log(x) %*% V
}

#' @rdname base_lr_transforms
#' @export
glrInv <- function(y, V){
  tmp <- exp(y %*% t(V))
  miniclo(tmp)
}

#' @rdname base_lr_transforms
#' @export
create_alr_base <- function(D,d, inv=FALSE){
  if (d < 1 | d > D) stop("invalid d given D")
  B <- diag(D)
  if (!inv){
    B[d,] <- rep(-1, D)
  } else {
    B[d,] <- rep(0, D)
  }
  B[,-d]
}

#' @rdname base_lr_transforms
#' @export
alr <- function(x, d=NULL){
  x <- vec_to_mat(x)
  if (is.null(d)) d <- ncol(x)
  B <- create_alr_base(ncol(x), d, inv=FALSE)
  glr(x, B)
}

#' @rdname base_lr_transforms
#' @export
alrInv <- function(y, d=NULL){
  y <- vec_to_mat(y)
  if (is.null(d)) d <- ncol(y)+1
  B <- create_alr_base(ncol(y)+1, d, inv=TRUE)
  glrInv(y, B)
}

#' @rdname base_lr_transforms
#' @export
create_default_ilr_base <- function(D){
  qr.Q(qr(create_alr_base(D, D)))
}

#' @rdname base_lr_transforms
#' @export
ilr <- function(x, V=NULL){
  x <- vec_to_mat(x)
  if (is.null(V)) V <- create_default_ilr_base(ncol(x))
  glr(x, V)
}

#' @rdname base_lr_transforms
#' @export
ilrInv <- function(y, V=NULL){
  y <- vec_to_mat(y)
  if (is.null(V)) V <- create_default_ilr_base(ncol(y)+1)
  glrInv(y, V)
}

#' @rdname base_lr_transforms
#' @export
create_clr_base <- function(D, inv=FALSE){
  if(!inv){
    M <- matrix(-1, D, D) + D*diag(D)
    return(M/D)
  } else {
    return(diag(D))
  }
}

#' @rdname base_lr_transforms
#' @export
clr <- function(x){
  x <- vec_to_mat(x)
  glr(x, create_clr_base(ncol(x)))
}

#' @rdname base_lr_transforms
#' @export
clrInv <- function(y){
  miniclo(exp(y))
}

