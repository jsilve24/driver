#' Convert CoDA covariance matricies between representations
#'
#' \code{ilrvar}, \code{clrvar}, and \code{varmat} (variation matrix).
#' \code{ilrvar2phi} calculates phi statistics (for proportionality)
#' from an ILR covariance matrix as described in Lovell (2015).
#'
#' @param Sigma covariance matrix in specified transformed space
#' @param V ILR contrast matrix (i.e., transformation matrix of ILR)
#' @param V1 ILR contrast matrix of basis Sigma is already in
#' @param V2 ILR contrast matrix of basis Sigma is desired in
#'
#' @return matrix
#' @name convert_coda_covariance
#' @examples
#' x <- matrix(runif(30),  10, 3)
#' x <- miniclo(x)
#' x.ilr <- ilr(x)
#' V <- create_default_ilr_base(3)
#' Sigma <- cov(x.ilr)
#' Sigma.clr <- ilrvar2clrvar(Sigma, V)
#' clrvar2ilrvar(Sigma.clr, V)
#' clrvar2varmat(Sigma.clr)
#' ilrvar2varmat(Sigma, V)
#' ilrvar2phi(Sigma,V)
NULL

#' @rdname convert_coda_covariance
#' @export
ilrvar2phi <- function(Sigma, V){
  Sigma.clr <- ilrvar2clrvar(Sigma,V)
  phi <- Sigma.clr
  phi[] <- 0
  for(i in 1:dim(Sigma.clr)[1]){
    for (j in 1:dim(Sigma.clr)[2]){
      phi[i,j] <- Sigma.clr[i,i] + Sigma.clr[j,j] - 2*Sigma.clr[i,j]
    }
  }
  return(phi)
}


#' @rdname convert_coda_covariance
#' @export
ilrvar2ilrvar <- function(Sigma, V1, V2){
   t(V2) %*% V1 %*% Sigma %*% t(V1) %*% V2
}


#' @rdname convert_coda_covariance
#' @export
ilrvar2clrvar <- function(Sigma, V){
  V %*% Sigma %*% t(V)
}
#' @rdname convert_coda_covariance
#' @export
clrvar2ilrvar <- function(Sigma, V){
  t(V) %*% Sigma %*% V
}
#' @rdname convert_coda_covariance
#' @export
clrvar2varmat <- function(Sigma){
  varmat <- matrix(0, nrow(Sigma), ncol(Sigma))
  for (i in 1:dim(Sigma)[1]){
    for (j in 1:dim(Sigma)[2]){
      varmat[i,j] <- Sigma[i,i] + Sigma[j,j] - 2*Sigma[i,j]
    }
  }
}
#' @rdname convert_coda_covariance
#' @export
ilrvar2varmat <- function(Sigma, V){
  Sigma <- ilrvar2clrvar(Sigma, V)
  clrvar2varmat(Sigma)
}


