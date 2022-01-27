#' Conjugate Linear model
#'
#' @param y k x 1 vector
#' @param X n x k matrix
#' @param mu0 k x 1
#' @param Lambda0 k x k
#' @param a0 scalar
#' @param b0 scalar
#' @param calc_lml if TRUE also returns model evidence (log marginal likelihood)
#'
#' @details
#'
#' Model:
#' \deqn{y_i ~ N(x_i^T \beta, \sigma^2)}
#' \deqn{\beta ~ N(\mu0, \sigma^2 \Lambda0)}
#' \deqn{\sigma^2 ~ IG(a0, b0)}
#'
#' from https://en.wikipedia.org/wiki/Bayesian_linear_regression
#'
#' @return list with elements muN, LambdaN, aN, bN, lml
#' @export
clm <- function(y,X,mu0,Lambda0,a0,b0, calc_ml=TRUE){
    X <- as.matrix(X)
    Lambda0 <- as.matrix(Lambda0)
    n <- nrow(X)
    y <- as.matrix(y)
    XTX <- crossprod(X)
    LambdaN <- as.matrix(XTX + Lambda0)
    muN <- solve(LambdaN) %*% (Lambda0 %*% mu0 + t(X)%*%y)
    aN <- a0 + n/2
    bN <- b0 + .5*(crossprod(y) + t(mu0)%*% Lambda0 %*% mu0 - t(muN) %*% LambdaN %*% muN)

    lml <- -n/2*log(2*pi) + .5*(log(det(Lambda0))-log(det(LambdaN)))
           + a0*log(b0) - aN-log(bN) + log(gamma(aN)) - log(gamma(a0))

    return(list(muN = muN, LambdaN = LambdaN, aN=aN, bN=bN, lml=lml))
}
