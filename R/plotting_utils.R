#' Vector to lower diagonal of matrix formed from factor by factor
#'
#' Formed by unique(vetor) %times% unique(vector)
#'
#' Useful for creating heatmaps using only lower diagonal.
#'
#' @param a vector
#' @param VarNames character vector of length 2
#'
#' @return data.frame with two columns named according to \code{VarNames}
#' @export
#'
#' @examples
#' lower_triangle_factors(rep(1:3, 10))
lower_triangle_factors <- function(a, VarNames=c("Var1", "Var2")){
  if(!is.factor(a)) a <- factor(a)
  l <- levels(a)
  z <- matrix(0, length(l), length(l))
  r <- row(z)[lower.tri(z)]
  c <- col(z)[lower.tri(z)]

  idx <- cbind(l[r],l[c])
  colnames(idx) <- VarNames
  idx <- as.data.frame(idx)
  if (is.factor(a)){
    idx[[VarNames[1]]] <- factor(idx[[VarNames[1]]], levels=l)
    idx[[VarNames[2]]] <- factor(idx[[VarNames[2]]], levels=l)
  }
  idx
}
