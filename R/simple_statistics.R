#' Mode of a vector x
#'
#' @param x vector of values
#'
#' @return vector (can return multiple maxima)
#' @export
#'
#' @examples
#' dmode(c(2,3,34,2,2))
dmode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[which( tab == max(tab) )]
}
