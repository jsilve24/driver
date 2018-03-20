
#' Calculate Equilvalent Log Fold Change for Given Change in Evidence Information
#'
#' @param x Evidence Information Units
#' @param r Number of parts in numerator of log-ratio
#' @param s number of parts in denominator of log-ratio
#'
#' @details EI is defined on log-ratio scale and for a given balance can be
#' derived as \deqn{y = sqrt(rs/(r+s))log(Gm(X^+)/Gm(X^-))}
#' @return scalar
#' @export
#'
#' @examples
#' eu2logfoldchange(1)
eu2logfoldchange <- function(x, r=1, s=1){
  exp(x/(sqrt(r*s/(r+s))))
}
