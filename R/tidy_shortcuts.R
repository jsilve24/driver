#' Shortcut for summarize variable with quantiles and mean
#'
#' @param data tidy data frame
#' @param var variable name (unquoted) to be summarised
#' @param ... other expressions to pass to summarise
#'
#' @return data.frame
#' @export
#' @import dplyr
#' @importFrom stats quantile
#' @importFrom rlang quos quo UQ
#' @examples
#' # library(dplyr)
#' # d <- data.frame("a" = c(1:10), "b"=rep(c(1,2), 5)) %>%
#' #   summarise_posterior(a, mean.b = mean(b))
summarise_posterior <- function(data, var, ...){
  qvar <- enquo(var)
  qs <- quos(...)


  data %>%
    summarise(p2.5 = quantile(!!qvar, prob=0.025),
              p25 = quantile(!!qvar, prob=0.25),
              p50 = quantile(!!qvar, prob=0.5),
              mean = mean(!!qvar),
              p75 = quantile(!!qvar, prob=0.75),
              p97.5 = quantile(!!qvar, prob=0.975),
              !!!qs)
}

