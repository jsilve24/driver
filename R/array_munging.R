#' Gather Multidimensional Array to Tidy Tibble
#'
#' @param a multidimensional array
#' @param value unquoted name of column with values (defaults to "var")
#' @param ... unquoted dimension names (defaults to "dim_1", "dim_2", etc...)
#' @param .id if specified, name for column created with name of a captured
#'
#' @return data.frame
#' @seealso spread_array
#' @export
#' @import dplyr, purrr, tidyr
#' @importFrom rlang quos enquo quo_name sym syms
#'
#' @examples
#' a <- array(1:100, dim =c(10, 5, 2))
#' gather_array(a, sequence, A, B, C)
gather_array <- function(a, value, ..., .id=NULL){
  qs <- rlang::quos(...)
  if (missing(value)) {
    evalue <- rlang::sym("var")}
  else {
    evalue <- rlang::enquo(value)
  }
  len <- length(qs)
  d <- dim(a)

  # Default Values
  if (len > 0) {
    dimnames <- purrr::map(qs, rlang::quo_name) %>%
      as_vector()
  } else {
    dimnames <- paste0("dim_", 1:length(d))
  }

  l <- list()
  for (i in 1:length(d)){
    l[[i]] <- 1:d[i]
  }
  names(l) <- dimnames
  tidy <- expand.grid(l)
  tidy[[rlang::quo_name(evalue)]] <- a[as.matrix(tidy)]
  if (!is.null(.id)) tidy[[.id]] <- rlang::expr_name(a)
  return(tidy)
}

#' Spread data.frame
#'
#' @param data a dataframe with integer indexed positions in an array
#' @param value data to be spread into the array
#' @param ... unquoted dimnames used to build array
#'
#' @details make sure all colnames supplied to ... are consecutive interger indexed positions
#' @return multidimensional array
#' @seealso gather_array
#' @export
#' @import dplyr, purrr, tidyr
#' @importFrom rlang quos enquo quo_name sym syms
#'
#' @examples
#' a <- array(1:100, dim=c(10, 5, 2))
#' ga <- gather_array(a, sequence, A, B, C)
#' head(ga)
#' spread_array(ga, sequence, A, B, C)
spread_array <- function(data, value, ...){
  evalue <- rlang::enquo(value)
  qs <- rlang::quos(...)
  l <- length(qs)

  # Default Values
  if (l == 0) {
    cn <- colnames(data)
    cn <- cn[grepl("dim_", cn)]
    # Validation of Defaults
    consecutive1 <- strsplit(cn, "_") %>%
      purrr::map(~.x[2]) %>%
      purrr::map(as.integer) %>%
      as_vector()
    if (!setequal(consecutive1, 1:length(consecutive1))) {
      stop("default dimnetion names must have consecutive integer suffixes")
    }
    ##
    cn <- cn[match(consecutive1, 1:length(consecutive1))]
    qs <- rlang::syms(cn)
  }
  if (missing(value)) evalue <- rlang::sym("var")

  tidy_dim <- data %>%
    select(!!!qs)
  unique_dim <- tidy_dim %>%
    as.list() %>%
    purrr::map(unique)
  length_dim <- unique_dim %>%
    purrr::map(length)

  # Input validation - Must be sequential integers
  class_dim <- data %>%
    select(!!!qs) %>%
    sapply(class)
  if (!all(class_dim=="integer")) stop("Dimension indexes must be integers")

  consecutive2 <- unique_dim %>%
    map2(map(length_dim, ~1:.x), setequal) %>%
    as_vector() %>%
    all()
  if (!consecutive2) stop("Dimension indexes must be consecutive")
  #####

  a <- array(NA, dim = length_dim)
  a[as.matrix(tidy_dim)] <- pull(data, rlang::quo_name(evalue))

  return(a)
}
