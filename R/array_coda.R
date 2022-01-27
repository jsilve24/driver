# Main Helper Function ----------------------------------------------------

#' Apply Function to 1 dimension of array along all others
#'
#' Used by a number of functions in the array_coda group.
#'
#' @param a input array
#' @param dim dimension (as integer) along which to apply matrix operation
#' @param f function to apply to matrix that returns a matrix
#' @param dimname (optional) charachter vector for dimension output from matrix operation
#'
#' @details If the matrix operation does not change the dimension of the array
#' and dimnames is null, uses existing dimnames.
#'
#' @return array
#' @export
#'
#' @importFrom rlang sym syms
#'
#' @examples
#' a <- array(runif(600), dim = c(100, 3, 2))
#' array_apply_1D_function(a, 2, miniclo)
array_apply_1D_function <- function(a, dimno, f, dimname=NULL){

  d <- dim(a)
  ndim <- length(d)
  sdim <- sym(paste0("dim_", dimno))
  sdim_other <- syms(paste0("dim_", (1:ndim)[(1:ndim) != dimno]))

  # Store Dimnames
  dn <- dimnames(a)

  # Acctual Computation
  ga <- a %>%
    gather_array(var) %>%
    spread(!!sdim, var)

  indicies <- ga %>%
     select(!!!sdim_other)

  b <- ga %>%
    select(-contains("dim")) %>%
    as.matrix() %>%
    f %>%
    `colnames<-`(., 1:ncol(.)) %>%
    as.data.frame() %>%
    bind_cols(indicies, .) %>%
    gather(!!sdim, var, -contains("dim")) %>%
    mutate(!!quo_name(sdim)  := as.integer(!!sdim)) %>%
    spread_array(var, !!!syms(paste0("dim_", 1:ndim)))

  # Update Dimnames
  if (!is.null(dn) || !is.null(dimname)){
    if (!is.null(dn)){
      dn[dimno] <- list(NULL)
    } else {
      dn <- list()
      for (i in 1:length(d)) dn[[i]] <- NULL
    }
    if (!is.null(dimname)){
      dn[[dimno]] <- dimname
    }
    dimnames(b) <- dn
  } else {
    names(dim(b)) <- NULL
  }
  return(b)
}


# Coda Array Functions ----------------------------------------------------


#' Closure Operation applied to array on margin
#'
#' Array version of \code{\link{miniclo}}.
#'
#' @param x multidimensional array
#' @param parts index of dimension of \code{x} that represents parts (e.g., compositional variables)
#'
#' @return array
#' @export
#'
#' @examples
#' x <- array(1:100, dim=c(10, 5, 2))
#' miniclo_array(x, parts=2)
miniclo_array <- function(x, parts){
  array_apply_1D_function(x, parts, miniclo)
}


#' Log-Ratios Transforms for Arrays
#'
#' Extension of \code{\link{base_lr_transforms}} to arrays over arbitrary
#' margins.
#'
#' @inheritParams base_lr_transforms
#' @param x multidimensional array in simplex
#' @param y multidimensional array in transformed space
#' @param parts index of dimension of \code{x} that represents parts (e.g., compositional variables)
#' @param coords index of dimension of \code{x} that represents coords (e.g., transformed variables)
#' @param dimname character vector of dimension name for resulting dimension after transformation
#'
#' @return array
#' @name array_lr_transforms
#'
#' @details accelerated for parts OR coords == 1
#'
#' @examples
#' a <- array(1:100, dim=c(10, 5, 2))
#' a <- miniclo_array(a, parts=2)
#' clr_array(a, parts=2)
NULL


#' @rdname array_lr_transforms
#' @export
glr_array <- function(x, V, parts, dimname = colnames(V)){
  if (parts == 1){
    dn <- dimnames(x)
    d <- dim(x)
    x <- matrix(x, d[1], prod(d[-1]))
    x <- t(V) %*% log(x)
    d[1] <- ncol(V)
    dim(x) <- d
    if (!is.null(dn)){
      if (!is.null(dimname)){
        dn[[1]] <- dimname
      } else {
        dn[1] <- list(NULL)
      }
        dimnames(x) <- dn
    }
    return(x)
  } else {
    f <- function(x) glr(x, V)
    return(array_apply_1D_function(x, parts, f, dimname))
  }
}

#' @rdname array_lr_transforms
#' @export
glrInv_array <- function(y, V, coords, dimname = rownames(V)){
  if (coords==1){
    dn <- dimnames(y)
    d <- dim(y)
    y <- matrix(y, d[1], prod(d[-1]))
    y <- exp(V %*% y)
    y <- t(miniclo(t(y)))
    d[1] <- nrow(V)
    dim(y) <- d
    if (!is.null(dn)){
      if (!is.null(dimname)) { dn[[1]] <- dimname } else {dn[1] <- list(NULL)}
      dimnames(y) <- dn
    }
    return(y)
  } else {
    f <- function(y) glrInv(y, V)
    return(array_apply_1D_function(y, coords, f, dimname))
  }
}

#' @rdname array_lr_transforms
#' @export
alr_array <- function(x, d=dim(x)[parts], parts){
  B <- create_alr_base(dim(x)[parts], d, inv=FALSE)
  glr_array(x, B, parts)
}

#' @rdname array_lr_transforms
#' @export
alrInv_array <- function(y, d=dim(y)[coords]+1, coords){
  B <- create_alr_base(dim(y)[coords]+1, d, inv=TRUE)
  glrInv_array(y, B, coords)
}

#' @rdname array_lr_transforms
#' @export
ilr_array <- function(x, V=NULL, parts){
  n.parts <- dim(x)[parts]
  if (is.null(V)) V <- create_default_ilr_base(n.parts)
  glr_array(x, V, parts)
}

#' @rdname array_lr_transforms
#' @export
ilrInv_array <- function(y, V=NULL, coords){
  n.coords <- dim(y)[coords]
  if (is.null(V)) V <- qr.Q(qr(create_alr_base(n.coords+1, n.coords+1)))
  glrInv_array(y, V, coords)
}

#' @rdname array_lr_transforms
#' @export
clr_array <- function(x, parts){
  n.parts <- dim(x)[parts]
  V <- create_clr_base(n.parts)
  glr_array(x, V, parts)
}

#' @rdname array_lr_transforms
#' @export
clrInv_array <- function(y, coords){
  n.coords <- dim(y)[coords]
  V <- diag(n.coords) # Not efficient but reuses code...
  glrInv_array(y, V, coords)
}





