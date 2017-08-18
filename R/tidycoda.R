# CODA Functions ----------------------------------------------------------

# generic functions
miniclo <- function(x){
  if (is.vector(x)) x <- matrix(x, nrow = 1)
  (x/rowSums(x))
}

vec_to_mat <- function(x){
  if (is.vector(x)) return(matrix(x, nrow = 1))
  x
}

# BASE CODA
# generic log-ratio transform
glr <- function(x, V){
  log(x) %*% V
}

# generic log-ratio transform - Inverse
glrInv <- function(y, V){
  tmp <- exp(y %*% t(V))
  miniclo(tmp)
}

# ALR
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

alr <- function(x, d=NULL){
  x <- vec_to_mat(x)
  if (is.null(d)) d <- ncol(x)
  B <- create_alr_base(ncol(x), d, inv=FALSE)
  glr(x, B)
}

alrInv <- function(y, d=NULL){
  y <- vec_to_mat(y)
  if (is.null(d)) d <- ncol(y)+1
  B <- create_alr_base(ncol(y)+1, d, inv=TRUE)
  glrInv(y, B)
}

# ILR
ilr <- function(x, V=NULL){
  x <- vec_to_mat(x)
  if (is.null(V)) V <- qr.Q(qr(create_alr_base(ncol(x), ncol(x))))
  glr(x, V)
}

ilrInv <- function(y, V=NULL){
  y <- vec_to_mat(y)
  if (is.null(V)) V <- qr.Q(qr(create_alr_base(ncol(y)+1, ncol(y)+1)))
  glrInv(y, V)
}

# CLR
create_clr_base <- function(D){
  M <- matrix(-1, D, D) + D*diag(D)
  M/D
}

clr <- function(x){
  x <- vec_to_mat(x)
  glr(x, create_clr_base(ncol(x)))
}

clrInv <- function(y){
  miniclo(exp(y))
}

# CODA Array
glr_array <- function(x, V, samples = 1, parts = 2){
  dims <- dim(x)
  split.margin <- (1:length(dims))[!(1:length(dims) %in% c(samples, parts))]
  dims.new <- dims
  #dims.new[parts] <- dims.new[parts]-1
  dims.new[parts] <- ncol(V)

  x.out <- array(NA, dim=dims.new)
  x.out[] <- apply(x, MARGIN = c(split.margin), FUN = glr, V)
  x.out
}

glrInv_array <- function(y, V, samples = 1, coords = 2){
  dims <- dim(y)
  split.margin <- (1:length(dims))[!(1:length(dims) %in% c(samples, coords))]
  dims.new <- dims
  #dims.new[coords] <- dims.new[coords] +1
  dims.new[coords] <- nrow(V)

  y.out <- array(NA, dim = dims.new)
  y.out[] <- apply(y, MARGIN = c(split.margin), FUN = glrInv, V)
  y.out
}

alr_array <- function(x, d=dim(x)[parts], samples=1, parts=2){
  B <- create_alr_base(dim(x)[parts], d, inv=FALSE)
  glr_array(x, B, samples, parts)
}

alrInv_array <- function(y, d=dim(y)[coords]+1, samples=1, coords=2){
  B <- create_alr_base(dim(y)[coords]+1, d, inv=TRUE)
  glrInv_array(y, B, samples, coords)
}


ilr_array <- function(x, V=NULL, samples = 1, parts = 2){
  n.parts <- dim(y)[parts]
  if (is.null(V)) V <- qr.Q(qr(create_alr_base(n.parts, n.parts)))
  glr_array(x, V, samples, parts)
}

ilrInv_array <- function(y, V=NULL, samples=1, coords=2){
  n.coords <- dim(y)[coords]
  if (is.null(V)) V <- qr.Q(qr(create_alr_base(n.coords+1, n.coords+1)))
  glrInv_array(y, V, samples, coords)
}

clr_array <- function(x, samples = 1, parts = 2){
  n.parts <- dim(x)[parts]
  V <- create_clr_base(n.parts)
  glr_array(x, V, samples, parts)
}

clrInv_array <- function(y, samples = 1, coords = 2){
  n.coords <- dim(y)[coords]
  V <- diag(n.coords) # Not efficient but reuses code...
  glrInv_array(y, V, samples, parts)
}


# Other Functions ---------------------------------------------------------

summarise_posterior <- function(data, var, ...){
  qvar <- quo(var)
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

