glr_array <- function(x, V, samples = 1, parts = 2){
  dims <- dim(x)
  split.margin <- (1:length(dims))[!(1:length(dims) %in% c(samples, parts))]
  dims.new <- dims
  dims.new[parts] <- ncol(V)

  x.out <- array(NA, dim=dims.new)
  x.out[] <- apply(x, MARGIN = c(split.margin), FUN = glr, V)
  if (!is.null(dimnames(V)))dimnames(x.out)[[parts]] <- colnames(V)   # Name output
  x.out
}

glrInv_array <- function(y, V, samples = 1, coords = 2){
  dims <- dim(y)
  split.margin <- (1:length(dims))[!(1:length(dims) %in% c(samples, coords))]
  dims.new <- dims
  dims.new[coords] <- nrow(V)

  y.out <- array(NA, dim = dims.new)
  y.out[] <- apply(y, MARGIN = c(split.margin), FUN = glrInv, V)
  if (!is.null(dimnames(V)))dimnames(y.out)[[coords]] <- rownames(V)   # Name output
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
  n.parts <- dim(x)[parts]
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
  glrInv_array(y, V, samples, coords)
}
