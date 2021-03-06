% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tiny_coda.R
\name{base_lr_transforms}
\alias{base_lr_transforms}
\alias{glr}
\alias{glrInv}
\alias{create_alr_base}
\alias{alr}
\alias{alrInv}
\alias{create_default_ilr_base}
\alias{ilr}
\alias{ilrInv}
\alias{create_clr_base}
\alias{clr}
\alias{clrInv}
\title{Log-Ratio Transformation}
\usage{
glr(x, V)

glrInv(y, V)

create_alr_base(D, d, inv = FALSE)

alr(x, d = NULL)

alrInv(y, d = NULL)

create_default_ilr_base(D)

ilr(x, V = NULL)

ilrInv(y, V = NULL)

create_clr_base(D, inv = FALSE)

clr(x)

clrInv(y)
}
\arguments{
\item{x}{vector or matrix (rows are samples, parts are columns) of data in simplex}

\item{V}{transformation matrix (defines transform)}

\item{y}{matrix (rows are samples, coords are columns) of transformed data}

\item{D}{the number of parts (e.g., number of columns in untransformed data)}

\item{d}{for ALR, which component (integer position) to take as reference
(default is ncol(x)) for alrInv corresponds to column position in untransformed
matrix.}

\item{inv}{for ALR and CLR, transformation matrix is different forward and inverse}
}
\value{
matrix (converts vectors to row matricies)
}
\description{
\code{glr} is generic log-ratio transform, code used by other
transforms, can be called directly.
}
\details{
The implementation of the ILR transform here relies on the fact that
all the standard log-ratio transforms can be written in the following form
\deqn{y=\log(x)V} with inverse transform given by
\deqn{x=\mathcal{C}[exp(yV^t)]} where \eqn{\mathcal{C}[\cdot]} is the closure operator (\code{\link{miniclo}}). Note however that if \eqn{V} does not represent an orthonormal
basis in the Aitchison geometry then the \eqn{V} used for the log-ratio transform may be different
than the one used for the reverse transform (this is the case for the ALR and CLR transforms).
Default ILR base formed by Gram-Schmidt orthogonalization of an ALR basis.
}
\examples{
#ALR Transform
x <- matrix(runif(30), 10, 3)
x <- miniclo(x)
x.alr <- alr(x, 2)
x <- alrInv(x.alr, 2)

# ILR
x.ilr <- ilr(x)
x <- ilrInv(x.ilr)

# CLR
x.clr <- clr(x)
x <- clrInv(x.clr)

# CUSTOM - Be careful if your custom matrix is not
# orthogonal the inverse transform may not be given by just the transpose!
# For example, this is the case for the ALR
V <- matrix(c(1, 1, -1), 3, 1)
x.custom <- glr(x, V)
}
