% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/array_coda.R
\name{array_apply_1D_function}
\alias{array_apply_1D_function}
\title{Apply Function to 1 dimension of array along all others}
\usage{
array_apply_1D_function(a, dimno, f, dimname = NULL)
}
\arguments{
\item{a}{input array}

\item{f}{function to apply to matrix that returns a matrix}

\item{dimname}{(optional) charachter vector for dimension output from matrix operation}

\item{dim}{dimension (as integer) along which to apply matrix operation}
}
\value{
array
}
\description{
Used by a number of functions in the array_coda group.
}
\details{
If the matrix operation does not change the dimension of the array
and dimnames is null, uses existing dimnames.
}
\examples{
a <- array(runif(600), dim = c(100, 3, 2))
array_apply_1D_function(a, 2, miniclo)
}
