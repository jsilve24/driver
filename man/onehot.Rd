% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unsorted.R
\name{onehot}
\alias{onehot}
\title{One Hot Encoding of vector}
\usage{
onehot(x)
}
\arguments{
\item{x}{vector with D possible categorical values (will be coerced to factor)}
}
\value{
matrix with D columns and same number of columns as length of x
}
\description{
One Hot Encoding of vector
}
\examples{
onehot(1:5)
}
