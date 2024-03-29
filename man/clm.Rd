% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conjugate_samplers.R
\name{clm}
\alias{clm}
\title{Conjugate Linear model}
\usage{
clm(y, X, mu0, Lambda0, a0, b0, calc_ml = TRUE)
}
\arguments{
\item{y}{k x 1 vector}

\item{X}{n x k matrix}

\item{mu0}{k x 1}

\item{Lambda0}{k x k}

\item{a0}{scalar}

\item{b0}{scalar}

\item{calc_lml}{if TRUE also returns model evidence (log marginal likelihood)}
}
\value{
list with elements muN, LambdaN, aN, bN, lml
}
\description{
Conjugate Linear model
}
\details{
Model:
\deqn{y_i ~ N(x_i^T \beta, \sigma^2)}
\deqn{\beta ~ N(\mu0, \sigma^2 \Lambda0)}
\deqn{\sigma^2 ~ IG(a0, b0)}

from https://en.wikipedia.org/wiki/Bayesian_linear_regression
}
