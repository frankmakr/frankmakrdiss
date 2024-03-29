% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poolingfactors.R
\name{calc_poolstats}
\alias{calc_poolstats}
\title{Pooling Factors for a Multilevel Model}
\source{
Gelman, A., & Pardoe, I. (2006).
Bayesian measures of explained variance and pooling
in multilevel (hierarchical) models.
\emph{Technometrics, 48 (2), 241–251.}
\url{https://doi.org/10.1198/004017005000000517}.
}
\usage{
calc_poolstats(y, draws_mat, pars = c("gamma", "delta", "y"))
}
\arguments{
\item{y}{A numeric vector with the response variable}

\item{draws_mat}{A \code{draws_matrix} with the relevant pooling parameters}

\item{pars}{A character vector with the names of the relevant residuals
of the multilevel model}
}
\value{
A named vector containing the pooling factors
}
\description{
\code{calc_poolfac()} calculates the pooling factors
according to Gelman and Pardoe (2006)
from a \code{CmdStanMCMC} object
}
\details{
A \code{draws_matrix} is a numeric matrix
in which the rows are posterior draws
and the columns are variables.
}
