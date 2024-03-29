% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot2data.R
\name{make_hypopointrange}
\alias{make_hypopointrange}
\title{Plot Data for Testing the Hypotheses}
\usage{
make_hypopointrange(draws_mat, label_list, probs = 0.87)
}
\arguments{
\item{draws_mat}{A \code{draws_matrix} containing the relevant effects}

\item{label_list}{A nested \code{list} containing the names for the y axis
nested in the names for the color groups}

\item{probs}{The probability for the highest density region
which defaults to \code{probs = 0.87}}
}
\value{
A data frame in the format for
using the corresponding \code{plot_hypopointrange()} function
}
\description{
\code{make_hypopointrange()} prepares the data
in the format for the function \code{plot_hypopointrange()}.
}
\details{
A \code{draws_matrix} is a numeric matrix
in which the rows are posterior draws
and the columns are variables.
}
