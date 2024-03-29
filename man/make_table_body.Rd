% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tablebody.R
\name{make_table_body}
\alias{make_table_body}
\title{LaTeX Tabular Body}
\usage{
make_table_body(
  data,
  space_after = 5,
  digits = 4,
  cov_tab = FALSE,
  row_names = TRUE
)
}
\arguments{
\item{data}{A named numeric \code{matrix}
or a \code{data.frame} with at least one numeric column containing the table data}

\item{space_after}{A numeric vector or single number
indicating the number of rows after which additional space is inserted
which defaults to \code{space_after = 5}}

\item{digits}{The number of digits to be displayed in the table}

\item{cov_tab}{A logical indicator for a covariance table
which defaults to \code{cov_tab = FALSE}}

\item{row_names}{A logical indicator for row names
which defaults to \code{row_names = TRUE}}
}
\value{
The LaTeX table body
}
\description{
\code{make_table_body()} strips the table body
from a \code{knitr::kable()} table
}
