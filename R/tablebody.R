#' LaTeX Tabular Row Seperation
#'
#' Utility function
#'
#' @param x The argument `space.after` from `make_table_body()`
#' @param y A necessary dummy variable
#' @noRd
sep_after <- function(x, y = character()) {
  if (!length(x)) {
    return(y)
  }
  sep_after(x[-length(x)], c(rep("", x[length(x)] - 1), "\\addlinespace", y)) 
}

#' LaTeX Tabular Body
#'
#' @description
#' `make_table_body()` strips the table body
#' from a `knitr::kable()` table
#' 
#' @param data A named numeric `matrix`
#'   or a `data.frame` with at least one numeric column containing the table data
#' @param space_after A numeric vector or single number
#'   indicating the number of rows after which additional space is inserted
#'   which defaults to `space_after = 5`
#' @param digits The number of digits to be displayed in the table
#' @param cov_tab A logical indicator for a covariance table
#'   which defaults to `cov_tab = FALSE`
#' @param row_names A logical indicator for row names
#'   which defaults to `row_names = TRUE`
#' @return The LaTeX table body
#' @export
make_table_body <- function(data,
                            space_after = 5,
                            digits = 4,
                            cov_tab = FALSE,
                            row_names = TRUE) {
  if (is.data.frame(data)) { 
    numerics <- unlist(lapply(data, is.numeric))
    if (sum(numerics) > 0) {
    data[, numerics] <- format(round(data[, numerics],
      digits = digits), scientific = FALSE)
    }
    else stop("The data frame has no numeric columns.")
  }
  if (is.matrix(data)) {
    if (is.numeric(data)) {
    data <- format(round(data, digits = digits), scientific = FALSE) 
    }
    else stop("The matrix is not numeric.")
  }
  if (cov_tab == TRUE) {
    data[upper.tri(data, diag = TRUE)] <- ""
    data <- data[-1, -nrow(data)]
  }
  table <- knitr::kable(data,
    format = "latex", booktabs = TRUE, linesep = sep_after(space_after),
    row.names = row_names, escape = FALSE)
  table_body <- strsplit(table, "midrule\n|\n\\\\bottomrule")[[1]][2]
  attributes(table_body) <- list(format = "latex", class = "knitr_kable")
  return(table_body)
}
