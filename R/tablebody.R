#' LaTeX Tabular Row Seperation
#'
#' Utility function
#'
#' @param x The argument \code{space.after} from \code{make_table_body}
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
#' \code{make_table_body} strips the table body
#' from a \code{knitr::kable} table
#' 
#' @param data A named matrix or data.frame containing the table data
#' @param space.after A numeric vector or single number
#'   indicating the number of rows after which row additional space is inserted
#' @param digits The number of digits to be displayed in the table
#' @param cor.tab Is the table a correlation table
#' @param row.names Does the data contain row.names
#' @return The LaTeX table body
#' @export
make_table_body <- function(data,
                            space.after = 5,
                            digits = 4,
                            cor.tab = FALSE,
                            row.names = TRUE) {
  if (is.data.frame(data)) { 
    numerics <- unlist(lapply(data, is.numeric))
    if (sum(numerics) > 0) {
    data[, numerics] <- format(round(data[, numerics],
      digits = digits), scientific = FALSE)
    }
    else stop("The data.frame has no numeric columns.")
  }
  if (is.matrix(data)) {
    if (is.numeric(data)) {
    data <- format(round(data, digits = digits), scientific = FALSE) 
    }
    else stop("The matrix is not numeric.")
  }
  if (cor.tab == TRUE) {
    data[upper.tri(data, diag = TRUE)] <- ""
    data <- data[-1, -nrow(data)]
  }
  table <- knitr::kable(data,
    format = "latex", booktabs = TRUE, linesep = sep_after(space.after),
    row.names = row.names, escape = FALSE)
  table_body <- strsplit(table, "midrule\n|\n\\\\bottomrule")[[1]][2]
  attributes(table_body) <- list(format = "latex", class = "knitr_kable")
  return(table_body)
}
