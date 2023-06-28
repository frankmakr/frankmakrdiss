#' Pairwise Relative AIC Comparison
#'
#' @description
#' `tabulate_paic()` calculates the probability
#' that one model minimizes the information loss over another model
#' and displays the results in a table.
#' 
#' @param vec_aic A numeric vector with AIC values
#' @return A numeric matrix with pairwise comparisons
#'   containing the probability minimizing the information loss
#' @export
tabulate_paic <- function(vec_aic) {
  stopifnot("vec_aic must be numeric" = is.numeric(vec_aic))
  min_aic <- which(vec_aic == min(vec_aic))
  delta_aic <- outer(vec_aic, vec_aic, "-")
  odds <- exp(delta_aic / 2)
  p <- odds / (odds + 1)
  res <- list(p_delta_aic = zapsmall(p),
              p_delta_aic_min = zapsmall(p[min_aic, ]))
  return(res)
}
