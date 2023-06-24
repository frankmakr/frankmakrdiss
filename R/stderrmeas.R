#' Standard Error of Measurement
#' 
#' Utility function
#' 
#' @param sd A numeric vector of standard deviations of measurement scales
#' @param rel A numeric vector of reliability coefficients of measurement scales
#' @return A numeric vector of equal length to input 
#' @noRd
calc_sem <- function(sd, rel) {
  sem <- sd * sqrt(1 - rel)
  return(sem)
}

#' Standard Error of Measurement and Confidence Intervals
#'
#' @description
#' `calc_semci()` returns the standard error of measurement
#' from a given reliability and the corresponding standard deviation.
#' The confidence intervals are calculated using
#' a normal or a student t distribution,
#' or according to the Tschebyschow inequality for an unknown distribution.
#'
#' @param data A numeric vector containing the measured values
#' @param sd A numeric vector of standard deviations of measurement scales
#' @param rel A numeric vector of reliability coefficients of measurement scales
#' @param se0 The number of standard errors from 0 which are regarded
#'   as the critical value for the confidence interval
#'   which defaults to `se0 = 2.8`
#' @param p_ci The confidence niveau in percent
#'   which defaults to `p_ci = NA'
#' @param dist One of `"normal"`, `"t"`, or `"none"`
#' @param ... The number of degrees of freedom for the student t distribution
#' @return A data.frame containing the inputs,
#'   the standard error of measurement, and the lower and upper bounds
#'   of the confidence interval
#' @details One of the arguments `se0` or `p_ci` must be set to a value.
#'  The other must be set to `NA`.
#' @export
calc_semci <- function(data, sd, rel,
                       se0 = 2.8,
                       p_ci = NA,
                       dist = "none",
                       ...) {
  args <- list(...)
  sem <- calc_sem(sd, rel)
  # Tschebyschow inequality
  if(dist == "none") {
    if(!is.na(se0)) {
      p_ci <- 1 - 1 / se0^2
    } else {
      se0 <- sqrt(1 / (1 - p_ci))
    }
  # z or t distribution
  } else {
    df <- ifelse(dist == "z", Inf, args$df)
    if(!is.na(se0)) { 
      p_ci <- 1 - (1 - stats::pt(se0, df)) * 2
    } else {
      se0 <- stats::qt(1 - ((1 - p_ci) / 2), df)
    }
  } 
  res <- data.frame(value = data, sd = sd, rel = rel, sem = sem, se0 = se0,
    p_ci = p_ci, dist = ifelse(dist == "t", paste0(dist, ", df=", df), dist),
    ll = data - se0 * sem, ul = data + se0 * sem)
  return(res)
}
