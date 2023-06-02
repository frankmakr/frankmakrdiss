#' Calculation of the pooling factors for a multilevel model
#'
#' \code{calc_poolfac} calculates the pooling factors
#' according to Gelman and Pardoe (2006)
#' from a cmdstanr fit object
#'
#' @param y A vector with the response variable
#' @param draws A draws matrix with the relevant pooling parameters.
#' @param pars A character vector with the names of the relevant residuals
#'   of the multilevel model.
#' @return The output will be a vector containing the pooling factors.
#'
calc_poolstats <- function(
                    y,
                    draws,
                    pars = c("gamma", "delta", "y")
                    ) {
  n_pars <- length(pars)
  n_draws <- nrow(draws)
  draws_epsilon <- lapply(paste0("epsilon_", pars),
    function(i) draws[, grep(i, colnames(draws))])
  draws_theta <- lapply(paste0(pars, "_raw")[-n_pars],
    function(i) draws[, grep(i, colnames(draws))])
  e_v_theta <- vector(mode = "double", length = n_pars)
  e_v_theta[-n_pars] <- vapply(draws_theta, function(i)
    matrixStats::mean2(matrixStats::rowVars(i)), FUN.VALUE = n_pars - 1)
  e_v_theta[n_pars] <- stats::var(y)
  e_v_epsilon <- vapply(draws_epsilon, function(i)
    matrixStats::mean2(matrixStats::rowVars(i)), FUN.VALUE = n_pars + 0)
  v_e_epsilon <- vapply(draws_epsilon, function(i)
    stats::var(matrixStats::colMeans2(i)), FUN.VALUE = n_pars + 0)
  poolstats <- stats::setNames(vector(mode = "list", length = 2L),
    c("pooling_factor", "explained_variance"))
  poolstats[[1]] <- stats::setNames(
    1 - v_e_epsilon / e_v_epsilon,
    paste0("lambda_", pars))
  poolstats[[2]] <- stats::setNames(
    1 - e_v_epsilon / e_v_theta,
    paste0("R2_", pars))
  return(lapply(poolstats, round, digits = 3))
}
