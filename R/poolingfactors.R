#' Pooling Factors for a Multilevel Model
#'
#' @description
#' `calc_poolfac()` calculates the pooling factors
#' according to Gelman and Pardoe (2006)
#' from a `CmdStanMCMC` object
#'
#' @param y A numeric vector with the response variable
#' @param draws_mat A `draws_matrix` with the relevant pooling parameters
#' @param pars A character vector with the names of the relevant residuals
#'   of the multilevel model
#' @return A named vector containing the pooling factors
#' @details
#' A `draws_matrix` is a numeric matrix
#' in which the rows are posterior draws
#' and the columns are variables.
#' @source
#' Gelman, A., & Pardoe, I. (2006).
#' Bayesian measures of explained variance and pooling
#' in multilevel (hierarchical) models.
#' *Technometrics, 48 (2), 241–251.*
#' <https://doi.org/10.1198/004017005000000517>.
calc_poolstats <- function(
                    y,
                    draws_mat,
                    pars = c("gamma", "delta", "y")
                    ) {
  n_pars <- length(pars)
  n_draws <- nrow(draws_mat)
  draws_epsilon <- lapply(paste0("epsilon_", pars),
    function(i) draws_mat[, grep(i, colnames(draws_mat))])
  draws_theta <- lapply(paste0(pars, "_raw")[-n_pars],
    function(i) draws_mat[, grep(i, colnames(draws_mat))])
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
