#' Example function for cmdstanr integration
#'
#' Test function
#'
#' @param stan_data The data used for the model
#' @return The function returns a stan fit object.
#' @export
fit_test <- function(stan_data) {
  stan_file <- system.file("beta_binomial.stan", package = "frankmakrdiss")
  stan_mod <- cmdstanr::cmdstan_model(stan_file)
  stan_fit <- stan_mod$sample(data = stan_data)
  return(stan_fit)
}
