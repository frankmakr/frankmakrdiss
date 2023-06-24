#' Fit Doctoral Dissertation's Stan Models
#'
#' @description
#' `stan_dissmodel()` fits the Stan models from the doctoral dissertation.
#'
#' @param model_number A character string containing the model number
#'   from the doctoral dissertation (i.e. "12.2")
#' @param stan_data The data used for the model as a named list 
#'   with the names corresponding to the variables in model description
#'   from the doctoral dissertation
#' @param ... Other arguments for `cmdstanr::cmdstan_model()` or
#'   `cmdstanr::cmdstan_sample()`
#' 
#' @details
#' The compiled model is stored in the R user cache
#' `tools::R_user_dir(package = "frankmakrdiss", which = "cache")`.
#'
#' @return A `CmdStanMCMC` object
#' @export
stan_dissmodel <- function(model_number, stan_data, ...) {
  args <- list(...)
  if (is.null(args$stanc_options)) args$stanc_options <- list("O1")
  if (is.null(args$chains)) args$chains <- 4
  if (is.null(args$adapt_delta)) args$adapt_delta <- 0.99
  if (is.null(args$refresh)) args$refresh <- 500
  if (is.null(args$show_messages)) args$show_messages <- FALSE 
  if (model_number == "12.3" && is.null(args$init)) args$init <- 0
  if (model_number == "14" && is.null(args$cpp_options))
    args$cpp_options <- list(stan_threads = TRUE)
  if (model_number == "14" && is.null(args$threads_per_chain))
    args$threads_per_chain <- getOption("mc.cores")
  if (model_number == "14") {
    args$chains <- 1
    args$iter_sampling <- 4000
  }
  model_name <- paste0("model_", model_number)
  assign(paste0("stan_", model_name),
    cmdstanr::cmdstan_model(
      stan_file = file.path(
        system.file(package = "frankmakrdiss", paste0(model_name, ".stan"))),
      dir = tools::R_user_dir(package = "frankmakrdiss", which = "cache"),
      cpp_options = args$cpp_options,
      stanc_options = args$stanc_options)
    )
  stan_fit <- get(paste0("stan_", model_name))$sample(
    data = stan_data,
    refresh = args$refresh,
    init = args$init,
    chains = args$chains,
    threads_per_chain = args$threads_per_chain,
    iter_warmup = args$iter_warmup,
    iter_sampling = args$iter_sampling,
    adapt_delta = args$adapt_delta,
    show_messages = args$show_messages
    )
  return(stan_fit)
}
