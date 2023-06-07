#' Fit Doctoral Dissertation's Stan Models
#'
#' @description
#' `stan_dissmodel` fits the stan models from the doctoral dissertation.
#'
#' @param model_number A character string containing the model number
#'   from the doctoral dissertation (i.e. "12.2")
#' @param stan_data The data used for the model as a named list 
#' @param ... Other arguments for `model()` or `sample()`
#' 
#' @details
#' The compiled model is stored in the R user cache
#' \code{tools::R_user_dir(package = "frankmakrdiss", which = "cache")}.
#'
#' @return The function returns a stan fit object.
#' @export
stan_dissmodel <- function(model_number, stan_data, ...) {
  args <- list(...)
  if (is.null(args$refresh)) args$refresh <- 500
  if (is.null(args$show_messages)) args$show_messages <- FALSE 
  if (is.null(args$init) && model_number == "12.3") args$init <- 0
  if (is.null(args$cpp_options)) args$cpp_options <- list(stan_threads = FALSE)
  model_name <- paste0("model_", model_number)
  assign(paste0("stan_", model_name),
    cmdstanr::cmdstan_model(
      stan_file = file.path(
        system.file(package = "frankmakrdiss"), paste0(model_name, ".stan")),
      dir = tools::R_user_dir(package = "frankmakrdiss", which = "cache"),
      cpp_options = args$cpp_options)
    )
  stan_fit <- get(paste0("stan_", model_name))$sample(
    data = stan_data,
    init = args$init,
    refresh = args$refresh,
    show_messages = args$show_messages)
  return(stan_fit)
}
