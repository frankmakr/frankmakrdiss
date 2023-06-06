#' Check CmdStan version
#'
#' Utility function
#' @noRd
.onattach <- function(libname, pkgname) {
  packageStartupMessage("This is frankmakrdiss version ",
    utils::packageVersion("frankmakrdiss"), ".")
  if (is.null(cmdstanr::cmdstan_path())) {
  packageStartupMessage(
    "- Use cmdstanr::set_cmdstan_path() to set the path to CmdStan.")
  packageStartupMessage(
    "- Use cmdstanr::install_cmdstan() to install CmdStan.")
  } else {
  packageStartupMessage("- Cmdstan path: ", cmdstanr::cmdstan_path())
  packageStartupMessage("- Cmdstan version: ",
    cmdstanr::cmdstan_version(error_on_NA = FALSE))
  }
}

.onload <- function(...) {
  cmdstan_version <- cmdstanr::cmdstan_version(error_on_NA = FALSE)
  if (is.null(cmdstan_version)) {
    stop("No CmdStan installation found.
      Run cmdstanr::install_cmdstan() to install.",
      call. = FALSE)
  }
}
