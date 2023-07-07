#' Calculate the Highest Density Region
#'
#' @description
#' `calc_hdr()` calculates the highest density region (HDR)
#' from a numeric vector containing posterior samples
#'
#' @param vec_x A numeric vector containing the posterior samples
#' @param probs A numeric vector with the probabilities for the credible intervals
#'   which defaults to `probs = c(0.50, 0.87, 0.99)`
#' @return A named numeric matrix containing the credible intervals
#' @export
calc_hdr <- function(vec_x, probs = c(0.50, 0.87, 0.99)) {
  stopifnot("vec_x must be numeric" = is.numeric(vec_x))
  stopifnot("probs must be numeric" = is.numeric(probs))
  stopifnot("Elements of probs must have values between 0 and 1" =
    min(probs) > 0 && max(probs) < 1)
  n <- length(vec_x)
  vec_x_ord <- sort.int(vec_x, method='quick')
  cutoff <- n - floor(n * probs)
  llims <- lapply(cutoff, function(i) vec_x_ord[1:i])
  ulims <- lapply(cutoff, function(i) vec_x_ord[(n - i + 1):n])
  minpos <- mapply(function(ul, ll) which.min(ul - ll), ulims, llims)
  hdr_mat <- mapply(function(ll, ul, pos) c(ll[pos], ul[pos]),
    llims, ulims, minpos)
  dimnames(hdr_mat) <- list(c("-", "+"), paste0(probs * 100, "% HDR"))
  return(hdr_mat)
}
