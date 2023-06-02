#' Calculate the Highest Density Region
#'
#' \code{calc_hdr} calculates the Highest Density Region (HDR)
#' from a vector containing posterior samples
#'
#' @param vec_x A numeric vector containing the posterior samples.
#' @param probs A vector with the probabilities for the credible intervals.
#'   The default calculates the 50%, 87%, and 99% HDR.
#' @return The output will be a matrix containing the credible intervals.
#'
calc_hdr <- function(vec_x, probs = c(0.50, 0.87, 0.99)) {
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
