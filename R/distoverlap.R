#' Calculate Distribution Overlap
#'
#' Utility function
#'
#' @param dist1 An empirical distribution of equal length to dist2
#' @param dist2 An empirical distribution of equal length to dist1
#' @return A number
#' @noRd
calc_overlap <- function(dist1, dist2) {
  dist_mat <- cbind(dist1, dist2)
  length_dist <- nrow(dist_mat)
  range_dist <- range(dist_mat)
  f_dist <- apply(dist_mat, 2, function(i) stats::density(i,
    from = range_dist[1], to = range_dist[2], n = length_dist)$y)
  f_uniform <- stats::runif(length_dist, 0, max(f_dist))
  z_dist <- apply(cbind(f_dist, matrixStats::rowMins(f_dist)), 2,
    function(i) f_uniform <= i)
  z_sums <- matrixStats::colSums2(z_dist)
  overlap <- (z_sums[3] / z_sums[1] + z_sums[3] / z_sums[2]) / 2
  return(overlap)
}

#' Calculate Distribution Overlap
#'
#' @description
#' `calc_drawsoverlap()` calculates the
#' generalized distribution overlap according to Weitzman (1970)
#' for a `draws_matrix`.
#'
#' @param draws_mat A `draws_matrix`
#' @return A n x n numeric matrix
#' @details
#' A `draws_matrix` is a numeric matrix
#' in which the rows are posterior draws
#' and the columns are variables.
#' @source
#' Weitzman, M. S. (1970).
#' *Measures of overlap of income distribution
#' of white and negro families in the United States*
#' (Technical paper No. 22).
#' United States Bureau of the Census.
#' <https://hdl.handle.net/2027/mdp.39015085502204>.
#' @references
#' Al-Saleh, M. F., & Samawi, H. M. (2007).
#' Interference on overlapping coefficients in two exponential populations.
#' *Journal of Modern Applied Statistical Methods, 6(2), 503–516.*
#' <https://doi.org/10.22237/jmasm/1193890440>.
#' @export
calc_drawsoverlap <- function(draws_mat) {
  n_col <- ncol(draws_mat)
  overlap_mat <- diag(n_col)
  overlap_mat[lower.tri(overlap_mat)] <- apply(utils::combn(1:n_col, 2), 2,
    function(i) calc_overlap(draws_mat[, i[1]], draws_mat[, i[2]]))
  return(overlap_mat)
}
