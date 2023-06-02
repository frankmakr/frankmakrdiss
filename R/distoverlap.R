#' Calculate Distribution overlap
#'
#' Utility function
#'
#' @param dist1 An empirical distribution of equal length to dist2
#' @param dist2 An empirical distribution of equal length to dist1
#' @return A number
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

#' Calculate Distribution overlap
#'
#' @description
#' \code{calc_drawsoverlap} calculates the
#' generalized distribution overlap according to Weitzman (1970)
#' for a \code{draws_matrix}
#'
#' @param draws_mat A \code{draws_matrix}
#' @return A n x n matrix
#' @export
calc_drawsoverlap <- function(draws_mat) {
  n_col <- ncol(draws_mat)
  overlap_mat <- diag(n_col)
  overlap_mat[lower.tri(overlap_mat)] <- apply(utils::combn(1:n_col, 2), 2,
    function(i) calc_overlap(draws_mat[, i[1]], draws_mat[, i[2]]))
  return(overlap_mat)
}
