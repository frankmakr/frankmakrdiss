#' Procrustes Transformation of MDS Configuration
#'
#' @description
#' `transform_procrustes()` calculates the transformation of a
#' multidimensional scaling configuration Y
#' to a target configuration X.
#'
#' @param x_mat A numeric matrix representing the MDS target configuration
#' @param y_mat A numeric matrix representing the MDS configuration
#'   to be compared
#' @param dilation A logical indicator for implementing optional scaling
#'   which defaults to `dilation = FALSE`
#' @return A numeric matrix representing the transformed MDS configuration
#' @export
transform_procrustes <- function(x_mat, y_mat, dilation = FALSE) {
  n_rows <- nrow(x_mat)
  z_mat <- diag(n_rows) - 1 / n_rows
  c_mat <- t(x_mat) %*% z_mat %*% y_mat
  svd_c_mat <- svd(c_mat)
  t_mat <- svd_c_mat$v %*% t(svd_c_mat$u)
  s <- ifelse(dilation,
    sum(diag(c_mat %*% t_mat)) / sum(diag(t(y_mat) %*% z_mat %*% y_mat)),
    1L)
  t_vec <- matrixStats::colSums2(x_mat - s * y_mat %*% t_mat) * 1 / n_rows
  y_hat <- sweep(s * y_mat %*% t_mat, 2, t_vec, "+")
  return(y_hat)
}
