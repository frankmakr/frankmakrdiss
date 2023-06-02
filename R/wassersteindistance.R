#' Calculate the Wasserstein distance
#'
#' Utility function
#'
#' @param x A distribution function as vector
#' @param y A distribution function as vector
#' @param type One of \code{"l1"}, \code{"l2"} or \code{"energy"}
#' @return The output will be the distance
calc_wasserstein <- function(x, y, type = c("l1", "l2", "energy")) {
  xy <- c(x, y)
  n_x <- length(x)
  n_y <- length(y)
  n_xy <- length(xy)
  ecdf_index <- order(xy)
  ecdf_x <- c(rep(1/n_x, n_x), rep(0, n_y))[ecdf_index]
  ecdf_y <- c(rep(0, n_x), rep(1/n_y, n_y))[ecdf_index]
  ecdf_xy <- xy[ecdf_index]
  p <- ifelse(type == "l1", 1, 2)
  W_p <- sum(
    abs(cumsum(ecdf_x) - cumsum(ecdf_y))[seq(n_xy - 1)]^p * diff(ecdf_xy)
    )^(1 / p)
  if (type == "energy") {
    return(W_p * sqrt(2))
  } else {
    return(W_p)
  }
}

#' Calculate the Wasserstein distance
#'
#' @description
#' \code{calc_drawswasserstein} calculates the
#' L1 or L2 Wasserstein distance or the Energy distance
#' for all pairwise combinations of distributions in a \code{draws_matrix}
#'
#' @param draws_mat A \code{draws_matrix} containing the distributions
#' @param ... The type of distance calculated,
#'   which must be one of \code{"l1"}, \code{"l2"} or \code{"energy"}
#' @return The output will be a distance matrix
calc_drawswasserstein <- function(draws_mat, ...) {
  n_col <- ncol(draws_mat)
  dist_mat <- matrix(0, n_col, n_col)
  dist_mat[lower.tri(dist_mat)] <- apply(utils::combn(1:n_col, 2), 2,
    function(i) calc_wasserstein(draws_mat[, i[1]], draws_mat[, i[2]], ...))
  dist_mat[upper.tri(dist_mat)] <- t(dist_mat)[upper.tri(dist_mat)]
  return(dist_mat)
}
