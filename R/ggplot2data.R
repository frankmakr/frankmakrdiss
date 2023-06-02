# ------------------------------
# ---                        ---
# --- Sample characteristics ---
# ---                        ---
# ------------------------------

#' Simulate one replication of sample probabilities
#'
#' Utility function
#'
#' @param n The sample size
#' @param probs A vector containing the probabilities
#' @return A matrix containing the sample probabilities
sim_sampleprobs <- function(n, probs) {
  x <- stats::rmultinom(n, 1, probs)
  return(matrixStats::rowSums2(x) / n)
}

#' Make plot data for evaluating representativity of the sample
#'
#' @description
#' \code{make_samplepointrange} prepares the data
#' in the format for the function \code{plot_samplepointrange}.
#'
#' @param sample_simlist A list containing the sample replications
#' @param population_simlist A list containing the population replications
#' @param probs The probability of the highest density region,
#'   which defaults to \code{probs = 0.87}
#' @return The ouput will be a data frame in the format for
#'   using the corresponding \code{plot_samplepointrange} function
#' @export
make_samplepointrange <- function(sample_simlist, population_simlist,
                                  probs = 0.87) {
  n_cols <- sapply(sample_simlist, ncol)
  median_1 <- unlist(lapply(sample_simlist, matrixStats::colMedians))
  median_2 <- unlist(lapply(population_simlist, matrixStats::colMedians))
  hdr_1 <- do.call(rbind,
    lapply(sample_simlist, function(i) t(apply(i, 2, calc_hdr, probs = probs))))
  hdr_2 <- do.call(rbind,
    lapply(population_simlist,
      function(i) t(apply(i, 2, calc_hdr, probs = probs))))
  varlist <- vector("list", 6)
  names(varlist) <- c("x", "y", "ll", "ul", "color", "fac")
  #varlist$x <- c(median_1, median_2)
  varlist$x <- c(median_1, median_2) - median_2
  varlist$y <- factor(rep(1:sum(n_cols), 2),
    levels = unlist(lapply(split(1:sum(n_cols), rep(seq_along(n_cols), n_cols)),
      rev), use.names = FALSE),
    labels = unlist(lapply(fernuni2019,
      function(i) rev(names(i)))[c(1, 3, 5, 6)], use.names = FALSE))
  #varlist$ll <- c(hdr_1[, 1], hdr_2[, 1])
  varlist$ll <- c(hdr_1[, 1], hdr_2[, 1]) - median_2
  #varlist$ul <- c(hdr_1[, 2], hdr_2[, 2])
  varlist$ul <- c(hdr_1[, 2], hdr_2[, 2]) - median_2
  varlist$color <- factor(rep(1:2, each = sum(n_cols)), levels = 2:1,
    labels = c("Studierendenstatistik", "Stichprobe"))
  varlist$fac <- factor(rep(rep(seq_along(n_cols), n_cols), 2),
    levels = seq_along(n_cols),
    labels = c("Gender", "Altersgruppe", "Wohnort", "Studierendenstatus"))
  pointrangedata <- data.frame(varlist, row.names = seq_along(varlist$x))
  return(pointrangedata)
}



# -------------------------------------
# ---                               ---
# --- Posterior retrodictive checks ---
# ---                               ---
# -------------------------------------

#' Count bins
#'
#' Utility function
#'
#' @param data A numerical vector
#' @param bx The breaking points
count_bins <- function(data, bx) {
  bincount <- matrixStats::binCounts(data, bx = bx, right = TRUE)
  names(bincount) <- as.character(round(bx[1:length(bx) - 1], digits = 2))
  return(bincount)
}

#' Count bins from a draws matrix
#'
#' Utility function
#'
#' @param draws_mat The draws matrix \code{y_rep_mat}
#' @param bx The breaking points
count_bins_iter <- function(draws_mat, bx) {
  iter <- 1:nrow(draws_mat)
  counts_mat <- sapply(iter, function(i) count_bins(draws_mat[i, ], bx))
  colnames(counts_mat) <- paste("iter", iter, sep = "_")
  return(counts_mat)
}

#' Group quantiles from a counts and a quantile matrix
#'
#' Utility function
#'
#' @param counts_mat A matrix returned from \code{count_bins_iter}
#' @param q_mat The row quantiles from a matrix returned from
#'   \code{count_bins_iter}
qgroups_iter <- function(counts_mat, q_mat) {
  group_rows <- function(row) {
    rowgroup <- .bincode(counts_mat[row, ], q_mat[row, ], include.lowest = TRUE)
    return(rowgroup)
  }
  groups_mat <- vapply(
    1:nrow(counts_mat),
    group_rows,
    FUN.VALUE = numeric(ncol(counts_mat))
    )
  dimnames(groups_mat) <- list(colnames(counts_mat), rownames(counts_mat))
  qgroups_mat <- t(groups_mat)
  return(qgroups_mat)
}

#' Make plot data for a posterior retrodictive check
#'
#' @description
#' \code{make_prcjitter} prepares the data
#' in the format for the function \code{plot_prcjitter}.
#'
#' @param y A list containing the sample replications
#' @param y_rep_mat A list containing the population replications
#' @return The ouput will be a data frame in the format for
#'   using the corresponding \code{plot_prcjitter} function
#' @export
make_prcjitter <- function(y, y_rep_mat) {
  bx <- seq(from = floor(min(y, y_rep_mat) * 100) / 100,
    to = ceiling(max(y, y_rep_mat) * 100) / 100 + 0.04, by = 0.02) - 0.001
  num_bins <- length(bx) - 1 
  counts_y <- count_bins(y, bx)
  counts_y[num_bins] <- counts_y[num_bins - 1]
  counts_mat <- count_bins_iter(y_rep_mat, bx)
  counts_mat[num_bins, ] <- NA
  q_mat <- matrixStats::rowQuantiles(counts_mat, probs = seq(0, 1, 0.01),
    type = 8L)
  med_rep <- q_mat[, "50%"]
  med_rep[num_bins] <- med_rep[num_bins - 1]
  qgroups_mat <- qgroups_iter(counts_mat, q_mat)
  repfactor <- ncol(counts_mat)
  varlist <- vector("list", 5)
  names(varlist) <- c("x", "y_rep", "fill_jitter", "y_step", "color_step")
  varlist$x <- rep(as.numeric(rownames(counts_mat)), repfactor)
  varlist$y_rep <- c(counts_mat)
  varlist$fill_jitter <- c(qgroups_mat) / 100
  varlist$y_step <- rep(c(med_rep, counts_y), repfactor / 2)
  varlist$color_step <- factor(rep(rep(2:1, each = num_bins), repfactor / 2),
    labels = c("Observationen", "Median Replikationen"))
  jitterdata <- data.frame(varlist, row.names = seq_along(varlist$x))
  return(jitterdata)
}

#' Calculate test statistics for posterior retrodictive checks
#'
#' Utility function
#'
#' @param data A numerical vector or matrix
calc_prcteststat <- function(data) {
  if (is.vector(data) == TRUE) {
    data <- matrix(data, nrow = 1)
  }
  prcteststat <- matrix(NA_real_, nrow = nrow(data), ncol = 2,
      dimnames = list(NULL, c("Median", "MAD")))
    prcteststat[, 1] <- matrixStats::rowQuantiles(data, probs = 0.5, type = 8L)
    prcteststat[, 2] <- matrixStats::rowMads(data, center = prcteststat[, 1],
      constant = 1)
  return(prcteststat)
}

#' Make plot data for a posterior retrodictive check
#'
#' @description
#' \code{make_prcstats} prepares the data
#' in the format for the function \code{plot_prcstats}.
#'
#' @param y A list containing the sample replications
#' @param y_rep_mat A list containing the population replications
#' @return The ouput will be a data frame in the format for
#'   using the corresponding \code{plot_prcstats} function
#' @export
make_prcstats <- function(y, y_rep_mat) {
  stat_y <- calc_prcteststat(y)
  stat_y_rep <- calc_prcteststat(y_rep_mat)
  bx <- lapply(1:2, function(i) seq(
    from = floor(min(stat_y[, i], stat_y_rep[, i]) * 100) / 100,
    to = ceiling(max(stat_y[, i], stat_y_rep[, i]) * 100) / 100,
    by = 0.01
    ) - 0.001)
  counts_stat <- lapply(1:2, function(i) count_bins(stat_y_rep[, i], bx[[i]]))
  shadelimits <- lapply(1:2, function(i) calc_hdr(stat_y_rep[, i],
    probs = 0.87))
  repfactor <- sapply(counts_stat, length)
  varlist <- vector("list", 5)
  names(varlist) <- c("stat", "stat_y", "x", "y", "fill")
  varlist$stat <- factor(
    unlist(lapply(1:2, function(i) rep(colnames(stat_y)[i], repfactor[i]))),
    levels = colnames(stat_y))
  varlist$stat_y <- unlist(lapply(1:2, function(i) rep(stat_y[i], repfactor[i])))
  varlist$x <- as.numeric(unlist(lapply(counts_stat, names)))
  varlist$y <- unlist(counts_stat)
  varlist$fill <- factor(
    unlist(mapply(findInterval,
      lapply(counts_stat, function(i) as.numeric(names(i))),
      lapply(shadelimits, sort),
      MoreArgs = list(left.open = TRUE))),
    levels = c(0:2),
    labels = c("Replikationen", "$\\qty{87}{\\percent}\\operatorname{HDR}$",
      "Replikationen"))
  prcstatsdata <- data.frame(varlist, row.names = seq_along(varlist$x))
  return(prcstatsdata)
}



# ----------------------
# ---                ---
# --- Graphs study 2 ---
# ---                ---
# ----------------------

#' Make plot data for showing a variance component
#'
#' @description
#' \code{make_sigmapointrange} prepares the data
#' in the format for the function \code{plot_sigmapointrange}.
#'
#' @param draws_mat_1 A draws matrix containing the
#'   superpopulation standard deviation from the unadjusted model
#' @param draws_mat_2 A draws matrix containing the
#'   superpopulation standard deviation from the adjusted model
#' @param probs A numeric value representing
#'   the probability for the highest density region,
#'   which defaults to \code{probs = 0.87}
#' @return The ouput will be a data frame in the format for
#'   using the corresponding \code{plot_sigmapointrange} function
#' @export
make_sigmapointrange <- function(draws_mat_1, draws_mat_2, probs = 0.87) {
  n_cols <- ncol(draws_mat_1)
  hdr <- t(apply(cbind(draws_mat_1, draws_mat_2), 2, calc_hdr, probs = probs))
  varlist <- vector("list", 5)
  names(varlist) <- c("x", "y", "ll", "ul", "color")
  varlist$x <- c(matrixStats::colMedians(draws_mat_1),
    matrixStats::colMedians(draws_mat_2))
  varlist$y <- factor(rep(1:n_cols, 2), levels = 1:n_cols,
    labels = c("Bezugsgemeinschaften $\\gamma$", "Personen $\\psi$",
      "Observationen $y$"))
  varlist$ll <- hdr[, 1]
  varlist$ul <- hdr[, 2]
  varlist$color <- factor(rep(1:2, each = n_cols), levels = 1:2,
    labels = c("Ohne Adjustierung", "Mit Adjustierung"))
  pointrangedata <- data.frame(varlist, row.names = seq_along(varlist$x))
  return(pointrangedata)
}

#' Calculate median centered effects
#'
#' @description
#' \code{calc_meddelta_gammas} centers the effects from a draws matrix
#' at the median
#'
#' @param draws_mat A draws matrix containing the effects
#' @return The output will be a draws matrix with median centered effects
#' @export
calc_meddelta_gammas <- function(draws_mat) {
  delta_mat <- draws_mat - matrixStats::rowMedians(draws_mat)
  return(delta_mat)
}

#' Make plot data for showing the community effects
#'
#' @description
#' \code{make_gammapointrange} prepares the data
#' in the format for the function \code{plot_gammapointrange}.
#'
#' @param draws_mat_1 A draws matrix containing the community effects
#'   from the unadjusted model
#' @param draws_mat_2 A draws matrix containing the community effects
#'   from the adjusted model
#' @param probs A numeric value representing
#'   the probability for the highest density region,
#'   which defaults to \code{probs = 0.87}
#' @return The ouput will be a data frame in the format for
#'   using the corresponding \code{plot_gammapointrange} function
#' @export
make_gammapointrange <- function(draws_mat_1, draws_mat_2, probs = 0.87) {
  n_cols <- ncol(draws_mat_1)
  hdr <- t(apply(cbind(draws_mat_1, draws_mat_2), 2, calc_hdr, probs = probs))
  varlist <- vector("list", 5)
  names(varlist) <- c("x", "y", "ll", "ul", "color")
  varlist$x <- c(matrixStats::colMedians(draws_mat_1),
    matrixStats::colMedians(draws_mat_2))
  varlist$y <- factor(rep(1:n_cols, 2), levels = n_cols:1)
  varlist$ll <- hdr[, 1]
  varlist$ul <- hdr[, 2]
  varlist$color <- factor(rep(1:2, each = n_cols), levels = 1:2,
    labels = c("Ohne Adjustierung", "Mit Adjustierung"))
  pointrangedata <- data.frame(varlist, row.names = seq_along(varlist$x))
  return(pointrangedata)
}

#' Make plot data for showing the pairwise overlapping probabilities
#'
#' @description
#' \code{make_compjitter} prepares the data
#' in the format for the function \code{plot_compjitter}.
#'
#' @param probs_list A list containing the
#'   pairwise overlapping probabilities
#' @param dims_list A list containing the names of the test dimensions
#' @return The ouput will be a data frame in the format for
#'   using the corresponding \code{plot_compjitter} function
#' @export
make_compjitter <- function(probs_list, dims_list) {
  n_probs <- lapply(probs_list, function(i) sum(lower.tri(i)))
  compjitter <- data.frame(
    probs = unlist(lapply(probs_list, function(i) i[lower.tri(i)])),
    dims = factor(rep(seq_along(dims_list), each = n_probs[[1]]),
      levels = rev(seq_along(dims_list)), labels = rev(names(dims_list))),
    fill_jitter = unlist(lapply(probs_list,
      function(i) findInterval(i[lower.tri(i)], stats::quantile(i[lower.tri(i)],
        probs = seq(0, 1, 0.01)), left.open = TRUE))) * 0.01)
  compjitter$alpha_jitter <- compjitter$fill_jitter
  compjitter$alpha_jitter[compjitter$alpha_jitter > 0.5] <-
    1 - compjitter$alpha_jitter[compjitter$alpha_jitter > 0.5]
  return(compjitter)
}
