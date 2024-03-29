# ------------------------------
# ---                        ---
# --- Sample Characteristics ---
# ---                        ---
# ------------------------------

#' One Replication of Sample Probabilities
#'
#' Utility function
#'
#' @param n The sample size
#' @param probs A numeric vector containing the probabilities
#' @return A numeric matrix containing the sample probabilities
sim_sampleprobs <- function(n, probs) {
  x <- stats::rmultinom(n, 1, probs)
  return(matrixStats::rowSums2(x) / n)
}

#' Plot Data for Evaluating Sample Representativity
#'
#' @description
#' `make_samplepointrange()` prepares the data
#' in the format for the function `plot_samplepointrange()`.
#'
#' @param sample_simlist A list containing the sample replications
#' @param population_simlist A list containing the population replications
#' @param probs The probability of the highest density region
#'   which defaults to `probs = 0.87`
#' @return A data frame in the format for
#'   using the corresponding `plot_samplepointrange()` function
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
  varlist$x <- c(median_1, median_2) - median_2
  varlist$y <- factor(rep(1:sum(n_cols), 2),
    levels = unlist(lapply(split(1:sum(n_cols), rep(seq_along(n_cols), n_cols)),
      rev), use.names = FALSE),
    labels = unlist(lapply(fernuni2019,
      function(i) rev(names(i)))[c(1, 3, 5, 6)], use.names = FALSE))
  varlist$ll <- c(hdr_1[, 1], hdr_2[, 1]) - median_2
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

#' Count Bins
#'
#' Utility function
#'
#' @param data A numeric vector
#' @param bx The breaking points
#' @noRd
count_bins <- function(data, bx) {
  bincount <- matrixStats::binCounts(data, bx = bx, right = TRUE)
  names(bincount) <- as.character(round(bx[1:length(bx) - 1], digits = 2))
  return(bincount)
}

#' Count Bins from a Draws Matrix
#'
#' Utility function
#'
#' @param draws_mat The `draws_matrix` `y_rep_mat`
#' @param bx The breaking points
#' @noRd
count_bins_iter <- function(draws_mat, bx) {
  iter <- 1:nrow(draws_mat)
  counts_mat <- sapply(iter, function(i) count_bins(draws_mat[i, ], bx))
  colnames(counts_mat) <- paste("iter", iter, sep = "_")
  return(counts_mat)
}

#' Group Quantiles from a Counts and a Quantile Matrix
#'
#' Utility function
#'
#' @param counts_mat A matrix returned from `count_bins_iter()`
#' @param q_mat The row quantiles from a matrix returned from
#'   `count_bins_iter()`
#' @noRd
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

#' Plot Data for a Posterior Retrodictive Check
#'
#' @description
#' `make_prcjitter()` prepares the data
#' in the format for the function `plot_prcjitter()`.
#'
#' @param y A numeric vector containing the observations
#' @param y_rep_mat A `draws_matrix` containing the replicated observations
#' @return A data frame in the format for
#'   using the corresponding `plot_prcjitter()` function
#' @details
#' A `draws_matrix` is a numeric matrix
#' in which the rows are posterior draws
#' and the columns are variables.
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

#' Test Statistics for a Posterior Retrodictive Check
#'
#' Utility function
#'
#' @param data A numeric vector or matrix
#' @noRd
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

#' Plot Data for a Posterior Retrodictive Check
#'
#' @description
#' `make_prcstats()` prepares the data
#' in the format for the function `plot_prcstats()`.
#'
#' @param y A numeric vector containing the observations
#' @param y_rep_mat A `draws_matrix` containing the replicated observations
#' @param tikzdevice Logical indicator for preparing the text for `tikzDevice`
#'   which defaults to `tikzdevice = FALSE`
#' @return A data frame in the format for
#'   using the corresponding `plot_prcstats()` function
#' @inherit make_prcjitter details
#' @export
make_prcstats <- function(y, y_rep_mat, tikzdevice = FALSE) {
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
  varlist$fill <- unlist(
    mapply(findInterval,
      lapply(counts_stat, function(i) as.numeric(names(i))),
      lapply(shadelimits, sort),
      MoreArgs = list(left.open = TRUE)))
  if (!tikzdevice) {
    varlist$fill <- factor(varlist$fill, levels = c(0:2),
      labels = c("Replikationen", "87% HDR", "Replikationen"))
  } else {
    varlist$fill <- factor(varlist$fill, levels = c(0:2),
      labels = c("Replikationen", "$\\qty{87}{\\percent}\\operatorname{HDR}$",
        "Replikationen"))
  }
  prcstatsdata <- data.frame(varlist, row.names = seq_along(varlist$x))
  return(prcstatsdata)
}



# ----------------------
# ---                ---
# --- Graphs study 1 ---
# ---                ---
# ----------------------

#' Plot data for community probabilities
#'
#' @description
#' `make_thetapointrange()` prepares the data
#' in the format for the function `plot_thetapointrange()`.
#'
#' @param draws_mat A `draws_matrix` containing the
#'   community probabilities
#' @param probs The probability for the highest density region
#'   which defaults to `probs = 0.87`
#' @param n_group_tilde Expected group size in the future sample
#' @param n_sample_tilde Expected size of the future sample
#' @param tikzdevice Logical indicator for preparing the text for `tikzDevice`
#'   which defaults to `tikzdevice = FALSE`
#' @inherit make_prcjitter details
#' @return A data frame in the format for
#'   using the corresponding `plot_thetapointrange()` function
#' @export
make_thetapointrange <- function(draws_mat,
                                 probs = 0.87,
                                 n_group_tilde, n_sample_tilde,
                                 tikzdevice = FALSE) {
  thetapointrange <- vector(mode = "list", length = 6)
  names(thetapointrange) <- c("med", "ll", "ul", "y", "color", "xint")
  thetapointrange$xint <- round(n_group_tilde / n_sample_tilde, digits = 2)
  thetapointrange$med <- matrixStats::colMedians(draws_mat)
  thetapointrange[2:3] <- asplit(apply(draws_mat, 2, calc_hdr, probs = probs), 1)
  thetapointrange[2:3] <- lapply(thetapointrange[2:3], function(i)
    i[order(thetapointrange$med)])
  thetapointrange$y <- factor(seq(ncol(draws_mat)),
    levels = seq(ncol(draws_mat)),
    labels = theta_comms[order(thetapointrange$med)])
  thetapointrange$color <- ifelse(
    thetapointrange$ll >= thetapointrange$xint, 1, 2)
  if (!tikzdevice) {
    thetapointrange$color <- factor(thetapointrange$color,
      labels = c(
        paste0("87% HDR \u03b8 \u2265 ", thetapointrange$xint),
        paste0("87% HDR \u03b8 < ", thetapointrange$xint))
      )
  } else {
    thetapointrange$color <- factor(thetapointrange$color,
      labels = c(
        paste0("$\\qty{87}{\\percent}\\operatorname{HDR}\\theta \\geq \\num{",
        thetapointrange$xint, "}$"),
        paste0("$\\qty{87}{\\percent}\\operatorname{HDR}\\theta < \\num{",
          thetapointrange$xint, "}$"))
      )
  }
  thetapointrange <- data.frame(thetapointrange, row.names = seq(ncol(draws_mat)))
  thetapointrange$med <- thetapointrange$med[order(thetapointrange$med)]
  return(thetapointrange)
}



# ----------------------
# ---                ---
# --- Graphs study 2 ---
# ---                ---
# ----------------------

#' Plot Data for Variance components
#'
#' @description
#' `make_sigmapointrange()` prepares the data
#' in the format for the function `plot_sigmapointrange()`.
#'
#' @param draws_mat_1 A `draws_matrix` containing the
#'   superpopulation standard deviation from the unadjusted model
#' @param draws_mat_2 A `draws_matrix` containing the
#'   superpopulation standard deviation from the adjusted model
#' @param probs The probability for the highest density region
#'   which defaults to `probs = 0.87`
#' @param tikzdevice Logical indicator for preparing the text for `tikzDevice`
#'   which defaults to `tikzdevice = FALSE`
#' @inherit make_prcjitter details
#' @return A data frame in the format for
#'   using the corresponding `plot_sigmapointrange()` function
#' @export
make_sigmapointrange <- function(draws_mat_1, draws_mat_2, probs = 0.87,
                                 tikzdevice = FALSE) {
  n_cols <- ncol(draws_mat_1)
  hdr <- t(apply(cbind(draws_mat_1, draws_mat_2), 2, calc_hdr, probs = probs))
  varlist <- vector("list", 5)
  names(varlist) <- c("x", "y", "ll", "ul", "color")
  varlist$x <- c(matrixStats::colMedians(draws_mat_1),
    matrixStats::colMedians(draws_mat_2))
  varlist$y <- rep(1:n_cols, 2)
  if (!tikzdevice) {
    varlist$y <- factor(varlist$y, levels = 1:n_cols,
    labels = c("Bezugsgemeinschaften \u03b3", "Personen \u03c8",
      "Observationen \u0079"))
  } else {
    varlist$y <- factor(varlist$y, levels = 1:n_cols,
    labels = c("Bezugsgemeinschaften $\\gamma$", "Personen $\\psi$",
      "Observationen $y$"))
  }
  varlist$ll <- hdr[, 1]
  varlist$ul <- hdr[, 2]
  varlist$color <- factor(rep(1:2, each = n_cols), levels = 1:2,
    labels = c("Ohne Adjustierung", "Mit Adjustierung"))
  pointrangedata <- data.frame(varlist, row.names = seq_along(varlist$x))
  return(pointrangedata)
}

#' Median Centered Effects
#'
#' @description
#' `calc_meddelta_gammas()` centers the effects from a `draws_matrix`
#' at the median
#'
#' @param draws_mat A `draws_matrix` containing the effects
#' @return A `draws_matrix` with median centered effects
#' @inherit make_prcjitter details
#' @export
calc_meddelta_gammas <- function(draws_mat) {
  delta_mat <- draws_mat - matrixStats::rowMedians(draws_mat)
  return(delta_mat)
}

#' Plot Data for Community Effects
#'
#' @description
#' `make_gammapointrange()` prepares the data
#' in the format for the function `plot_gammapointrange()`.
#'
#' @param draws_mat_1 A `draws_matrix` containing the community effects
#'   from the unadjusted model
#' @param draws_mat_2 A `draws_matrix` containing the community effects
#'   from the adjusted model
#' @param probs The probability for the highest density region
#'   which defaults to `probs = 0.87`
#' @return A data frame in the format for
#'   using the corresponding `plot_gammapointrange()` function
#' @inherit make_prcjitter details
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

#' Plot Data for Pairwise Distribution Overlap
#'
#' @description
#' `make_compjitter()` prepares the data
#' in the format for the function `plot_compjitter()`.
#'
#' @param probs_list A list containing the
#'   pairwise overlapping probabilities
#' @param dims_list A list containing the names of the test dimensions
#' @return A data frame in the format for
#'   using the corresponding `plot_compjitter()` function
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

#' Plot Data for a Posterior Retrodictive Check
#'
#' @description
#' `make_hypo_prcstats()` prepares the data
#' in the format for the function `plot_hypo_prcstats()`.
#'
#' @param y A numeric vector containing the observations
#' @param y_rep_mat A `draws_matrix` containing the replicated observations
#' @return A data frame in the format for
#'   using the corresponding `plot_hypo_prcstats()` function
#' @inherit make_prcjitter details
#' @export
make_hypo_prcstats <- function(y, y_rep_mat) {
  bx <- lapply(seq_along(y), function(i) seq(
    from = floor(min(y[i], y_rep_mat[, i]) * 100) / 100,
    to = ceiling(max(y[i], y_rep_mat[, i]) * 100) / 100,
    by = 2
    ))
  counts_stat <- lapply(seq_along(y), function(i)
    count_bins(y_rep_mat[, i], bx[[i]]))
  shadelimits <- lapply(seq_along(y), function(i)
    calc_hdr(y_rep_mat[, i], probs = 0.87))
  repfactor <- sapply(counts_stat, length)
  varlist <- vector("list", 5)
  names(varlist) <- c("stat_fac", "stat_y", "x", "y", "fill")
  varlist$stat_fac <- factor(
    unlist(lapply(seq_along(y), function(i) rep(i, repfactor[i]))))
  varlist$stat_y <- unlist(lapply(seq_along(y), function(i)
    rep(y[i], repfactor[i])))
  varlist$x <- as.numeric(unlist(lapply(counts_stat, names)))
  varlist$y <- unlist(counts_stat)
  varlist$fill <- factor(
    unlist(mapply(findInterval,
      lapply(counts_stat, function(i) as.numeric(names(i))),
      lapply(shadelimits, sort),
      MoreArgs = list(left.open = TRUE))),
    levels = c(0:2), labels = c("Replikationen",
      "$\\qty{87}{\\percent}\\operatorname{HDR}$", "Replikationen"))
  prcstatsdata <- data.frame(varlist, row.names = seq_along(varlist$x))
  return(prcstatsdata)
}

#' Plot Data for Testing the Hypotheses
#'
#' @description
#' `make_hypopointrange()` prepares the data
#' in the format for the function `plot_hypopointrange()`.
#'
#' @param draws_mat A `draws_matrix` containing the relevant effects
#' @param label_list A nested `list` containing the names for the y axis
#'  nested in the names for the color groups 
#' @param probs The probability for the highest density region
#'   which defaults to `probs = 0.87`
#' @return A data frame in the format for
#'   using the corresponding `plot_hypopointrange()` function
#' @inherit make_prcjitter details
#' @export
make_hypopointrange <- function(draws_mat, label_list, probs = 0.87) {
  n_cols <- ncol(draws_mat)
  hdr <- t(apply(draws_mat, 2, calc_hdr, probs = probs))
  varlist <- vector("list", 5)
  names(varlist) <- c("x", "y", "ll", "ul", "dim")
  varlist$x <- matrixStats::colMedians(draws_mat)
  varlist$y <- factor(1:n_cols, levels = rev(1:n_cols),
    labels = rev(unlist(label_list)))
  varlist$ll <- hdr[, 1]
  varlist$ul <- hdr[, 2]
  varlist$dim <- factor(
    rep(seq_along(label_list), sapply(label_list, length)),
    labels = names(label_list))
  pointrangedata <- data.frame(varlist, row.names = seq_along(varlist$x))
  return(pointrangedata)
}

# ----------------------
# ---                ---
# --- Graphs study 3 ---
# ---                ---
# ----------------------

#' Plot Data for a Posterior Retrodictive Check
#'
#' @description
#' `make_dp_prcjitter()` prepares the data
#' in the format for the function `plot_dp_prcjitter()`.
#'
#' @param y A numeric vector containing the observations
#' @param y_rep_mat A `draws_matrix` containing the replicated observations
#' @return A data frame in the format for
#'   using the corresponding `plot_prcjitter()` function
#' @inherit make_prcjitter details
#' @export
make_dp_prcjitter <- function(y, y_rep_mat) {
  bx <- seq(from = floor(min(y, y_rep_mat) * 100) / 100,
    to = ceiling(max(y, y_rep_mat) * 100) / 100, by = 0.5)
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

#' Plot Data for a Posterior Retrodictive Check
#'
#' @description
#' `make_dp_prcstats()` prepares the data
#' in the format for the function `plot_dp_prcstats()`.
#'
#' @param y A numeric vector containing the observations
#' @param y_rep_mat A `draws_matrix` containing the replicated observations
#' @param tikzdevice Logical indicator for preparing the text for `tikzDevice`
#'   which defaults to `tikzdevice = FALSE`
#' @return A data frame in the format for
#'   using the corresponding `plot_dp_prcstats()` function
#' @inherit make_prcjitter details
#' @export
make_dp_prcstats <- function(y, y_rep_mat, tikzdevice = FALSE) {
  stat_y <- calc_prcteststat(y)
  stat_y_rep <- calc_prcteststat(y_rep_mat)
  bx <- lapply(1:2, function(i) seq(
    from = floor(min(stat_y[, i], stat_y_rep[, i]) * 100) / 100,
    to = ceiling(max(stat_y[, i], stat_y_rep[, i]) * 100) / 100,
    by = 0.1
    ))
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
  varlist$fill <- unlist(
    mapply(findInterval,
      lapply(counts_stat, function(i) as.numeric(names(i))),
      lapply(shadelimits, sort),
      MoreArgs = list(left.open = TRUE)))
  if (!tikzdevice) {
    varlist$fill <- factor(varlist$fill, levels = c(0:2),
      labels = c("Replikationen", "87% HDR", "Replikationen"))
  } else {
    varlist$fill <- factor(varlist$fill, levels = c(0:2),
      labels = c("Replikationen", "$\\qty{87}{\\percent}\\operatorname{HDR}$",
        "Replikationen"))
  }
  prcstatsdata <- data.frame(varlist, row.names = seq_along(varlist$x))
  return(prcstatsdata)
}

#' Plot Data for Cluster Probabilities
#'
#' @description
#' `make_lambdapointrange()` prepares the data
#' in the format for the function `plot_lambdapointrange()`.
#'
#' @param draws_mat_list A list of `draws_matrix` objects
#'   containing the lambdas
#' @param labels A character vector containing the names of the graph facets
#' @param probs A numeric value representing
#'   the probability for the highest density region
#'   which defaults to `probs = 0.87`
#' @param tikzdevice Logical indicator for preparing the text for `tikzDevice`
#'   which defaults to `tikzdevice = FALSE`
#' @return A data frame in the format for
#'   using the corresponding `plot_dp_prcstats()` function
#' @inherit make_prcjitter details
#' @export
make_lambdapointrange <- function(draws_mat_list, labels, probs = 0.87,
                                  tikzdevice = FALSE) {
  n_rep <- length(draws_mat_list)
  n_cols <- rapply(draws_mat_list, ncol)[1]
  draws_cbind <- do.call(cbind, draws_mat_list)
  hdr <- t(apply(draws_cbind, 2, calc_hdr, probs = probs))
  varlist <- vector("list", 6)
  names(varlist) <- c("x", "y", "ll", "ul", "color", "dim")
  varlist$x <- matrixStats::colMedians(draws_cbind)
  varlist$y <- factor(rep(1:n_cols, n_rep), levels = rev(1:n_cols))
  varlist$ll <- hdr[, 1]
  varlist$ul <- hdr[, 2]
  varlist$color <- ifelse(varlist$ul < 0.03, 1, ifelse(varlist$x < 0.01, 2, 3))
  if (!tikzdevice) {    
    varlist$color <- factor(varlist$color, labels = c(
      "ul < 0.03 \u22c0 \u0078\u0303 < 0.01",
      "ul \u2265 0.03 \u22c0 \u0078\u0303 < 0.01",
      "ul \u2265 0.03 \u22c0 \u0078\u0303 \u2265 0.01"))
  } else {
    varlist$color <- factor(varlist$color, labels = c(
      "$\\mathrm{ul} < \\num{0.03} \\land \\tilde{x} < \\num{0.01}$",
      "$\\mathrm{ul} \\geq \\num{0.03} \\land \\tilde{x} < \\num{0.01}$",
      "$\\mathrm{ul} \\geq \\num{0.03} \\land \\tilde{x} \\geq \\num{0.01}$"))
  }
  varlist$dim <- factor(rep(1:n_rep, each = n_cols), levels = 1:n_rep,
    labels = labels)
  pointrangedata <- data.frame(varlist, row.names = seq_along(varlist$x))
  return(pointrangedata)
}

#' Plot Data for Cluster Densities
#'
#' @description
#' `make_logclusterdensities()` prepares the data
#' in the format for the function `plot_logclusterdensities()`.
#'
#' @param draws_mat_list A named list of `draws_matrix` objects
#'   containing the log cluster densities 
#' @return A data frame in the format for
#'   using the corresponding `plot_logclusterdensities()` function
#' @inherit make_prcjitter details
#' @export
make_logclusterdensities <- function(draws_mat_list) {
  lcd_matlist <- lapply(draws_mat_list, function(i)
    apply(array(i, c(4000, 22, 10)), c(2, 3), stats::median))
  xy_vars <- expand.grid(
    y = factor(1:22, levels = 22:1, labels = rev(frankmakrdiss::conval_comms$short)),
    x = factor(1:10))
  lcdlist <- sapply(seq_along(lcd_matlist), function(i)
    data.frame(xy_vars[, 2:1], density = c(lcd_matlist[[i]]),
      dim = factor(seq_along(lcd_matlist)[i],
        levels = seq_along(lcd_matlist), labels = names(lcd_matlist))),
    simplify = FALSE)
  lcddata <- do.call(rbind, lcdlist)
  return(lcddata)
}

#' Highest Density Region 2d
#'
#' Utility function
#' 
#' @param x_draws The x coordinates of the MDS configuration
#'   in the `draws_matrix`
#' @param y_draws The y coordinates of the MDS configuration
#'   in the `draws_matrix`
#' @param ... Optional arguments for `MASS::kde2d()`
#' @noRd
calc_2d_hdr <- function(x_draws, y_draws, ...) {
  probs <- seq(0.5, 0.95, 0.025)
  kde <- MASS::kde2d(x_draws, y_draws, ...)
  kde_x <- kde$x
  kde_y <- kde$y
  kde_z <- kde$z
  sort_kde_z <- sort(kde_z)
  dens_xy <- cumsum(sort_kde_z) * diff(kde_x[1:2]) * diff(kde_y[1:2])
  contour_levels <- lapply(probs, function(i)
    stats::approx(dens_xy, sort_kde_z, xout = 1 - i)$y)
  contour_lines <- grDevices::contourLines(kde_x, kde_y, kde_z,
    levels = contour_levels)
  contour_dat <- do.call(rbind, lapply(contour_lines, data.frame))
  hdr_dat <- data.frame(
    contour_dat[, -1],
    hdr_label = as.numeric(as.character(
      factor(contour_dat$level, labels = sort(probs, decreasing = TRUE))
      )),
    hdr_group = factor(
      rep(sort(seq(contour_lines), decreasing = TRUE),
        sapply(contour_lines, function(i) length(i$x))
        ))
    )
  return(hdr_dat)
}

#' Highest Density Region 2d Countours
#'
#' Utility function
#' 
#' @param mds_draws A `draws_matrix` containing the MDS configuration
#' @noRd
make_mds_contourdata <- function(mds_draws) {
  contour_list <- lapply(1:22,
    function(i) calc_2d_hdr(
      mds_draws[, i, 1],
      mds_draws[, i, 2],
      h = c(MASS::bandwidth.nrd(mds_draws[, i, 1]),
        MASS::bandwidth.nrd(mds_draws[, i, 2])) * 4,
      n = 25)
    )
  contour_grouplist <- lapply(contour_list,
    function(i) as.numeric(i$hdr_group)
    )
  contour_groupshift <- cumsum(
    sapply(contour_grouplist, function(i) length(unique(i)))
    )
  contour_groups <- c(
    contour_grouplist[[1]],
    unlist(lapply(1:21,
      function(i) contour_grouplist[[i + 1]] + contour_groupshift[i]))
    )
  mds_contourdata <- do.call(rbind, contour_list)
  mds_contourdata$contour_group <- contour_groups
  mds_contourdata$group <- factor(
    rep(seq(contour_grouplist), sapply(contour_grouplist, length)),
    labels = frankmakrdiss::conval_comms$short)
  return(mds_contourdata)
}

#' Highest Density Region 2d Medoids
#'
#' Utility function
#' 
#' @param mds_draws A `draws_matrix` containing the MDS configuration
#' @noRd
make_mds_pointdata <- function(mds_draws) {
  mds_pointdata <- data.frame(
    x = matrixStats::colMedians(mds_draws[, , 1]),
    y = matrixStats::colMedians(mds_draws[, , 2]),
    label = frankmakrdiss::conval_comms$short,
    group = factor(frankmakrdiss::conval_comms$short,
      levels = frankmakrdiss::conval_comms$short)
    )
  return(mds_pointdata)
}

#' Plot Data for MDS Configuration
#'
#' @description
#' `make_mds_plotdata()` prepares the data
#' in the format for the function `plot_dp_mds()`.
#'
#' @param draws_mat_list A named list of `draws_matrix` objects
#'   containing the MDS configurations
#' @param target_mat A `draws_matrix`
#'   containing the target configuration for the procrustes transformation
#' @return A data frame in the format for
#'   using the corresponding `plot_dp_mds()` function
#' @inherit make_prcjitter details
#' @export
make_mds_plotdata <- function(draws_mat_list, target_mat) {
  mds_dim <- factor(seq_along(draws_mat_list),
    labels = names(draws_mat_list))
  mds_hat <- lapply(
    lapply(draws_mat_list, array, c(4000, 22, 2)),
    function(i) aperm(
      sapply(1:4000, function(j)
        transform_procrustes(
          array(target_mat, c(4000, 22, 2))[j, , ],
          i[j, , ]),
        simplify = "array"),
      c(3, 1, 2))
    )
  mds_plotdata <- vector("list", length = 2L)
  names(mds_plotdata) <- c("medoids", "density")
  mds_plotdata$medoids <- do.call(rbind,
    lapply(seq_along(mds_hat), function(i)
      data.frame(make_mds_pointdata(mds_hat[[i]]), dim = mds_dim[i])
      ))
  mds_plotdata$density <- do.call(rbind,
    lapply(seq_along(mds_hat), function(i)
      data.frame(make_mds_contourdata(mds_hat[[i]]), dim = mds_dim[i])
      ))
  return(mds_plotdata)
}
