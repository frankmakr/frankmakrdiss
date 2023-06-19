# ------------------------------
# ---                        ---
# --- Sample characteristics ---
# ---                        ---
# ------------------------------

#' One Replication of Sample Probabilities
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

#' Plot Data for Evaluating Sample Representativity
#'
#' @description
#' `make_samplepointrange` prepares the data
#' in the format for the function `plot_samplepointrange`.
#'
#' @param sample_simlist A list containing the sample replications
#' @param population_simlist A list containing the population replications
#' @param probs The probability of the highest density region,
#'   which defaults to `probs = 0.87`
#' @return The ouput will be a data frame in the format for
#'   using the corresponding `plot_samplepointrange` function
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
#' @param data A numerical vector
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
#' @param draws_mat The draws matrix `y_rep_mat`
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
#' @param counts_mat A matrix returned from `count_bins_iter`
#' @param q_mat The row quantiles from a matrix returned from
#'   `count_bins_iter`
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
#' `make_prcjitter` prepares the data
#' in the format for the function `plot_prcjitter`.
#'
#' @param y A vector containing the sample replications
#' @param y_rep_mat A draws matrix containing the population replications
#' @return The ouput will be a data frame in the format for
#'   using the corresponding `plot_prcjitter` function
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
#' @param data A numerical vector or matrix
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
#' `make_prcstats` prepares the data
#' in the format for the function `plot_prcstats`.
#'
#' @param y A vector containing the sample replications
#' @param y_rep_mat A draws matrix containing the population replications
#' @param tikzdevice Logical indicator for preparing the text for tikzDevice
#'   which defaults to `tikzdevice = FALSE`
#' @return The ouput will be a data frame in the format for
#'   using the corresponding `plot_prcstats` function
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
#' `make_thetapointrange` prepares the data
#' in the format for the function `plot_thetapointrange`.
#'
#' @param draws_mat A draws matrix containing the
#'   community probabilities
#' @param probs The probability for the Highest Density Region
#'   which defaults to `probs = 0.87`
#' @param n_group_tilde Expected group size in the future sample
#' @param n_sample_tilde Expected size of the future sample
#' @param tikzdevice Logical indicator for preparing the text for tikzDevice
#'   which defaults to `tikzdevice = FALSE`
#' @return The output will be a data frame in the format for
#'   using the corresponding `plot_thetapointrange` function
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
        paste0("87% HDR \u03B8 \u2265 ", thetapointrange$xint),
        paste0("87% HDR \u03B8 < ", thetapointrange$xint))
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
#' `make_sigmapointrange` prepares the data
#' in the format for the function `plot_sigmapointrange`.
#'
#' @param draws_mat_1 A draws matrix containing the
#'   superpopulation standard deviation from the unadjusted model
#' @param draws_mat_2 A draws matrix containing the
#'   superpopulation standard deviation from the adjusted model
#' @param probs A numeric value representing
#'   the probability for the highest density region,
#'   which defaults to `probs = 0.87`
#' @param tikzdevice Logical indicator for preparing the text for tikzDevice
#'   which defaults to `tikzdevice = FALSE`
#' @return The ouput will be a data frame in the format for
#'   using the corresponding `plot_sigmapointrange` function
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
    labels = c("Bezugsgemeinschaften \u03B3", "Personen \u03C8",
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
#' `calc_meddelta_gammas` centers the effects from a draws matrix
#' at the median
#'
#' @param draws_mat A draws matrix containing the effects
#' @return The output will be a draws matrix with median centered effects
#' @export
calc_meddelta_gammas <- function(draws_mat) {
  delta_mat <- draws_mat - matrixStats::rowMedians(draws_mat)
  return(delta_mat)
}

#' Plot Data for Community Effects
#'
#' @description
#' `make_gammapointrange} prepares the data
#' in the format for the function `plot_gammapointrange`.
#'
#' @param draws_mat_1 A draws matrix containing the community effects
#'   from the unadjusted model
#' @param draws_mat_2 A draws matrix containing the community effects
#'   from the adjusted model
#' @param probs A numeric value representing
#'   the probability for the highest density region,
#'   which defaults to `probs = 0.87`
#' @return The ouput will be a data frame in the format for
#'   using the corresponding `plot_gammapointrange` function
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
#' `make_compjitter` prepares the data
#' in the format for the function `plot_compjitter`.
#'
#' @param probs_list A list containing the
#'   pairwise overlapping probabilities
#' @param dims_list A list containing the names of the test dimensions
#' @return The ouput will be a data frame in the format for
#'   using the corresponding `plot_compjitter` function
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



# ----------------------
# ---                ---
# --- Graphs study 3 ---
# ---                ---
# ----------------------

#' Plot Data for a Posterior Retrodictive Check
#'
#' @description
#' `make_dp_prcjitter` prepares the data
#' in the format for the function `plot_dp_prcjitter`.
#'
#' @param y A vector containing the sample replications
#' @param y_rep_mat A draws matrix containing the population replications
#' @return The ouput will be a data frame in the format for
#'   using the corresponding `plot_prcjitter` function
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
#' `make_dp_prcstats` prepares the data
#' in the format for the function `plot_dp_prcstats`.
#'
#' @param y A vector containing the sample replications
#' @param y_rep_mat A draws matrix containing the population replications
#' @param tikzdevice Logical indicator for preparing the text for tikzDevice
#'   which defaults to `tikzdevice = FALSE`
#' @return The ouput will be a data frame in the format for
#'   using the corresponding `plot_dp_prcstats` function
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
#' `make_lambdapointrange` prepares the data
#' in the format for the function `plot_lambdapointrange`.
#'
#' @param draws_mat_list A list of draws matrices containing the lambdas
#' @param labels A character vector containing the names of the graph facets
#' @param probs A numeric value representing
#'   the probability for the highest density region,
#'   which defaults to `probs = 0.87`
#' @param tikzdevice Logical indicator for preparing the text for tikzDevice
#'   which defaults to `tikzdevice = FALSE`
#' @return The ouput will be a data frame in the format for
#'   using the corresponding `plot_dp_prcstats` function
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
      "ul < 0.03 \u22C0 \u0078\u0304 < 0.01",
      "ul \u2265 0.03 \u22C0 \u0078\u0304 < 0.01",
      "ul \u2265 0.03 \u22C0 \u0078\u0304 \u2265 0.01"))
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
#' `make_logclusterdensities` prepares the data
#' in the format for the function `plot_logclusterdensities`.
#'
#' @param draws_mat_list A named list of draws matrices containing the
#'   log cluster densities 
#' @return The ouput will be a data frame in the format for
#'   using the corresponding `plot_logclusterdensities` function
#' @export
make_logclusterdensities <- function(draws_mat_list) {
  lcd_matlist <- lapply(draws_mat_list, function(i)
    apply(array(i, c(4000, 22, 10)), c(2, 3), stats::median))
  xy_vars <- expand.grid(
    y = factor(1:22, levels = 22:1, labels = rev(frankmakrdiss::conval_comms$short)),
    x = factor(1:10))
  lcdlist <- sapply(seq(length(lcd_matlist)), function(i)
    data.frame(xy_vars[, 2:1], density = c(lcd_matlist[[i]]),
      dim = factor(seq(length(lcd_matlist))[i],
        levels = seq(length(lcd_matlist)), labels = names(lcd_matlist))),
    simplify = FALSE)
  lcddata <- do.call(rbind, lcdlist)
  return(lcddata)
}
