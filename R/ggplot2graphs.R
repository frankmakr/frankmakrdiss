# ---------------------
# ---               ---
# --- ggplot2 Theme ---
# ---               ---
# ---------------------

#' ggplot2 theme
#'
#' Utility function
#' @noRd
theme_frankmakrdiss <- function(...) {
  ggplot2::theme_grey() %+replace%
  ggplot2::theme(
    axis.line = ggplot2::element_line(color = "black"),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(color = "grey96"),
    legend.key = ggplot2::element_blank()
    )
}



# ------------------------------
# ---                        ---
# --- Sample Characteristics ---
# ---                        ---
# ------------------------------

#' Plot Sample Characteristics
#'
#' @description
#' `plot_samplepointrange` plots data
#' in the format from the function `make_samplepointrange`.
#'
#' @param samplepointrange A data frame returned from the function
#'   `make_samplepointrange`
#' @param tikzdevice Logical indicator for preparing the text for tikzDevice
#'   which defaults to `tikzdevice = FALSE`
#' @return The output will be a ggplot2 graph
#' @export
plot_samplepointrange <- function(samplepointrange, tikzdevice = FALSE) {
  sampleplot <- ggplot2::ggplot(samplepointrange,
      ggplot2::aes(x = .data$x * 100, y = .data$y)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$color), size = 2,
      position = ggplot2::position_dodge(width = 0.7)) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = .data$ll * 100, xmax = .data$ul * 100,
      color = .data$color), size = 1.5,
      position = ggplot2::position_dodge(width = 0.7), key_glyph = "path") +
    ggplot2::scale_color_manual(
      values = ggplot2::alpha(
        as.vector(colorscheme_frankmakrdiss[c(1, 3), 5]), c(0.3, 1))) +
    ggplot2::facet_wrap(ggplot2::vars(.data$fac), dir = "v", scales = "free") +
    ggplot2::ylab(NULL) +
    theme_frankmakrdiss() +
    ggplot2::theme(legend.position = "top")
  if (!tikzdevice) {
    sampleplot +
      ggplot2::xlab("Abweichung vom Median der Studierendenstatistik in %") +
      ggplot2::guides(color = ggplot2::guide_legend(
        title = "87% HDR: ", reverse = TRUE))
  } else {
    sampleplot +
      ggplot2::xlab(
        "Abweichung vom Median der Studierendenstatistik in \\si{\\percent}") +
      ggplot2::guides(color = ggplot2::guide_legend(
        title = "$\\qty{87}{\\percent}\\operatorname{HDR}$: ", reverse = TRUE))
  }
}



# -------------------------------------
# ---                               ---
# --- Posterior retrodictive checks ---
# ---                               ---
# -------------------------------------

#' Plot Posterior Retrodictive Check
#'
#' @description
#' `plot_prcjitter` plots data
#' in the format from the function `make_prcjitter`.
#'
#' @param prcjitter A data frame returned from the function
#'   `make_prcjitter`
#' @return The output will be a ggplot2 graph
#' @export
plot_prcjitter <- function(prcjitter) {
  ggplot2::ggplot(prcjitter, ggplot2::aes(x = .data$x, y = .data$y_rep)) +
    ggplot2::geom_jitter(ggplot2::aes(fill = .data$fill_jitter),
      shape = "circle filled", stroke = 0, size = 3, alpha = 0.1,
      width = 0.005, na.rm = TRUE) +
    ggplot2::scale_fill_gradient2(low = colorscheme_frankmakrdiss[1, 5],
      mid = colorscheme_frankmakrdiss[4, 5],
      high = colorscheme_frankmakrdiss[1, 5], midpoint = 0.5, limits = c(0, 1),
      labels = scales::label_number(decimal.mark = ",")) +
    ggplot2::geom_step(ggplot2::aes(x = .data$x - 0.01, y = .data$y_step,
      color = .data$color_step)) +
    ggplot2::scale_color_manual(
      values = c("#000000", colorscheme_frankmakrdiss[6, 5]),
      na.translate = FALSE) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(decimal.mark = ",")) +
    ggplot2::xlab("y-Werte") +
    ggplot2::ylab("H\u00e4ufigkeiten") +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE,
        barwidth = 0.8, title = "Quantilbereich", title.vjust = 3),
      color = ggplot2::guide_legend(
        title = NULL, override.aes = list(size = 2))) +
    theme_frankmakrdiss()
}

#' Plot Posterior Retrodictive Check
#'
#' @description
#' `plot_prcstats` plots data
#' in the format from the function `make_prcstats`.
#'
#' @param prcstats A data frame returned from the function
#'   `make_prcstats`
#' @return The output will be a ggplot2 graph
#' @export
plot_prcstats <- function(prcstats) {
  ggplot2::ggplot(prcstats, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$fill),
      color = colorscheme_frankmakrdiss[1, 5], width = 0.01) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$stat_y,
      color = "Observationen")) +
    ggplot2::scale_color_manual(values = "#000000") +
    ggplot2::scale_fill_manual(
      values = c("#ffffff", colorscheme_frankmakrdiss[2, 5])) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(decimal.mark = ",")) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("H\u00e4ufigkeiten") +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = NULL, override.aes = list(size = 1.5)),
      fill = ggplot2::guide_legend(title = NULL)) +
    ggplot2::facet_wrap(ggplot2::vars(.data$stat), scales = "free_x") +
    theme_frankmakrdiss()
}



# ----------------------
# ---                ---
# --- Graphs study 1 ---
# ---                ---
# ----------------------

#' Plot Community Probabilities
#'
#' @description
#' `plot_thetapointrange` plots data
#' in the format from the function `make_thetapointrange`.
#'
#' @param thetapointrange A data frame returned from the function
#'   `make_thetapointrange`
#' @param tikzdevice Logical indicator for preparing the text for tikzDevice
#'   which defaults to `tikzdevice = FALSE`
#' @return The output will be a ggplot2 graph
#' @export
plot_thetapointrange <- function(thetapointrange, tikzdevice = FALSE) {
  thetaplot <- ggplot2::ggplot(thetapointrange,
      ggplot2::aes(x = .data$med, y = .data$y, color = .data$color)) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$xint),
      linetype = "longdash") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$ll, xmax = .data$ul)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = scales::alpha(
      as.vector(colorscheme_frankmakrdiss[c(3, 1), 5]), c(1, 0.3))) +
    ggplot2::scale_x_continuous(limits = c(0, 1),
      labels = scales::label_number(decimal.mark = ",")) +
    theme_frankmakrdiss() +
    ggplot2::theme(legend.position = "top")
    if (!tikzdevice) {
      thetaplot + ggplot2::labs(
        x = "Auftretenswahrscheinlichkeit \u03b8", y = NULL, color = NULL)
    } else {
      thetaplot + ggplot2::labs(
        x = "Auftretenswahrscheinlichkeit $\\theta$", y = NULL, color = NULL)
    }
}



# ----------------------
# ---                ---
# --- Graphs study 2 ---
# ---                ---
# ----------------------

#' Plot Variance Components
#'
#' @description
#' `plot_sigmapointrange` plots data
#' in the format from the function `make_sigmapointrange`.
#'
#' @param sigmapointrange A data frame returned from the function
#'   `make_sigmapointrange`
#' @param tikzdevice Logical indicator for preparing the text for tikzDevice
#'   which defaults to `tikzdevice = FALSE`
#' @return The output will be a ggplot2 graph
#' @export
plot_sigmapointrange <- function(sigmapointrange, tikzdevice = FALSE) {
  sigmaplot <- ggplot2::ggplot(sigmapointrange,
      ggplot2::aes(x = .data$x, y = .data$item)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$color), size = 2,
      position = ggplot2::position_dodge(width = 0.7)) +
    ggplot2::geom_linerange(ggplot2::aes(
      xmin = .data$ll, xmax = .data$ul, color = .data$color), linewidth = 1.5,
      position = ggplot2::position_dodge(width = 0.7), key_glyph = "path") +
    ggplot2::scale_color_manual(values = ggplot2::alpha(
      as.vector(colorscheme_frankmakrdiss[c(1, 3), 5]), c(0.3, 1))) +
    ggplot2::scale_x_continuous(limits = c(0, 1.5),
      labels = scales::label_number(decimal.mark = ",")) +
    ggplot2::facet_wrap(ggplot2::vars(.data$y), dir = "v") +
    ggplot2::ylab("Item") +
    theme_frankmakrdiss() +
    ggplot2::theme(legend.position = "top")
  if (!tikzdevice) {
    sigmaplot +
      ggplot2::xlab("\u03c3\u0303") +
      ggplot2::guides(color = ggplot2::guide_legend(
        title = "87% HDR: ", reverse = TRUE))
  } else {
    sigmaplot +
      ggplot2::xlab("$\\tilde{\\sigma}$") +
      ggplot2::guides(color = ggplot2::guide_legend(
        title = "$\\qty{87}{\\percent}\\operatorname{HDR}$: ",
          reverse = TRUE))
  }
}

#' Plot Community Effects
#'
#' @description
#' `plot_gammapointrange` plots data
#' in the format from the function `make_gammapointrange`.
#'
#' @param gammapointrange A data frame returned from the function
#'   `make_gammapointrange`
#' @param tikzdevice Logical indicator for preparing the text for tikzDevice
#'   which defaults to `tikzdevice = FALSE`
#' @return The output will be a ggplot2 graph
#' @export
plot_gammapointrange <- function(gammapointrange, tikzdevice = FALSE) {
  gammaplot <- ggplot2::ggplot(gammapointrange,
      ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey70") +
    ggplot2::geom_point(ggplot2::aes(color = .data$color),
      position = ggplot2::position_dodge(width = 0.5), size = 1.5) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$ll, xmax = .data$ul,
      color = .data$color), position = ggplot2::position_dodge(width = 0.5),
      linewidth = 1, key_glyph = "path") +
    ggplot2::scale_color_manual(values = ggplot2::alpha(
      as.vector(colorscheme_frankmakrdiss[c(1, 3), 5]), c(0.3, 1))) +
    ggplot2::scale_x_continuous(limits = c(-0.6, 0.6),
      labels = scales::label_number(decimal.mark = ",")) +
    ggplot2::scale_y_discrete(labels = c(rbind(rev(seq(2, 22, 2)), ""))) +
    ggplot2::facet_wrap(ggplot2::vars(.data$item), dir = "v", ncol = 3) +
    ggplot2::ylab("Bezugsgemeinschaft") +
    theme_frankmakrdiss() +
    ggplot2::theme(legend.position = "top")
  if (!tikzdevice) {
    gammaplot +
      ggplot2::xlab("\u03b3 - \u03b3\u0303") +
      ggplot2::guides(color = ggplot2::guide_legend(
        title = "87% HDR: ", reverse = TRUE))
  } else {
    gammaplot +
      ggplot2::xlab("$\\gamma_j - \\tilde{\\gamma}$") +
      ggplot2::guides(color = ggplot2::guide_legend(
        title = "$\\qty{87}{\\percent}\\operatorname{HDR}$: ",
          reverse = TRUE))
  }
}

#' Plot Pairwise Distribution Overlapping
#'
#' @description
#' `plot_compjitter` plots data
#' in the format from the function `make_compjitter`.
#'
#' @param compjitter A data frame returned from the function
#'   `make_compjitter`
#' @param tikzdevice Logical indicator for preparing the text for tikzDevice
#'   which defaults to `tikzdevice = FALSE`
#' @return The output will be a ggplot2 graph
#' @export
plot_compjitter <- function(compjitter, tikzdevice = FALSE) {
  compplot <- ggplot2::ggplot(compjitter,
      ggplot2::aes(x = .data$probs, y = .data$dims)) +
    ggplot2::geom_jitter(ggplot2::aes(fill = .data$fill_jitter,
      alpha = .data$alpha_jitter), shape = "circle filled", stroke = 0,
      size = 2, height = 0.2) +
    ggplot2::scale_fill_gradient2(low = colorscheme_frankmakrdiss[1, 5],
      mid = colorscheme_frankmakrdiss[6, 5],
      high = colorscheme_frankmakrdiss[1, 5], midpoint = 0.5,
        limits = c(0, 1), labels = scales::label_number(decimal.mark = ",")) +
    ggplot2::scale_x_continuous(limits = c(0, 1),
      labels = scales::label_number(decimal.mark = ",")) +
    ggplot2::ylab("Testdimension") +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE,
        barwidth = 0.8, title = "Quantilbereich", title.vjust = 3),
      alpha = "none") +
    theme_frankmakrdiss()
  if (!tikzdevice) {
    compplot +
      ggplot2::xlab("\u00dcberschneidungswahrscheinlichkeit (OVL)")
  } else {
    compplot +
      ggplot2::xlab(
        "\u00dcberschneidungswahrscheinlichkeit ($\\mathit{OVL}$)")
  }
}



# ----------------------
# ---                ---
# --- Graphs study 3 ---
# ---                ---
# ----------------------

#' Plot Posterior Retrodictive Check
#'
#' @description
#' `plot_dp_prcjitter` plots data
#' in the format from the function `make_dp_prcjitter`.
#'
#' @param prcjitter A data frame returned from the function
#'   `make_dp_prcjitter`
#' @return The output will be a ggplot2 graph
#' @export
plot_dp_prcjitter <- function(prcjitter) {
  ggplot2::ggplot(prcjitter, ggplot2::aes(x = .data$x, y = .data$y_rep)) +
  ggplot2::geom_jitter(ggplot2::aes(fill = .data$fill_jitter),
    shape = "circle filled", stroke = 0, size = 6, alpha = 0.1, width = 0.15,
    na.rm = TRUE) +
  ggplot2::scale_fill_gradient2(low = colorscheme_frankmakrdiss[1, 5],
    mid = colorscheme_frankmakrdiss[4, 5],
    high = colorscheme_frankmakrdiss[1, 5], midpoint = 0.5, limits = c(0, 1),
    labels = scales::label_number(decimal.mark = ",")) +
  ggplot2::geom_step(ggplot2::aes(
    x = .data$x - 0.25, y = .data$y_step, color = .data$color_step)) +
  ggplot2::scale_color_manual(values = c("#000000",
    colorscheme_frankmakrdiss[6, 5]),
    na.translate = FALSE) +
  ggplot2::scale_x_continuous(
    labels = scales::label_number(decimal.mark = ",")) +
  ggplot2::xlab("y-Werte") +
  ggplot2::ylab("H\u00e4ufigkeiten") +
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE,
      barwidth = 0.8, title = "Quantilbereich", title.vjust = 3),
    color = ggplot2::guide_legend(title = NULL, override.aes = list(size = 2))
    ) +
  theme_frankmakrdiss()
}

#' Plot Posterior Retrodictive Check
#'
#' @description
#' `plot_dp_prcstats` plots data
#' in the format from the function `make_dp_prcstats`.
#'
#' @param prcstats A data frame returned from the function
#'   `make_dp_prcstats`
#' @return The output will be a ggplot2 graph
#' @export
plot_dp_prcstats <- function(prcstats) {
  ggplot2::ggplot(prcstats, ggplot2::aes(x = .data$x, y = .data$y)) +
  ggplot2::geom_col(ggplot2::aes(fill = .data$fill),
    color = colorscheme_frankmakrdiss[1, 5], width = 0.1) +
  ggplot2::geom_vline(ggplot2::aes(
    xintercept = .data$stat_y, color = "Observationen")) +
  ggplot2::scale_color_manual(values = "#000000") +
  ggplot2::scale_fill_manual(values = c("#ffffff",
    colorscheme_frankmakrdiss[2, 5])) +
  ggplot2::scale_x_continuous(
    labels = scales::label_number(decimal.mark = ",")) +
  ggplot2::xlab(NULL) +
  ggplot2::ylab("H\u00e4ufigkeiten") +
  ggplot2::guides(
    color = ggplot2::guide_legend(title = NULL,
      override.aes = list(size = 1.5)),
    fill = ggplot2::guide_legend(title = NULL)) +
  ggplot2::facet_wrap(ggplot2::vars(.data$stat), scales = "free_x") +
  theme_frankmakrdiss()
}

#' Plot Cluster Probabilities
#'
#' @description
#' `plot_lambdapointrange` plots data
#' in the format from the function `make_lambdapointrange`.
#'
#' @param lambdapointrange A data frame returned from the function
#'   `make_lambdapointrange`
#' @param tikzdevice Logical indicator for preparing the text for tikzDevice
#'   which defaults to `tikzdevice = FALSE`
#' @return The output will be a ggplot2 graph
#' @export
plot_lambdapointrange <- function(lambdapointrange, tikzdevice = FALSE) {
lambdaplot <- ggplot2::ggplot(lambdapointrange,
    ggplot2::aes(x = .data$x, y = .data$y, color = .data$color)) +
  ggplot2::geom_linerange(ggplot2::aes(xmin = .data$ll, xmax = .data$ul),
    linewidth = 1.5, position = ggplot2::position_dodge(width = 0.7),
    key_glyph = "path") +
  ggplot2::geom_point(ggplot2::aes(color = .data$color), size = 2,
    position = ggplot2::position_dodge(width = 0.7)) +
  ggplot2::facet_wrap(ggplot2::vars(.data$dim), dir = "v") +
  ggplot2::scale_color_manual(values = scales::alpha(as.vector(
    colorscheme_frankmakrdiss[c(1, 2, 5), 5]), c(0.3, 1, 1))) +
  ggplot2::scale_x_continuous(limits = c(0, 1),
    labels = scales::label_number(decimal.mark = ",")) +
  ggplot2::ylab("Cluster") +
  theme_frankmakrdiss()
  if (!tikzdevice) {
    lambdaplot +
      ggplot2::xlab("p") +
      ggplot2::guides(color = ggplot2::guide_legend(title = "87% HDR: ",
        reverse = TRUE))
  } else {
    lambdaplot +
      ggplot2::xlab("$p$") +
      ggplot2::guides(color = ggplot2::guide_legend(
        title = "$\\qty{87}{\\percent}\\operatorname{HDR}$: ",
        reverse = TRUE))
  }
}

#' Plot Cluster Densities
#'
#' @description
#' `plot_logclusterdensities` plots data
#' in the format from the function `make_logclusterdensities`.
#'
#' @param lcddata A data frame returned from the function
#'   `make_logclusterdensities`
#' @param tikzdevice Logical indicator for preparing the text for tikzDevice
#'   which defaults to `tikzdevice = FALSE`
#' @return The output will be a ggplot2 graph
#' @export
plot_logclusterdensities <- function(lcddata, tikzdevice = FALSE) {
  lcdplot <- ggplot2::ggplot(lcddata, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$density), height = 0.5) +
    ggplot2::facet_wrap(ggplot2::vars(.data$dim), dir = "v") +
    ggplot2::xlab("Cluster") +
    ggplot2::ylab("Bezugsgemeinschaft") +
    theme_frankmakrdiss()
  if (!tikzdevice) {
    lcdplot +
      ggplot2::scale_fill_stepsn(
        breaks = c("-14,84" = -14.84, "-9,35" = -9.35, "-3,91" = -3.91),
        limits = c("-\u221e" = min(lcddata$density), "0" = 0),
        colors = scales::alpha(
          colorscheme_frankmakrdiss[c(1, 1, 2, 5), 5], c(0.1, 0.1, 0.5, 1))) +
      ggplot2::guides(fill = ggplot2::guide_colorsteps(title = "log(Dichte)",
        show.limits = TRUE, title.vjust = 1.8, barwidth = 0.5))
  } else {
    lcdplot +
      ggplot2::scale_fill_stepsn(
        breaks = c("$\\num{-14.84}$" = -14.84, "$\\num{-9.35}$" = -9.35,
          "$\\num{-3.91}$" = -3.91),
        limits = c("$-\\infty$" = min(lcddata$density), "$0$" = 0),
        colors = scales::alpha(
          colorscheme_frankmakrdiss[c(1, 1, 2, 5), 5], c(0.1, 0.1, 0.5, 1))) +
      ggplot2::guides(fill = ggplot2::guide_colorsteps(
        title = "$\\log(\\text{Dichte})$", show.limits = TRUE,
        title.vjust = 1.8, barwidth = 0.5))
  }
}

#' Plot MDS Configuration
#'
#' @description
#' `plot_mds` plots data
#' in the format from the function `make_mds_plotdata`.
#'
#' @param mds_plotdata A data frame returned from the function
#'   `make_mds_plotdata`
#' @param mds_color_groups A numerical vector of `length= 4`
#'   containing the numbers of the communities in `conval_comms`
#'   to be highlighted
#' @return The output will be a ggplot2 graph
#' @export
plot_mds <- function(mds_plotdata, mds_color_groups) {
  ggplot2::ggplot(mds_plotdata[[1]], ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_polygon(data = mds_plotdata[[2]][
      !mds_plotdata[[2]]$group %in% mds_color_groups, ],
      ggplot2::aes(group = .data$contour_group, fill = .data$hdr_label),
      alpha = 0.1, show.legend = FALSE) +
    ggplot2::scale_fill_gradient(low = "grey70", high = "grey95") +
    ggnewscale::new_scale("fill") +
    ggplot2::geom_polygon(data = mds_plotdata[[2]][
      mds_plotdata[[2]]$group == mds_color_groups[1], ],
      ggplot2::aes(group = .data$contour_group, fill = .data$hdr_label),
      alpha = 0.1, show.legend = FALSE) +
    ggplot2::scale_fill_gradient(low = colorscheme_frankmakrdiss[4, 2],
      high = colorscheme_frankmakrdiss[1, 2]) +
    ggnewscale::new_scale("fill") +
    ggplot2::geom_polygon(data = mds_plotdata[[2]][
      mds_plotdata[[2]]$group == mds_color_groups[2], ],
      ggplot2::aes(group = .data$contour_group, fill = .data$hdr_label),
      alpha = 0.1, show.legend = FALSE) +
    ggplot2::scale_fill_gradient(low = colorscheme_frankmakrdiss[4, 3],
      high = colorscheme_frankmakrdiss[1, 3]) +
    ggnewscale::new_scale("fill") +
    ggplot2::geom_polygon(data = mds_plotdata[[2]][
      mds_plotdata[[2]]$group == mds_color_groups[3], ],
      ggplot2::aes(group = .data$contour_group, fill = .data$hdr_label),
      alpha = 0.1, show.legend = FALSE) +
    ggplot2::scale_fill_gradient(low = colorscheme_frankmakrdiss[4, 4],
      high = colorscheme_frankmakrdiss[1, 4]) +
    ggnewscale::new_scale("fill") +
    ggplot2::geom_polygon(data = mds_plotdata[[2]][
      mds_plotdata[[2]]$group == mds_color_groups[4], ],
      ggplot2::aes(group = .data$contour_group, fill = .data$hdr_label),
      alpha = 0.1) +
    ggplot2::scale_fill_gradient(low = colorscheme_frankmakrdiss[4, 5],
      high = colorscheme_frankmakrdiss[1, 5],
      labels = scales::label_number(decimal.mark = ",")) +
    ggplot2::scale_alpha_continuous(range = c(0.5, 0.1)) +
    ggplot2::geom_point(data = mds_plotdata[[1]][
      !mds_plotdata[[1]]$group %in% mds_color_groups, ], color = "grey60") +
    ggrepel::geom_text_repel(data = mds_plotdata[[1]][
      mds_plotdata[[1]]$group %in% mds_color_groups, ],
      ggplot2::aes(label = .data$label),
      max.overlaps = Inf, min.segment.length = 0, direction = "both",
      box.padding = 1, force = 0.6, segment.ncp = 3, segment.curvature = 0.1,
      size = 3) +
    ggplot2::geom_point(data = mds_plotdata[[1]][
      mds_plotdata[[1]]$group %in% mds_color_groups, ],
      ggplot2::aes(color = .data$group), show.legend = FALSE) +
    ggplot2::scale_color_manual(
      values = unname(colorscheme_frankmakrdiss[5, c(2:5)])) +
    ggplot2::facet_wrap(ggplot2::vars(.data$dim), dir = "v") +
    ggplot2::scale_x_continuous(limits = c(-7, 6), breaks = seq(-6, 6, 2)) +
    ggplot2::scale_y_continuous(limits = c(-4, 4)) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(title = "HDR", title.vjust = 0.7,
        barheight = 7, barwidth = 0.5), draw.llim = "none",
        draw.ulim = "none",
      limits = c(0.5, 0.95)) +
    ggplot2::xlab("MDS Dimension 1") +
    ggplot2::ylab("MDS Dimension 2") +
    theme_frankmakrdiss()
}
