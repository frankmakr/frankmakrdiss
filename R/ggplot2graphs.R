#' ggplot2 colorscheme
#'
#' Utility data
colorscheme_frankmakrdiss <- matrix(
  c("#dcbccc", "#c799b0", "#b97c9b", "#a25079", "#8f275b", "#7c003e",
    "#bcdcdc", "#99c7c7", "#7cb9b9", "#50a2a2", "#278f8f", "#007c7c",
    "#fbf3da", "#f8e8b5", "#f5dc90", "#dbc376", "#aa975c", "#7a6c42",
    "#d1e1ec", "#b3cde0", "#6497b1", "#005b96", "#03396c", "#011f4b",
    "#dcbcbc", "#c79999", "#b97c7c", "#a25050", "#8f2727", "#7c0000"),
  ncol = 5,
  dimnames = list(
    c("light", "light_highlight",
      "mid", "mid_highlight",
      "dark", "dark_highlight"),
    c("pink", "teal", "yellow", "blue", "red"))
  )

#' ggplot2 theme
#'
#' Utility function
#' @noRd
theme_frankmakrdiss <- function(...) {
  ggplot2::theme_grey() %+replace%
  ggplot2::theme(
    axis.line = ggplot2::element_line(color = "black"),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_line(color = "gray96"),
    legend.key = ggplot2::element_blank()
    )
}

#' Plot Sample Characteristics
#'
#' @description
#' \code{plot_samplepointrange} plots data
#' in the format from the function \code{make_samplepointrange}.
#'
#' @param samplepointrange A data frame returned from the function
#'   \code{make_samplepointrange}
#' @return The output will be a ggplot2 graph
#' @export
plot_samplepointrange <- function(samplepointrange) {
  ggplot2::ggplot(samplepointrange,
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
    ggplot2::xlab(
      "Abweichung vom Median der Studierendenstatistik in \\si{\\percent}") +
    ggplot2::ylab(NULL) +
    ggplot2::guides(color = ggplot2::guide_legend(
      title = "$\\qty{87}{\\percent}\\operatorname{HDR}$: ",
      reverse = TRUE)) +
    theme_frankmakrdiss() +
    ggplot2::theme(legend.position = "top")
}

#' Plot Posterior Retrodictive Check
#'
#' @description
#' \code{plot_prcjitter} plots data
#' in the format from the function \code{make_prcjitter}.
#'
#' @param prcjitter A data frame returned from the function
#'   \code{make_prcjitter}
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
#' \code{plot_prcstats} plots data
#' in the format from the function \code{make_prcstats}.
#'
#' @param prcstats A data frame returned from the function
#'   \code{make_prcstats}
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

#' Plot Variance Components
#'
#' @description
#' \code{plot_sigmapointrange} plots data
#' in the format from the function \code{make_sigmapointrange}.
#'
#' @param sigmapointrange A data frame returned from the function
#'   \code{make_sigmapointrange}
#' @return The output will be a ggplot2 graph
#' @export
plot_sigmapointrange <- function(sigmapointrange) {
  ggplot2::ggplot(sigmapointrange,
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
    ggplot2::xlab("$\\tilde{\\sigma}$") +
    ggplot2::ylab("Item") +
    ggplot2::guides(color = ggplot2::guide_legend(
      title = "$\\qty{87}{\\percent}\\operatorname{HDR}$: ", reverse = TRUE)) +
    theme_frankmakrdiss() +
    ggplot2::theme(legend.position = "top")
}

#' Plot Community Effects
#'
#' @description
#' \code{plot_gammapointrange} plots data
#' in the format from the function \code{make_gammapointrange}.
#'
#' @param gammapointrange A data frame returned from the function
#'   \code{make_gammapointrange}
#' @return The output will be a ggplot2 graph
#' @export
plot_gammapointrange <- function(gammapointrange) {
  ggplot2::ggplot(gammapointrange, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_vline(xintercept = 0, color = "gray70") +
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
    ggplot2::xlab("$\\gamma_j - \\tilde{\\gamma}$") +
    ggplot2::ylab("Bezugsgemeinschaft") +
    ggplot2::guides(color = ggplot2::guide_legend(
      title = "$\\qty{87}{\\percent}\\operatorname{HDR}$: ", reverse = TRUE)) +
    theme_frankmakrdiss() +
    ggplot2::theme(legend.position = "top")
}

#' Plot Pairwise Distribution Overlapping
#'
#' @description
#' \code{plot_compjitter} plots data
#' in the format from the function \code{make_compjitter}.
#'
#' @param compjitter A data frame returned from the function
#'   \code{make_compjitter}
#' @return The output will be a ggplot2 graph
#' @export
plot_compjitter <- function(compjitter) {
  ggplot2::ggplot(compjitter, ggplot2::aes(x = .data$probs, y = .data$dims)) +
    ggplot2::geom_jitter(ggplot2::aes(fill = .data$fill_jitter,
      alpha = .data$alpha_jitter), shape = "circle filled", stroke = 0,
      size = 2, height = 0.2) +
    ggplot2::scale_fill_gradient2(low = colorscheme_frankmakrdiss[1, 5],
      mid = colorscheme_frankmakrdiss[6, 5],
      high = colorscheme_frankmakrdiss[1, 5], midpoint = 0.5,
        limits = c(0, 1), labels = scales::label_number(decimal.mark = ",")) +
    ggplot2::scale_x_continuous(limits = c(0, 1),
      labels = scales::label_number(decimal.mark = ",")) +
    ggplot2::xlab("\u00dcberschneidungswahrscheinlichkeit ($\\mathit{OVL}$)") +
    ggplot2::ylab("Testdimension") +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE,
        barwidth = 0.8, title = "Quantilbereich", title.vjust = 3),
      alpha = "none") +
    theme_frankmakrdiss()
}
