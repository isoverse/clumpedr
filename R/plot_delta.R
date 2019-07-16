#' Create a plot of \eqn{\Delta_{47}}{Δ47} values as a function of measurement
#' time.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from
#'   [temperature_calculation()].
#' @param x What to put on the x axis.
#' @param y What to put on the y axis.
#' @param fill The fill aesthetic for the violin plots, defaults to broadid;
#'   same as colour.
#' @param shape The shape aesthetic for the violin plots, defaults to outlier.
#'   Also filters based on this for violin.
#' @param ... Additional aesthetics to pass to [plot_base()].
#' @export
plot_delta <- function(.data, x = file_datetime, y = D47_final, fill = broadid, shape = outlier, ...) {
  # global variables and defaults
  file_datetime <- D47_final <- broadid <- outlier <- NULL

  .data %>%
    plot_base(x = {{ x }}, y = {{ y }}, fill = {{ fill }}, shape = {{ shape }}, ...) +
    geom_violin(aes(group = .data$file_id), alpha = .5, data = filter(.data, {{ shape }})) +
    geom_point(alpha = .5) +
    labs(fill = "Broad Identifier")
}
