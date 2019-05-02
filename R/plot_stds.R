#' Plot standards
#'
#' Plot the standards in the familiar \eqn{\delta}{δ} vs \eqn{\Delta}{Δ} plot.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from [acid_fractionation()]
#' @param x Column to put on the x axis.
#' @param y Column to put on the y ayis.
#' @param ... Additional aesthetics to pass to [plot_base()].
#' @export
plot_stds <- function(.data, x = d47_mean, y = D47_final, ...) {
  # global variables and defaults
  d47_mean <- D47_final <- NULL

  x <- enquo(x)
  y <- enquo(y)

  .data %>%
    plot_base(x = !! x, y = !! y, ...) +
    geom_point()# +
    ## labs(x = delta[47]~"(\u2030)", y = Delta[47]~"(\u2030)")
    # currently excluded because it throws errors in plotly::ggplotly()
}
