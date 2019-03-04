#' Create a plot of \eqn{\Delta_{47}}{Δ47} values as a function of measurement
#' time.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from [temperature_calculation()].
#' @param x What to put on the x axis.
#' @param y What to put on the y axis.
#' @param ... Additional aesthetics to pass to [plot_base()].
#' @export
plot_delta <- function(.data, x = file_datetime, y = D47_final, ...) {
  # global variables and defaults
  file_datetime <- D47_final <- NULL

  x <- enquo(x)
  y <- enquo(y)
  .data %>%
    plot_base(x = !! x, y = !! y, ...) +
    geom_point()
}
