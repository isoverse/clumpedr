#' Create a plot of \eqn{\Delta_{47}}{Δ47} values as a function of measurement
#' time.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from [temperature_calculation()].
# TODO: inherit aesthetics from geom_point
#' @inheritParams ggplot2::geom_point
#' @param ... Additional aesthetics to pass to [plot_base()].
#' @export
plot_delta <- function(dat, x = file_datetime, y = D47_final, ...) {
  x <- enquo(x)
  y <- enquo(y)
  dat %>%
    plot_base(x = !! x, y = !! y, ...) +
    geom_point()
}
