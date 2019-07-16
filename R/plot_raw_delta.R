#' Plot raw delta values as a function of measurement time.
#'
#' Cycles are averaged and pointranges indicate one standard deviation offset
#' based on the number of cycles taken into account.
#'
#' @details Note that with \eqn{\delta^{13}}C and \eqn{\delta^{18}}O, the
#'   pointrange is invisible because the cycles have so little variance that
#'   the line segment usually falls within the dots.
#'
#' @param .data The input tibble, result of [bulk_and_clumping_deltas()].
#' @param .info The metadata of the raw cycles, used to get the `file_datetime`.
#' @param x X aesthetic. Defaults to file_datetime.
#' @param y Y aesthetic. Defaults to D47_raw.
#' @param shape Shape aesthetic. Defaults to `outlier_cycle`.
#' @param raw_points Logical, whether to include individual cycle estimates (defaults to `FALSE`).
#' @param point_alpha The alpha value of the raw points if they are plotted.
#' @export
plot_raw_delta <- function(.data, .info, x = file_datetime, y = D47_raw, shape = outlier_cycle, raw_points = FALSE,
                           point_alpha = .5, quiet = default(quiet)) {
  # global variables and defaults
  file_datetime <- D47_raw <- outlier_cycle <- NULL

  if (!quiet)
    glue("Info: generating a plot of raw delta value {quo_name(enquo(y))} as a function of {quo_name(enquo(x))}.") %>%
      message()

  plotdat <- .data %>%
    ## select(file_id, {{ y }}) %>%
    left_join(.info, by = "file_id") %>%
    group_by("file_id")

  pl <- plotdat %>%
    plot_base(x = {{ x }}, y = {{ y }}, shape = {{ shape }}) +
    stat_summary(fun.data = mean_cl_normal,
                 ## fun.ymin = function(x) mean(x) - sd(x),
                 ## fun.ymax = function(x) mean(x) + sd(x),
                 geom = "pointrange")

  if (raw_points) {
    pl <- pl + geom_point(alpha = point_alpha)
  }

  pl
}
