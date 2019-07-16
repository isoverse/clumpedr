#' Plot outliers
#'
#' This function plots raw delta values that are outliers, based on several criteria.
#'
#' @param dat A [tibble][tibble::tibble-package] resulting from [find_outliers()].
#' @param x X aesthetic. Defaults to `file_datetime`.
#' @param y Y aesthetic. Defaults to `D47_raw`.
#' @param shape Shape aesthetic. Defaults to `outlier`.
#' @export
#' @seealso find_outliers
plot_outliers <- function(dat, x = file_datetime, y = D47_raw, shape = outlier) {
  # global variables and defaults
  file_datetime <- D47_raw <- outlier <- NULL

  plot_base(dat, x = {{ x }}, y = {{ y }}, shape = {{ shape }}) +
    geom_point(alpha = .2) +
    stat_summary(geom = "pointrange", fun.data = mean_cl_normal)
}
