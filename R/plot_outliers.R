#' Plot outliers
#'
#' This function plots raw delta values that are outliers, based on several criteria.
#'
#' @param dat A [tibble][tibble::tibble-package] resulting from [find_outliers()].
#' @param x Column to put on the x axis.
#' @param y Column to put on the y ayis.
#' @export
#' @seealso find_outliers
plot_outliers <- function(dat, x = file_datetime, y = D47_raw_mean) {
  # global variables and defaults
  file_datetime <- D47_raw_mean <- outlier <- sess_id1_mean <- sess_id1_med <-
    sess_id1_sd <- sess_id1_n <- sess_mean <- sess_sd <- sess_n <- NULL

  x <- enquo(x)
  y <- enquo(y)

  dat %>%
    plot_base(x = !! x, y = !! y, shape = outlier,
              sess_id1_mean = sess_id1_mean, sess_id1_med = sess_id1_med,
              sess_id1_sd = sess_id1_sd, sess_id1_n = sess_id1_n,
              sess_mean = sess_mean, sess_sd = sess_sd, sess_n = sess_n) +
    geom_point()
}
