#' plot_outliers
#'
#' This function plots raw delta values that are outliers, based on several criteria.
#'
#' @param dat A [tibble][tibble::tibble-package], resulting from [add_info()]
#' @param col The quoted column that should be plotted.
plot_outliers <- function(dat, col = quo(D47raw_mean)) {
  dat %>%
      plot_base() +
      geom_point(aes(x = file_datetime, y = !!col, shape = outlier,
                     run_mean = run_mean, run_sd = run_sd, run_n = run_n,
                     mean_run_std_sd = mean_run_std_sd), size = 3)
}
