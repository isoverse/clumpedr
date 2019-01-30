#' plot_delta
#'
#' @param dat A [tibble][tibble::tibble-package], resulting from `temperature_calculation`
#' @param col Target column.
plot_delta <- function(dat, col = quo(D47_final)) {
  dat %>%
      plot_base() + geom_point(aes(x = file_datetime, y = !!col))
}
