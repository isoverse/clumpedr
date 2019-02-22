#' Plot standards
#'
#' Plot the standards in the familiar \eqn{\delta} vs \eqn{\Delta} plot.
#'
#' @param dat A [tibble][tibble::tibble-package], resulting from [acid_fractionation()]
#' @param parameter parameter description
#' @export
plot_stds <- function(dat, parameter) {
  dat %>%
    plot_base(x = d47_mean, y = D47_final, colour = broadid) +
    geom_point() +
    labs(x = delta[47]~"(\u2030)", y = Delta[47]~"(\u2030)")
}
