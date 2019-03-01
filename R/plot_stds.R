#' Plot standards
#'
#' Plot the standards in the familiar \eqn{\delta}{δ} vs \eqn{\Delta}{Δ} plot.
#'
#' @param dat A [tibble][tibble::tibble-package], resulting from [acid_fractionation()]
#' @export
plot_stds <- function(dat, x = d47_mean, y = D47_final, ...) {
  x <- enquo(x)
  y <- enquo(y)

  dat %>%
    plot_base(x = !! x, y = !! y, ...) +
    geom_point()# +
    ## labs(x = delta[47]~"(\u2030)", y = Delta[47]~"(\u2030)")
    # currently excluded because it throws errors in plotly::ggplotly()
}
