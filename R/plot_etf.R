#' Plot the Empirical Transfer Function
#'
#' Create a plot with the expected \eqn{\Delta_{47}}{Δ47} value on the x-axis
#' and the \eqn{\Delta_{47}}{Δ47} derrived from the ETF on the y-axis.
#'
#' @inheritParams calculate_etf
#' @param std_names The names of the standards.
#' @param out The column name that holds the transformed values.
#' @param raw The column name with raw data.
#' @param exp The column name with expected data.
#' @param shape The column of outliers, mapped to the shape aesthetic.
#' @param point_alpha The alpha of the points to plot.
#' @param session The session to facet by. Defaults to `Preparation`. after the
#'   application of the empirical transfer function.
#' @family empirical transfer functions
#' @export
plot_etf <- function(.data, std_names = paste0("ETH-", 1:3),
                     out = D47_etf,
                     raw = D47_raw,
                     exp = expected_D47,
                     outlier = outlier,
                     point_alpha = .5,
                     session = Preparation) {
  # global variables and defaults
  expected_D47 <- D47_raw <- Preparation <- NULL

  pld <- .data %>%
    mutate({{ exp }} := ifelse(is.na({{ exp }} & !.data$broadid %in% std_names),
                               {{ out }}, {{ exp }}))

  pld %>%
    plot_base(x = {{ exp }}, y = {{ raw }}, shape = {{ outlier }}) +
    geom_smooth(aes(group = {{ session }}, colour = NULL),
                method = "lm",
                data = filter(pld, .data$broadid %in% std_names, {{ outlier }} == FALSE),
                fullrange = TRUE) +
    geom_point(alpha = point_alpha)
}
