#' Plot the Empirical Transfer Function
#'
#' Create a plot with the expected \eqn{\Delta_{47}}{Δ47} value on the x-axis
#' and the \eqn{\Delta_{47}}{Δ47} derrived from the ETF on the y-axis.
#'
#' @inheritParams calculate_etf
#' @param std_names The names of the standards.
#' @param D47_etf The column name that holds the \eqn{\Delta_{47}}{Δ47} values
#'   after the application of the empirical transfer function.
#' @param raw The column name with raw \eqn{\Delta_{47}}{Δ47} values.
#' @param exp The column name with expected \eqn{\Delta_{47}}{Δ47} values.
#' @param session The session to facet by. Defaults to `Preparation`.
#' @family empirical transfer functions
#' @export
plot_etf <- function(.data, std_names = paste0("ETH-", 1:3),
                     D47_etf = D47_etf,
                     raw = D47_raw,
                     exp = expected_D47,
                     session = Preparation) {
  # global variables and defaults
  expected_D47 <- D47_raw <- Preparation <- NULL

  pld <- .data %>%
    mutate({{ exp }} := ifelse(is.na({{ exp }} & !.data$broadid %in% std_names), {{ D47_etf }}, {{ exp }}))

  pld %>%
    plot_base(x = {{ exp }}, y = {{ raw }}) +
    geom_point() +
    geom_smooth(aes(group = "yes"), method = "lm",
                data = filter(pld, .data$broadid %in% std_names, .data$outlier == FALSE)) +
    facet_grid(rows = vars({{ session }}))
}
