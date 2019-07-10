#' Apply the ETF
#'
#' Uses the previously calculated intercept and slope, and uses them to
#' calculate the \eqn{\Delta_{47}}{Δ47} values.
#'
#' @details Note that the intercept and slope were calculated with the
#'   dependent and independent variables in the other direction, so we flip
#'   them here. i.e.: \deqn{\Delta_{47etf} = - (\alpha / \beta) + (1 / \beta)
#'   \times \Delta_{47_raw}}{Δ47_etf = - (α / β) + (1 / β) * Δ47_raw}
#'
#' @param .data A [tibble][tibble::tibble-package] containing column D47.
#' @param D47 The column with \eqn{\Delta_{47}}{Δ47} values to use.
#' @param D47_out The new column name.
#' @family empirical transfer functions
#' @export
apply_etf <- function(.data, intercept=intercept, slope=slope, D47 = D47_raw, D47_out = D47_etf) {
  # global variables and defaults
  D47_raw <- intercept <- slope <- D47_etf <- NULL

  D47 <- enquo(D47)
  D47_out <- enquo(D47_out)

  .data %>%
    mutate(!! D47_out := - (intercept / slope) + (1 / slope) * !! D47)
}
