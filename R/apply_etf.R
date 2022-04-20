#' Apply the ETF
#'
#' Uses the previously calculated intercept and slope, and uses them to
#' calculate the \eqn{\Delta_{47}} values.
#'
#' @details Note that the intercept and slope were calculated with the
#'   dependent and independent variables in the other direction, so we flip
#'   them here. i.e.: \deqn{\Delta_{47etf} = - (\alpha / \beta) + (1 / \beta)
#'   \times \Delta_{47_raw}}
#'
#' @param .data A [tibble][tibble::tibble-package] containing column D47.
#' @param intercept The column name with the ETF intercept.
#' @param slope The column name with the ETF slope.
#' @param raw The column with raw values to apply the ETF to.
#' @param out The new column name.
#' @family empirical transfer functions
#' @export
apply_etf <- function(.data, intercept = intercept, slope = slope, raw = D47_raw, out = D47_etf, quiet = default(quiet)) {
  # defaults
  D47_raw <- D47_etf <- NULL

  if (!quiet)
    glue("Info: Applying ETF to {quo_name(enquo(raw))} using \u03b1 = {quo_name(enquo(slope))} and \u03b2 = {quo_name(enquo(intercept))}.") %>%
      message()

  .data %>%
    mutate({{ out }} := - ({{ intercept }} / {{ slope }}) + (1 / {{ slope }}) * {{ raw }})
}
