#' Calculate the Empirical Transfer Function
#'
#' @param .data A [tibble][tibble::tibble-package].
# #' @param outlier Column with sample outlier information. Looks up string `"no_outlier"`.
#' @param raw Column name of the raw data.
#' @param exp Column name of expected \eqn{\Delta_{47}}{Δ47} values.
#' @param session The column name to group analyses by. Defaults to
#'   `Preparation`.
#' @param etf The column name of the new model.
#' @param etf_coefs The column name holding the coefficients of the model.
#' @param slope The column name of the new slope.
#' @param intercept The column name of the new intercept.
#' @export
#' @importFrom stats na.exclude
calculate_etf <- function(.data, raw = D47_raw_mean, exp = expected_D47,
                          session = Preparation, etf = etf,
                          etf_coefs = etf_coefs, slope = slope,
                          intercept = intercept, quiet = default(quiet)) {
  # global variables and defaults
  outlier <- D47_raw_mean <- expected_D47 <- Preparation <- NULL

  # this makes it continue even if lm fails.
  pos_lm <- purrr::possibly(
    function(x) {
      x %>%
      filter(!.data$outlier & !is.na(.data$outlier)) %>%
      lm(stats::formula(paste(quo_name(enquo(raw)), "~", quo_name(enquo(exp)))),
         data = ., na.action = na.exclude)
    },
    otherwise = NA_real_, quiet = quiet)

  pos_tidy <- purrr::possibly(broom::tidy, otherwise=NA_real_, quiet=quiet)
  pos_map_dbl <- purrr::possibly(map_dbl, otherwise=NA_real_, quiet=quiet)

  .data %>%
    nest(session_nested = -{{ session }}) %>%
    mutate({{ etf }} := map(session_nested, pos_lm),
           {{ etf_coefs }} := map({{ etf }}, "coefficients"),
           {{ intercept }} := map_dbl({{ etf_coefs }}, 1),
           {{ slope }} := map_dbl({{ etf_coefs }}, 2)) %>%
    unnest(cols = .data$session_nested)
}
