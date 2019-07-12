#' Calculate the Empirical Transfer Function
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param outlier Column with sample outlier information. Looks up string `"no_outlier"`.
#' @param raw Column name of raw \eqn{\Delta_{47}}{Δ47} values.
#' @param exp Column name of expected \eqn{\Delta_{47}}{Δ47} values.
#' @param session The column name to group analyses by. Defaults to
#'   `Preparation`.
#' @export
#' @importFrom stats na.exclude
calculate_etf <- function(.data, outlier = outlier, raw = D47_raw_mean, exp = expected_D47,
                          session = Preparation, quiet = default(quiet)) {
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
    group_by({{ session }}) %>%
    nest() %>%
    mutate(etf = map(data, pos_lm),
           etf_coefs=map(.data$etf, "coefficients"),
           intercept=map_dbl(.data$etf_coefs, 1),
           slope=map_dbl(.data$etf_coefs, 2)) %>%
    ## select(-etf_coefs) %>%
    unnest(cols = .data$data)
}
