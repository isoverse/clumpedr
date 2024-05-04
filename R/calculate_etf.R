#' Calculate the Empirical Transfer Function
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param raw Column name of raw \eqn{\Delta_{47}} values.
#' @param exp Column name of expected \eqn{\Delta_{47}} values.
#' @param session The column name to group analyses by. Defaults to
#'   `Preparation`.
#' @param etf The column name of the new model.
#' @param etf_coefs The column name with the coefficients of the model.
#' @param slope The column name of the new slope.
#' @param intercept The column name of the new intercept.
#' @param parallel Whether or not (default) to process this in parallel, using package `furrr`.
#' @importFrom stats na.exclude
#' @inheritParams dots
#' @inheritParams quiet
#' @family empirical transfer functions
#' @export
calculate_etf <- function(.data, ...,
                          raw = D47_raw_mean, exp = expected_D47,
                          session = Preparation, etf = etf,
                          etf_coefs = etf_coefs, slope = slope,
                          intercept = intercept, parallel = FALSE, quiet = NULL) {
  # global variables and defaults
  if (parallel & !requireNamespace("furrr", quietly = TRUE)) {
    stop("Package \"furrr\" is needed for this function to work.\n Please install it or run this with `parallel = FALSE`",
      call. = FALSE)
  }
  if (is.null(quiet)) {
    quiet <- default(quiet)
  }

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

  if (!quiet)
    glue("Info: Calculating ETF with {quo_name(enquo(raw))} as a function of {quo_name(enquo(exp))} for each {quo_name(enquo(session))}.") %>%
      message()

  if (parallel) {
    out <- .data %>%
      nest(.by = {{ session }}, .key = "session_nested") %>%
      mutate({{ etf }} := furrr::future_map(.data$session_nested, pos_lm),
      {{ etf_coefs }} := furrr::future_map({{ etf }}, "coefficients"),
      {{ intercept }} := furrr::future_map_dbl({{ etf_coefs }}, 1),
      {{ slope }} := furrr::future_map_dbl({{ etf_coefs }}, 2)) %>%
      unnest(cols = "session_nested")
  } else {
    out <- .data %>%
      nest(.by = {{ session }}, .key = "session_nested") %>%
      ## nest(session_nested = -{{ session }}) %>%
      mutate({{ etf }} := map(.data$session_nested, pos_lm),
      {{ etf_coefs }} := map({{ etf }}, "coefficients"),
      {{ intercept }} := pos_map_dbl({{ etf_coefs }}, 1),
      {{ slope }} := pos_map_dbl({{ etf_coefs }}, 2)) %>%
      unnest(cols = "session_nested")
  }

  out
}

pos_tidy <- purrr::possibly(broom::tidy, otherwise = NA_real_, quiet = FALSE)
pos_map_dbl <- purrr::possibly(map_dbl, otherwise = NA_real_, quiet = FALSE)
