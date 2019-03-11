#' Calculate the Empirical Transfer Function
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param raw Column name of raw \eqn{\Delta_{47}}{Δ47} values.
#' @param exp Column name of expected \eqn{\Delta_{47}}{Δ47} values.
#' @param session The column name to group analyses by. Defaults to
#'   `Preparation`.
#' @export
calculate_etf <- function(.data, raw = D47_raw_mean, exp = expected_D47,
                          session = Preparation, quiet = default(quiet)) {
  # global variables and defaults
  D47_raw_mean <- expected_D47 <- Preparation <- `(Intercept)` <-
    term <- estimate <- intercept <- slope <- NULL

  raw <- enquo(raw)
  exp <- enquo(exp)

  session <- enquo(session)

  # this makes it continue even if lm fails.
  pos_lm <- purrr::possibly(lm, otherwise = NA, quiet = quiet)

  etf <- .data %>%
    group_by(!! session) %>%
    do(model = broom::tidy(pos_lm(stats::formula(paste(quo_name(raw), "~", quo_name(exp))),
                                  data = ., na.action = na.omit))) %>%
    unnest() %>%
    select(!! session, term, estimate) %>%
    spread(term, estimate) %>%
    select(!! session, intercept = `(Intercept)`, slope = expected_D47) %>%
    right_join(.data, by = quo_name(session))

  etf
}
