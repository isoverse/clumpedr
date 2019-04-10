#' Calculate the Empirical Transfer Function
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param cycle_dis Column with disabled cycle information. Looks up string `"no_drop"`.
#' @param outlier Column with sample outlier information. Looks up string `"no_outlier"`.
#' @param raw Column name of raw \eqn{\Delta_{47}}{Δ47} values.
#' @param exp Column name of expected \eqn{\Delta_{47}}{Δ47} values.
#' @param session The column name to group analyses by. Defaults to
#'   `Preparation`.
#' @export
calculate_etf <- function(.data, cycle_dis = cycle_dis, outlier = outlier, raw = D47_raw_mean, exp = expected_D47,
                          session = Preparation, quiet = default(quiet)) {
  # global variables and defaults
  cycle_dis <- outlier <- D47_raw_mean <- expected_D47 <- Preparation <- `(Intercept)` <-
    term <- estimate <- intercept <- slope <- NULL

  raw <- enquo(raw)
  exp <- enquo(exp)

  session <- enquo(session)

  # this makes it continue even if lm fails.
  pos_lm <- purrr::possibly(lm, otherwise = NA, quiet = quiet)

  etf <- .data %>%
    # filter out bad cycles and outliers
    # TODO: make this procedure operate on nested tibbles! <3
    # http://stat545.com/block024_group-nest-split-map.html
    filter(cycle_dis == "no_drop",
           outlier == "no_outlier") %>%
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
