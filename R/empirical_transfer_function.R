#' Empirical Transfer Function
#'
#' Calculate and apply the empirical transfer function (ETF). First append the
#' expected values for the standards, then calculate the ETF per Preparation,
#' then apply the ETF to all the samples and standards.
#'
#' @param .data A [tibble][tibble::tibble-package] resulting from [collapse_cycles()].
#' @inheritParams append_expected_values
#' @inheritParams calculate_etf
#' @family empirical transfer functions
#' @export
empirical_transfer_function <- function(.data,
                                        std_names = paste0("ETH-", 1:3),
                                        D47 = c(0.258, 0.256, 0.691), #0.507),
                                        aff = 0.062,
                                        raw = D47_raw_mean, exp = expected_D47,
                                        session = Preparation,
                                        quiet = default(quiet),
                                        genplot = default(genplot)) {
  raw <- enquo(raw)
  exp <- enquo(exp)
  session <- enquo(session)

  if (!quiet)
    message("Info: calculating and applying Emperical Transfer Function.")
  out <- .data %>%
    append_expected_values(std_names = std_names, D47 = D47, aff = aff) %>%
    calculate_etf(raw = !! raw, exp = !! exp, session = !! session) %>%
    apply_etf(D47 = !! raw)
  if (genplot)
    out %>%
      pipe_plot(plot_etf, std_names = std_names, session = !! session)
  out
}

# http://r-pkgs.had.co.nz/man.html#dry2
# TODO: use @describeIn or @rdname to also include the references from below in the wrapper.

#' Append expected values
#'
#' Append the expected values for the standards. Defaults to using
#' ETH-1--ETH-3, subtracting the acid fractionation factor before-hand.
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param std_names Names of the standards.
#' @param D47 Expected values of the standards at 25 °C. Defaults to Müller et
#'   al., 2017.
#' @inheritParams acid_fractionation
#' @param id1 Name of the standard/sample identifier column.
#' @param exp Name of the new column that will hold expected values.
#'
#' @references
#' W. F. Defliese, M.T. Hren, K. C. Lohmann. Compositional and temperature
#' effects of phosphoric acid fractionation on \eqn{\Delta_{47}}{Δ47} analysis
#' and implications for discrepant calibrations. _Chem. Geol._ **2016**, _396_,
#' 51.
#'
#' S. T. Murray, M. M. Arienzo, P. K. Swart. Determining the
#' \eqn{\Delta_{47}}{Δ47} acid fractionation in dolomites. _Geochem. Cosmochim.
#' Acta_ **2016**, _174_, 42.
#'
#' I. A. Müller, A. Fernandez, J. Radke, J. van Dijk, D. Bowen, J. Schwieters,
#' S. M. Bernasconi. Carbonate clumped isotope analyses with the
#' long-integration dual-inlet (LIDI) workflow: scratching at the lower sample
#' weight boundaries. _Rapid Commun. Mass Spectrom._ **2017**, _31_,
#' 1057--1066.
#' @family empirical transfer functions
#' @export
append_expected_values <- function(.data,
                                   std_names = paste0("ETH-", 1:3),  # we don't use ETH-4!
                                   D47 = c(0.258, 0.256, 0.691), #, 0.507),
                                   aff = 0.062,
                                   id1 = `Identifier 1`,
                                   exp = expected_D47) {
  id1 <- enquo(id1)
  exp <- enquo(exp)

  # TODO: vectorize this, so you can add as many standards as desired?
  .data %>%
    mutate(!! exp := case_when(!! id1 == std_names[1] ~ D47[1] - aff,
                               !! id1 == std_names[2] ~ D47[2] - aff,
                               !! id1 == std_names[3] ~ D47[3] - aff,
                               TRUE ~ NA_real_))
}

#' Calculate the Empirical Transfer Function
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param session The column name to group analyses by. Defaults to
#'   `Preparation`.
#' @export
calculate_etf <- function(.data, raw = D47_raw_mean, exp = expected_D47,
                          session = Preparation, quiet = default(quiet)) {
  raw <- enquo(raw)
  exp <- enquo(exp)

  if (quo_name(raw) != "D47_raw_mean" | quo_name(exp) != "expected_D47")
    warning("Currently ignoring option, lm call cannot process quosure.")

  session <- enquo(session)

  # this makes it continue even if lm fails.
  pos_lm <- purrr::possibly(lm, otherwise = NA, quiet = quiet)

  etf <- .data %>%
    group_by(!! session) %>%
    # TODO: pass quosures as expression to lm? For now I've hard-coded it.
    do(model = broom::tidy(pos_lm(D47_raw_mean ~ expected_D47, data = ., na.action = na.omit))) %>%
    unnest() %>%
    select(!! session, term, estimate) %>%
    spread(term, estimate) %>%
    select(!! session, intercept = `(Intercept)`, slope = expected_D47) %>%
    right_join(.data, by = quo_name(session))
}

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
#' @family empirical transfer functions
#' @export
apply_etf <- function(.data, D47 = D47_raw_mean) {
  D47 <- enquo(D47)
  .data %>%
    mutate(D47_etf = - (intercept / slope) + (1 / slope) * !! D47)
}

#' Plot the Empirical Transfer Function
#'
#' Create a plot with the expected \eqn{\Delta_{47}}{Δ47} value on the x-axis
#' and the \eqn{\Delta_{47}}{Δ47} derrived from the ETF on the y-axis.
#'
#' @inheritParams calculate_etf
#' @family empirical transfer functions
#' @export
plot_etf <- function(.data, std_names = paste0("ETH-", 1:3),
                     D47_etf = D47_etf,
                     exp = expected_D47,
                     raw = D47_raw_mean,
                     session = Preparation) {
  session <- enquo(session)
  D47_etf <- enquo(D47_etf)
  exp <- enquo(exp)
  raw <- enquo(raw)

  pld <- .data %>%
    mutate(!! exp := ifelse(is.na(!! exp), !! D47_etf, !! exp))

  pld %>%
    plot_base() +
    geom_point(aes(x = !! exp, y = !! raw)) +
    geom_smooth(aes(x = !! exp, y = !! raw, group = "yes"), method = "lm",
                data = filter(pld, broadid %in% std_names)) +
    facet_grid(rows = vars(!! session))
}
