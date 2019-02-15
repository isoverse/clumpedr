#' Empirical Transfer Function
#'
#' Calculate and apply the empirical transfer function (ETF). First append the
#' expected values for the standards, then calculate the ETF per Preparation,
#' then apply the ETF to all the samples and standards.
#'
#' @param dat A [tibble][tibble::tibble-package] resulting from [collapse_cycles()].
#' @inheritParams append_expected_values
#' @family empirical transfer functions
#' @export
empirical_transfer_function <- function(dat,
                                        names = paste0("ETH-", 1:3),
                                        D47 = c(0.258, 0.256, 0.691), #0.507),
                                        aff = 0.062, quiet = default(quiet),
                                        genplot = default(genplot)) {
    if (!quiet)
        message("Info: calculating and applying Emperical Transfer Function.")
    out <- dat %>%
        append_expected_values(names = names, D47 = D47, aff = aff) %>%
        calculate_etf() %>%
        apply_etf()
    if (genplot)
        out %>%
            pipe_plot(plot_etf, std_names = names)
    out
}

#' Append expected values
#'
#' Append the expected values for the standards. Defaults to using ETH-1--ETH-3.
#'
#' @param dat Data yadayada.
#' @param names Names of the standards.
#' @param D47 Expected values of the standards at 25 °C, from Müller et al., 2017.
#' @inheritParams acid_fractionation
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
append_expected_values <- function(dat,
                                   names = paste0("ETH-", 1:3),  # we don't use ETH-4!
                                   D47 = c(0.258, 0.256, 0.691), #, 0.507),
                                   aff = 0.062) {
    ## TODO: vectorize this, so you can add as many standards as desired!
    dat %>%
        mutate(expected_D47 = ifelse(`Identifier 1` == names[1], D47[1] - aff,
                              ifelse(`Identifier 1` == names[2], D47[2] - aff,
                              ifelse(`Identifier 1` == names[3], D47[3] - aff,
                              ## ifelse(`Identifier 1` == names[4], D47[4] - aff,
                                     NA_real_))))
}

#' Calculate the Empirical Transfer Function
#'
#' @export
calculate_etf <- function(dat) {
    etf <- dat %>%
        filter(!is.na(D47raw_mean)) %>%
        group_by(Preparation) %>%
        do(model = broom::tidy(lm(D47raw_mean ~ expected_D47, data = .))) %>%
        unnest() %>%
        select(Preparation, term, estimate) %>%
        spread(term, estimate) %>%
        select(Preparation, intercept = `(Intercept)`, slope = expected_D47) %>%
        right_join(dat, by = "Preparation")
}

#' Apply the ETF
#'
#' Uses the previously calculated intercept and slope, and uses them to
#' calculate the \eqn{\Delta_{47}}{Δ47} values.
#'
#' @details Note that the intercept and slope were calculated with the
#'   dependent and independent variables in the other direction, so we flip
#'   them here. i.e.: \deqn{\Delta_{47etf} = - (\alpha / \beta) + (1 / \beta)
#'   \times \Delta_{47raw}}{Δ47_etf = - (α / β) + (1 / β) * Δ47_raw}
#' @param dat A [tibble][tibble::tibble-package] containing column D47.
#' @param D47 The column with \eqn{\Delta_{47}}{Δ47} values to use.
#' @family empirical transfer functions
#' @export
apply_etf <- function(dat, D47 = quo(D47raw_mean)) {
    dat %>%
        mutate(D47_etf = - (intercept / slope) + (1 / slope) * !!D47)
}

#' Plot the Empirical Transfer Function
#'
#' Create a plot with the expected \eqn{\Delta_{47}}{Δ47} value on the x-axis
#' and the \eqn{\Delta_{47}}{Δ47} derrived from the ETF on the y-axis.
#'
#' @family empirical transfer functions
#' @export
plot_etf <- function(dat, std_names = paste0("ETH-", 1:3)) {
    pld <- dat %>%
        mutate(expected_D47 = ifelse(is.na(expected_D47), D47_etf, expected_D47))

    pld %>%
        plot_base() +
        geom_point(aes(x = expected_D47, y = D47raw_mean)) +
        geom_smooth(aes(x = expected_D47, y = D47raw_mean, group = "yes"), method = "lm",
                    data = filter(pld, broadid %in% std_names)) +
        facet_grid(rows = vars(Preparation))
}
