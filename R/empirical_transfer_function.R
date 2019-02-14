#' Empirical Transfer Function
#'
#' Calculate and apply the empirical transfer function (ETF). First append the
#' expected values for the standards, then calculate the ETF per Preparation,
#' then apply the ETF to all the samples and standards.
#'
#' @param dat Tibble passed from `collapse_cycles`.
#' @inheritParams append_expected_values
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
#' @param dat Data yadayada.
#' @param names Names of the standards.
#' @param D47 Expected values of the standards at 25 °C, from Müller et al., 2017.
#' @inheritParams acid_fractionation
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
#' @param dat A [tibble][tibble::tibble-package] containing column D47.
#' @param D47 The column with \eqn{\Delta_47} values to use.
#' @export
apply_etf <- function(dat, D47 = quo(D47raw_mean)) {
    dat %>%
        mutate(D47_etf = - (intercept / slope) + (1 / slope) * !!D47)
}

#' Plot the Empirical Transfer Function
#'
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
