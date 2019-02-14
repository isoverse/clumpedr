#' Add acid fractionation
#'
#' @param dat Data.
#' @param aff Temperature-dependent acid fractionation projection from 70 °C to
#'   25 °C. Defaults to 0.062, which is the average of the values obtained by
#'   De Vlieze et al., 2015 and Murray et al., 2016. See also Müller et al.,
#'   2017.
#' @param D47_etf The quoted column name of the Δ47 values to use for the acid
#'   fractionation calculation.
acid_fractionation <- function(dat, aff = 0.062, D47_etf = quo(D47_etf),
                               quiet = default(quiet)) {
    if (!quiet)
        glue("Info: adding temperature-dependent acid fractionation factor of {aff}.") %>%
            message()
    dat %>%
        mutate(D47_final = !!D47_etf + aff)
}
