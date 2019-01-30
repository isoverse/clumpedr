#' Add acid fractionation
#'
#' @param dat Data.
#' @param aff Temperature-dependent acid fractionation projection from 70 °C to
#'     25 °C. Defaults to 0.062, which is the average of the values obtained by
#'     De Vlieze et al., 2015 and Murray et al., 2016. See also Müller et al.,
#'     2017.
acid_fractionation <- function(dat, aff = 0.062, quiet = default(quiet)) {
    if (!quiet)
        glue("Info: adding temperature-dependent acid fractionation factor of {aff}.") %>%
            message()
    dat %>%
        mutate(D47_final = D47_etf + kaff)
}
