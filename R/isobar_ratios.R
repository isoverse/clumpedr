#' Compute isotopic ratios
#'
#' Compute isotopic ratios for a sample with isotopic ratios R13 and R18,
#' optionally accounting for non-zero values of \eqn{\Delta^{17}}{Δ17}O and
#' clumped isotope anomalies (all expressed in permil). Function based on
#' Daëron et al., 2016.
#'
#' @param dat A tibble with columns R13 and R18.
#' @inherit default.params
#' @param R13 The quoted column name for R13 (default `quo(R13)`).
#' @param R18 The quoted column name for R18 (default `quo(R18)`).
#' @param R45 the quoted output column name for R45 (default `quo(R45)`)
#'
#' @references Daëron, M., Blamart, D., Peral, M., & Affek, H. P., Absolute
#'   isotopic abundance ratios and the accuracy of \eqn{\Delta_{47}}{Δ47}
#'   measurements, _Chemical Geology_ **2016**, _442_, 83–96.
#'   \url{http://dx.doi.org/10.1016/j.chemgeo.2016.08.014}
#' @export
isobar_ratios <- function(dat, ## input columns
                          R13 = quo(R13), R18 = quo(R18),
                          ## output columns
                          R45 = quo(R45), R46 = quo(R46), R47 = quo(R47),
                          R48 = quo(R48), R49 = quo(R49),
                          ## constants
                          ##R13_PBD = 0.01118,  # not used
                          R17_PDBCO2 = default(R17_PDBCO2),
                          R18_PDBCO2 = default(R18_PDBCO2),
                          lambda = default(lambda),
                          D17O = default(D17O),
                          D47 = default(D47),
                          D48 = default(D48),
                          D49 = default(D49)) {
    dat %>%
        mutate(
            ## Compute R17
            R17 = R17_PDBCO2 * exp(D17O / 1000) * (!!R18 / R18_PDBCO2) ^ lambda,

            ## Compute isotope concentrations
            C12 = (1 + !!R13) ^ -1,
            C13 = C12 * !!R13,
            C16 = (1 + R17 + !!R18) ^ -1,
            C17 = C16 * R17,
            C18 = C16 * !!R18,

            ## Compute stochastic isotopologue concentrations
            C626 = C16 * C12 * C16,
            C627 = C16 * C12 * C17 * 2,
            C628 = C16 * C12 * C18 * 2,
            C636 = C16 * C13 * C16,
            C637 = C16 * C13 * C17 * 2,
            C638 = C16 * C13 * C18 * 2,
            C727 = C17 * C12 * C17,
            C728 = C17 * C12 * C18 * 2,
            C737 = C17 * C13 * C17,
            C738 = C17 * C13 * C18 * 2,
            C828 = C18 * C12 * C18,
            C838 = C18 * C13 * C18,

            ## Compute stochastic isobar ratios
            !!R45 := (C636 + C627) / C626,
            !!R46 := (C628 + C637 + C727) / C626,
            !!R47 := (C638 + C728 + C737) / C626,
            !!R48 := (C738 + C828) / C626,
            !!R49 := C838 / C626,

            ## Account for stochastic anomalies
            !!R47 := !!R47 * (1 + D47 / 1000),
            !!R48 := !!R48 * (1 + D48 / 1000),
            !!R49 := !!R49 * (1 + D49 / 1000)) %>%
    ## Return isobar ratios
    select(-c(C12, C13, C16, C17, C18, C626, C627, C628, C636, C637, C638, C727,
              C728, C737, C738, C828, C838))
}
