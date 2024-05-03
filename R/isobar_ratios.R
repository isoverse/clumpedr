#' Compute isotopic ratios
#'
#' Compute isotopic ratios for a sample with isotopic ratios R13 and R18,
#' optionally accounting for non-zero values of \eqn{\Delta^{17}}O and
#' clumped isotope anomalies (all expressed in permil). Function based on
#' Daeron et al., 2016.
#'
#' @param data A tibble with columns R13 and R18.
# TODO: figure out how to also inherit references from default.params
#' @param R13 The column name for R13.
#' @param R18 The column name for R18.
#' @param R45 The output column name for R45.
#' @param R46 The output column name for R46.
#' @param R47 The output column name for R47.
#' @param R48 The output column name for R48.
#' @param R49 The output column name for R49.
#' @inheritParams default.params
#'
#' @references Daeron, M., Blamart, D., Peral, M., & Affek, H. P., Absolute
#'   isotopic abundance ratios and the accuracy of \eqn{\Delta_{47}}
#'   measurements, _Chemical Geology_ **2016**, _442_, 83--96.
#'   \doi{10.1016/j.chemgeo.2016.08.014}
isobar_ratios <- function(data,
                          # input columns
                          R13 = R13, R18 = R18,
                          ## output columns
                          R45 = R45, R46 = R46, R47 = R47,
                          R48 = R48, R49 = R49,
                          # constants
                          #R13_PBD = 0.01118,  # not used
                          R17_PDBCO2 = default(R17_PDBCO2),
                          R18_PDBCO2 = default(R18_PDBCO2),
                          lambda = default(lambda),
                          D17O = default(D17O),
                          D47 = default(D47),
                          D48 = default(D48),
                          D49 = default(D49)) {
  data %>%
    mutate(
      # Compute R17
      R17 = R17_PDBCO2 * exp(D17O / 1000) * ({{ R18 }} / R18_PDBCO2) ^ lambda,

      # Compute isotope concentrations
      C12 = (1 + {{ R13 }}) ^ -1,
      C13 = .data$C12 * {{ R13 }},
      C16 = (1 + .data$R17 + {{ R18 }}) ^ -1,
      C17 = .data$C16 * .data$R17,
      C18 = .data$C16 * {{ R18 }},

      # Compute stochastic isotopologue concentrations
      C626 = .data$C16 * .data$C12 * .data$C16,
      C627 = .data$C16 * .data$C12 * .data$C17 * 2,
      C628 = .data$C16 * .data$C12 * .data$C18 * 2,
      C636 = .data$C16 * .data$C13 * .data$C16,
      C637 = .data$C16 * .data$C13 * .data$C17 * 2,
      C638 = .data$C16 * .data$C13 * .data$C18 * 2,
      C727 = .data$C17 * .data$C12 * .data$C17,
      C728 = .data$C17 * .data$C12 * .data$C18 * 2,
      C737 = .data$C17 * .data$C13 * .data$C17,
      C738 = .data$C17 * .data$C13 * .data$C18 * 2,
      C828 = .data$C18 * .data$C12 * .data$C18,
      C838 = .data$C18 * .data$C13 * .data$C18,

      # Compute stochastic isobar ratios
      {{ R45 }} := (.data$C636 + .data$C627) / .data$C626,
      {{ R46 }} := (.data$C628 + .data$C637 + .data$C727) / .data$C626,
      {{ R47 }} := (.data$C638 + .data$C728 + .data$C737) / .data$C626,
      {{ R48 }} := (.data$C738 + .data$C828) / .data$C626,
      {{ R49 }} := .data$C838 / .data$C626,

      # Account for stochastic anomalies
      {{ R47 }} := {{ R47 }} * (1 + D47 / 1000),
      {{ R48 }} := {{ R48 }} * (1 + D48 / 1000),
      {{ R49 }} := {{ R49 }} * (1 + D49 / 1000))
}
