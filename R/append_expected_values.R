#' Append expected values
#'
#' Append the expected values for the standards. Defaults to using
#' ETH-1--ETH-3, subtracting the acid fractionation factor before-hand.
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param std_names Names of the standards.
#' @param D47 Expected values of the standards. Defaults to Müller et al., 2017
#'   at 70 °C acid digestion.
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
                                   D47 = c(0.258, 0.256, 0.691) - 0.062, #, 0.507),
                                   id1 = `Identifier 1`,
                                   exp = expected_D47) {
  # global variables and defaults
  `Identifier 1` <- expected_D47 <- NULL

  if (length(std_names) != length(D47))
    stop("std_names should be of equal length to D47.")

  expected_standard_values <- tibble({{ id1 }} := std_names, {{ exp }} := D47)

  .data %>%
    left_join(expected_standard_values, by = {{ id1 }})
}
