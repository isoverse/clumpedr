#' Append expected values
#'
#' Append the expected values for the standards. Defaults to using
#' ETH-1--ETH-3, subtracting the acid fractionation factor before-hand.
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param std_names Names of the standards.
#' @param std_values Expected values of the standards. Defaults to Müller et
#'   al., 2017 at 70 °C acid digestion.
#' @param exp Name of the new column that will hold expected values.
#' @param by Name of the standard/sample identifier column.
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
#'
#' @export
#' @family empirical transfer functions
append_expected_values <- function(.data,
                                   std_names = paste0("ETH-", 1:3),  # we don't use ETH-4!
                                   std_values = c(0.258, 0.256, 0.691) - 0.062, #, 0.507),
                                   exp = expected_D47,
                                   by = `Identifier 1`,
                                   quiet = default(quiet)) {
  # global variables and defaults
  `Identifier 1` <- expected_D47 <- NULL

  if (length(std_names) != length(std_values))
    stop("std_names should be of equal length to std_values.")

  if (anyNA(std_values)) {
    std_names <- std_names[!is.na(std_values)]
    std_values <- std_values[!is.na(std_values)]
  }

  if (!quiet)
    glue("Info: Appending expected values as {quo_name(enquo(exp))} for standards {glue::glue_collapse(unique(std_names), sep = ' ', last = ' and ', width = 30)}") %>%
      message()

  expected_standard_values <- tibble({{ by }} := std_names,
                                     {{ exp }} := std_values)

  by_quo_name <- quo_name(enquo(by))

  .data %>%
    left_join(expected_standard_values, by = by_quo_name)
}
