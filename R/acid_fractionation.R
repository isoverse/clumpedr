#' Add acid fractionation
#'
#' The temperature-dependent acid fractionation projection from 70 \eqn{^\circ}C to 25 \eqn{^\circ}C.
#' It subtracts the `aff` from the `D47`-value.
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param aff Temperature-dependent acid fractionation projection from 70 \eqn{^\circ}C to
#'   25 \eqn{^\circ}C. Defaults to 0.062, which is the average of the values obtained by
#'   De Vlieze et al., 2015 and Murray et al., 2016. See Muller et al., 2017.
#' @param D47 The column name of the \eqn{\Delta_47} values to use for the acid
#'   fractionation calculation.
#' @param D47_out The desired new column name.
#'
#' @references
#'
#' W. F. Defliese, M.T. Hren, K. C. Lohmann. Compositional and temperature
#' effects of phosphoric acid fractionation on \eqn{\Delta_{47}} analysis and
#' implications for discrepant calibrations. _Chem. Geol._ **2016**, _396_, 51.
#'
#' S. T. Murray, M. M. Arienzo, P. K. Swart. Determining the \eqn{\Delta_{47}}
#' acid fractionation in dolomites. _Geochem. Cosmochim. Acta_ **2016**, _174_,
#' 42.
#'
#' I. A. Muller, A. Fernandez, J. Radke, J. van Dijk, D. Bowen, J. Schwieters,
#' S. M. Bernasconi. Carbonate clumped isotope analyses with the
#' long-integration dual-inlet (LIDI) workflow: scratching at the lower sample
#' weight boundaries. _Rapid Commun. Mass Spectrom._ **2017**, _31_,
#' 1057--1066.
#'
#' @export
acid_fractionation <- function(.data, aff = 0.062, D47 = D47_etf,
                               D47_out = D47_final,
                               quiet = default(quiet)) {
  # global variables and defaults
  D47_etf <- D47_final <- NULL

  if (!quiet)
    glue("Info: adding temperature-dependent acid fractionation factor of {aff}.") |>
      message()
  .data |>
    mutate({{ D47_out }} := {{ D47 }} + aff)
}
