#' Default parameters
#'
#' @param R13_PDB The R13 value of the PDB reference. Defaults to 0.01118, from
#'   Chang and Li, 1990.
#' @param R18_PDB The R18 value of the PDB reference. Defaults to 1.008751,
#'   from Coplen et al., 1983.
#' @param R17_PDBCO2 The R17 value of PDB (g). Defaults to 0.0003931, from
#'   Assonov and Brenninkmeijer 2003, re-scaled by Brand, 2010.
#' @param R18_PDBCO2 The R18 value of PDB (g). Defaults to 0.00208839, derrived
#'   from Baertschi, 1976, calculated by Brand 2010 eq. 2.
#' @param lambda Isotopic fractionation of \eqn{{}^17}O. Defaults to 0.528,
#'   from Barkan and Luz, 2005.
#' @param D17O TODO: look up in Daëron 2016 what this does again.
#' @references Brand, W. A., Assonov, S. S., & Coplen, T. B. (2010). Correction
#'   for the 17O interference in \eqn{\delta^{13}}C measurements when analyzing
#'   CO2 with stable isotope mass spectrometry (IUPAC Technical Report). Pure
#'   and Applied Chemistry, 82(8), 1719–1733.
#'   \url{https://doi.org/10.1351/PAC-REP-09-01-05}
#' @keywords internal
#' @name default.params
#' @aliases clumpedr.params clumpedr.parameters default.parameters
#'   brand.parameters
NULL

## clumpedr.params <- function(R13_PDB = default(R13_PDB),
##                             R18_PDB = default(R18_PDB),
##                             R17_PDBCO2 = default(R17_PDBCO2),
##                             R18_PDBCO2 = default(R18_PDBCO2),
##                             lambda = default(lambda), quiet = default(quiet)) {
##     if (!quiet)
##         message("Info: clumpedr parameters can be listed with `clumpedr_get_default_parameters()`")
## }
