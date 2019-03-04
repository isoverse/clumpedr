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
#' @param lambda Isotopic fractionation of \eqn{^{17}}{17}O. Defaults to 0.528,
#'   from Barkan and Luz, 2005.
#' @param D17O TODO: look up in Daëron 2016 what this does again.
#' @param D47 TODO: look up
#' @param D48 TODO: look up
#' @param D49 TODO: look up
#' @references
#'
#' TODO: add all the references here!
#'
#' Brand, W. A., Assonov, S. S., & Coplen, T. B. Correction for the 17O
#' interference in \eqn{\delta^{13}}{δ13}C measurements when analyzing
#' CO\eqn{_{2}}{2} with stable isotope mass spectrometry (IUPAC Technical
#' Report). _Pure and Applied Chemistry_, **2010**, _82(8)_, 1719–1733.
#' \url{https://doi.org/10.1351/PAC-REP-09-01-05}
#' @keywords internal
#' @name default.params
#' @aliases clumpedr.params clumpedr.parameters default.parameters
#'   brand.parameters Brand brand_parameters
NULL
