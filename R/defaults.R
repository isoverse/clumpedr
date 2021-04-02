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
#' @param D17O The difference between the expected \eqn{\delta^{17}}{\u03b417}O
#'   (\eqn{\delta^{18}}{\u03b418}O\eqn{-\lambda*\delta^{18}}{- lambda *\u03b418}O)
#'   and the actual \eqn{\delta^{17}}{\u03b417}O value.
#' @param D47 TODO: look up
#' @param D48 TODO: look up
#' @param D49 TODO: look up
#' @references
#'
#' Assonov, S., Brenninkmeijer C. On the \eqn{^{17}}{17}O correction for
#'   CO\eqn{_{2}}{2} mass spectrometric isotopic analysis _Rapid Commun. Mass
#'   Spectrom._, **2003**, 17, pp. 1007-1016, 10.1002/rcm.1012
#'
#' Baertschi, P., Absolute \eqn{^{18}}{18}O content of standard mean ocean
#'   water _Earth Planet. Sci. Lett._, **1976**, 31, pp. 341-344
#'
#' Barkan, E., Luz B. High-precision measurements of
#'   \eqn{^{17}}{17}O/\eqn{^{16}}{16}O and \eqn{^{18}}{18}O/\eqn{^{16}}{16}O
#'   ratios in H2O _Rapid Commun. Mass Spectrom._, **2005**, 19, pp. 3737-3742
#'
#' Chang, T., Li W. A calibrated measurement of the atomic weight of carbon
#'   _Chin. Sci. Bull._, **1990**, 35 (290)
#'
#' Coplen, T., C. Kendall, Hopple J. Comparison of stable isotope reference
#'     samples _Nature_, **1983**, 302 (5905) , pp. 236-238, 10.1038/302236a0
#'
#' Brand, W. A., Assonov, S. S., & Coplen, T. B. Correction for the
#'   \eqn{^{17}}{17}O interference in \eqn{\delta^{13}}{δ13}C measurements when
#'   analyzing CO\eqn{_{2}}{2} with stable isotope mass spectrometry (IUPAC
#'   Technical Report). _Pure and Applied Chemistry_, **2010**, _82(8)_,
#'   1719–1733. \url{https://doi.org/10.1351/PAC-REP-09-01-05}
#'
#' @keywords internal
#' @name default.params
#' @aliases clumpedr.params clumpedr.parameters default.parameters
#'   brand.parameters Brand brand_parameters
NULL
