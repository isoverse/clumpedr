% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isobar_ratios.R
\name{isobar_ratios}
\alias{isobar_ratios}
\title{Compute isotopic ratios}
\usage{
isobar_ratios(.data, R13 = R13, R18 = R18, R45 = R45, R46 = R46,
  R47 = R47, R48 = R48, R49 = R49,
  R17_PDBCO2 = default(R17_PDBCO2), R18_PDBCO2 = default(R18_PDBCO2),
  lambda = default(lambda), D17O = default(D17O), D47 = default(D47),
  D48 = default(D48), D49 = default(D49))
}
\arguments{
\item{.data}{A tibble with columns R13 and R18.}

\item{R13}{The column name for R13.}

\item{R18}{The column name for R18.}

\item{R45}{The output column name for R45.}

\item{R46}{The output column name for R46.}

\item{R47}{The output column name for R47.}

\item{R48}{The output column name for R48.}

\item{R49}{The output column name for R49.}

\item{R17_PDBCO2}{The R17 value of PDB (g). Defaults to 0.0003931, from
Assonov and Brenninkmeijer 2003, re-scaled by Brand, 2010.}

\item{R18_PDBCO2}{The R18 value of PDB (g). Defaults to 0.00208839, derrived
from Baertschi, 1976, calculated by Brand 2010 eq. 2.}

\item{lambda}{Isotopic fractionation of \eqn{^{17}}{17}O. Defaults to 0.528,
from Barkan and Luz, 2005.}

\item{D17O}{TODO: look up in Daëron 2016 what this does again.}

\item{D47}{TODO: look up}

\item{D48}{TODO: look up}

\item{D49}{TODO: look up}
}
\description{
Compute isotopic ratios for a sample with isotopic ratios R13 and R18,
optionally accounting for non-zero values of \eqn{\Delta^{17}}{Δ17}O and
clumped isotope anomalies (all expressed in permil). Function based on
Daëron et al., 2016.
}
\references{
Daëron, M., Blamart, D., Peral, M., & Affek, H. P., Absolute
isotopic abundance ratios and the accuracy of \eqn{\Delta_{47}}{Δ47}
  measurements, \emph{Chemical Geology} \strong{2016}, \emph{442}, 83–96.
\url{http://dx.doi.org/10.1016/j.chemgeo.2016.08.014}
}
