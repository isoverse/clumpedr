% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abundance_ratios.R
\name{abundance_ratios}
\alias{abundance_ratios}
\title{Calculate abundance ratio based on input intensities}
\usage{
abundance_ratios(
  .data,
  i44 = s44,
  i45 = s45,
  i46 = s46,
  i47 = s47,
  i48 = s48,
  i49 = s49,
  R45 = R45,
  R46 = R46,
  R47 = R47,
  R48 = R48,
  R49 = R49,
  quiet = default(quiet)
)
}
\arguments{
\item{.data}{A \link[tibble:tibble-package]{tibble}.}

\item{i44}{Name of mass 44 column.}

\item{i45}{Name of mass 45 column.}

\item{i46}{Name of mass 46 column.}

\item{i47}{Name of mass 47 column.}

\item{i48}{Name of mass 48 column.}

\item{i49}{Name of mass 49 column.}

\item{R45}{Desired new name of the calculated ratio for mass 45.}

\item{R46}{Desired new name of the calculated ratio for mass 46.}

\item{R47}{Desired new name of the calculated ratio for mass 47.}

\item{R48}{Desired new name of the calculated ratio for mass 48.}

\item{R49}{Desired new name of the calculated ratio for mass 49.}
}
\description{
Calculate abundance ratios by dividing input mass column \eqn{i} over mass
44. \deqn{R_i = mass_i / mass_{44}}
}
