% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spread_match.R
\name{spread_match}
\alias{spread_match}
\title{Spread and match sample gas and reference gas.}
\usage{
spread_match(
  .data,
  ...,
  method = "normal",
  masses = c(44:49, 54),
  quiet = NULL
)
}
\arguments{
\item{.data}{A \link[tibble:tibble-package]{tibble}, resulting from \code{isoreader::iso_get_raw_data()}.}

\item{...}{These dots are for future extensions and must be empty.}

\item{method}{"linterp" for linear interpolation, or "normal" for
conventional bracketing of sample gas.}

\item{masses}{The masses to generate r and s columns from.}

\item{quiet}{If \code{TRUE}, print info messages. If \code{NULL} (default) listens to \code{options(clumpedr.quiet)}}
}
\description{
Spread and match sample gas and reference gas.
}
