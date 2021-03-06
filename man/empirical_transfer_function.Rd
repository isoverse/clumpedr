% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/empirical_transfer_function.R
\name{empirical_transfer_function}
\alias{empirical_transfer_function}
\title{Empirical Transfer Function}
\usage{
empirical_transfer_function(.data, std_names = paste0("ETH-", 1:3),
  D47 = c(0.258, 0.256, 0.691), aff = 0.062, raw = D47_raw,
  exp = expected_D47, id1 = `Identifier 1`, session = Preparation,
  quiet = default(quiet), genplot = default(genplot))
}
\arguments{
\item{.data}{A \link[tibble:tibble-package]{tibble} resulting from \code{\link[=collapse_cycles]{collapse_cycles()}}.}

\item{std_names}{Names of the standards.}

\item{D47}{Expected values of the standards at 25 °C. Defaults to Müller et
al., 2017.}

\item{aff}{Temperature-dependent acid fractionation projection from 70 °C to
25 °C. Defaults to 0.062, which is the average of the values obtained by
De Vlieze et al., 2015 and Murray et al., 2016. See Müller et al., 2017.}

\item{raw}{Column name of raw \eqn{\Delta_{47}}{Δ47} values.}

\item{exp}{Name of the new column that will hold expected values.}

\item{id1}{Name of the standard/sample identifier column.}

\item{session}{The column name to group analyses by. Defaults to
\code{Preparation}.}
}
\description{
Calculate and apply the empirical transfer function (ETF). First append the
expected values for the standards, then calculate the ETF per Preparation,
then apply the ETF to all the samples and standards.
}
\seealso{
Other empirical transfer functions: \code{\link{append_expected_values}},
  \code{\link{apply_etf}}, \code{\link{plot_etf}}
}
\concept{empirical transfer functions}
