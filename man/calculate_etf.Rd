% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_etf.R
\name{calculate_etf}
\alias{calculate_etf}
\title{Calculate the Empirical Transfer Function}
\usage{
calculate_etf(
  .data,
  ...,
  raw = D47_raw_mean,
  exp = expected_D47,
  session = Preparation,
  etf = etf,
  etf_coefs = etf_coefs,
  slope = slope,
  intercept = intercept,
  parallel = FALSE,
  quiet = NULL
)
}
\arguments{
\item{.data}{A \link[tibble:tibble-package]{tibble}.}

\item{...}{These dots are for future extensions and must be empty.}

\item{raw}{Column name of raw \eqn{\Delta_{47}} values.}

\item{exp}{Column name of expected \eqn{\Delta_{47}} values.}

\item{session}{The column name to group analyses by. Defaults to
\code{Preparation}.}

\item{etf}{The column name of the new model.}

\item{etf_coefs}{The column name with the coefficients of the model.}

\item{slope}{The column name of the new slope.}

\item{intercept}{The column name of the new intercept.}

\item{parallel}{Whether or not (default) to process this in parallel, using package \code{furrr}.}

\item{quiet}{If \code{TRUE}, print info messages. If \code{NULL} (default) listens to \code{options(clumpedr.quiet)}}
}
\value{
Same as \code{.data} with new columns \code{etf}, \code{etf_coefs}, \code{slope}, and \code{intercept}.
}
\description{
Calculate the Empirical Transfer Function
}
\seealso{
Other empirical transfer functions: 
\code{\link{append_expected_values}()},
\code{\link{apply_etf}()},
\code{\link{empirical_transfer_function}()}
}
\concept{empirical transfer functions}
