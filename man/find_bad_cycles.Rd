% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_bad_cycles.R
\name{find_bad_cycles}
\alias{find_bad_cycles}
\title{Find bad cycles based on several criteria.}
\usage{
find_bad_cycles(
  .data,
  ...,
  min,
  max,
  fac,
  v44 = "v44.mV",
  cycle = "cycle",
  relative_to = "init",
  quiet = NULL
)
}
\arguments{
\item{.data}{A \link[tibble:tibble-package]{tibble}, resulting from
\code{\link[isoreader:iso_get_raw_data]{isoreader::iso_get_raw_data()}}.}

\item{...}{These dots are for future extensions and must be empty.}

\item{min}{Minimum intensity level for good cycles. Defaults to 1,500 mV.}

\item{max}{Maximum intensity level for good cycles. Defaults to 50,000 mV.}

\item{fac}{Factor for how much larger the current drop should be than the
one specified in \code{relative_to}.}

\item{v44}{Column name of mass 44.}

\item{cycle}{Column name of the column with the measurement cycle number.}

\item{relative_to}{cycle Drop detection occurs relative to either the first
cycle ("init", default) or to the previous cycle ("prev").}

\item{quiet}{If \code{TRUE}, print info messages. If \code{NULL} (default) listens to \code{options(clumpedr.quiet)}}
}
\value{
Same as \code{.data} but with some new columns that allow us to disable
certain cycles/whole samples.
}
\description{
This function tries to identify cycles in which the pressure has dropped
fast. It first disables all the cycles above or below certain thresholds
(\code{min}, \code{max}), and then calculates whether the next cycle has dropped by
more than \code{fac} times either the initial (\verb{relative to = "init"}, default)
or the previous (\code{relative_to = "prev"}) cycle.
}
\details{
The drop in intensity can be defined relative to the first change
in intensity (default), or to the previous cycle.

Note that the min, max, and fac input parameters must be columns in the
input data, so that reproducibility is guaranteed.
}
\examples{
# get the raw data from the example dataset
isoreader::iso_get_raw_data(standards, include_file_info = "Analysis") |>
  # add new columns with criteria
  dplyr::mutate(dis_min = 500, dis_max = 50000, dis_fac = 3) |>
  find_bad_cycles(min = "dis_min", max = "dis_max",
                  fac = "dis_fac", relative_to = "init")
}
\concept{cycle functions}
