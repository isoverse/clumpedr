% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_outliers.R
\name{summarize_outlier}
\alias{summarize_outlier}
\title{Summarize the outlier columns.}
\usage{
summarize_outlier(.data, out_column = outlier, ..., quiet = NULL)
}
\arguments{
\item{.data}{A \link[tibble:tibble-package]{tibble} with raw Delta values and file information.}

\item{out_column}{The name of the outlier column.}

\item{...}{These dots are for future extensions and must be empty.}

\item{quiet}{If \code{TRUE}, print info messages. If \code{NULL} (default) listens to \code{options(clumpedr.quiet)}}
}
\description{
Calculate whether a sample is an outlier or not based on all the existing \code{"outlier_"} columns.
}
