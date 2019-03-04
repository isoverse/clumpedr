#' Filter out all the clumped files from regular stable isotope geochemistry
#'
#' This function applies the [iso_filter_files()][isoreader::iso_filter_files]
#' function to filter by the Method using a regular expression.
#'
#' @param .did A [tibble][tibble::tibble-package], resulting from
#'   [iso_read_dual_inlet()][isoreader::iso_read_dual_inlet()]
#' @param regex The regular expression that captures the desired methods. The
#'   default is a very explicit filtering of our presently used method names,
#'   be sure to change this!
#' @references
#' \url{https://en.wikipedia.org/wiki/Regular_expression}
#' @export
filter_clumped <- function(.did, regex = NULL, quiet = default(quiet)) {
  # global variables and defaults
  Method <- NULL

  if (is.null(regex)) {
    regex <- "(Clumped LIDI Kiel.met)|(Clumped Kiel click clack.met)|(Clumped LIDI Kiel big samples.met)"
    if (!quiet)
      glue("Warning: no regex passed, using default UU-configuration of:\n{regex}") %>%
        message()
  }
  .did %>% isoreader::iso_filter_files(grepl(regex, Method))
}
