#' Filter out all the clumped files from regular stable isotope geochemistry
#'
#' @param dat A [tibble][tibble::tibble-package], resulting from
#'   `isoreader::iso_read_dual_inlet()`
#' @param regex The regular expression that captures the desired methods. The
#'   default is a very explicit filtering of our presently used method names,
#'   be sure to change this!
filter_clumped <- function(dat,
                           regex = NULL, quiet = default(quiet)) {
    if (is.null(regex)) {
        regex <- "(Clumped LIDI Kiel.met)|(Clumped Kiel click clack.met)|(Clumped LIDI Kiel big samples.met)"
        if (!quiet)
            glue("Warning: no regex passed, using default UU-configuration of:\n{regex}") %>%
                message()
    }
    dat %>%
      isoreader::iso_filter_files(grepl(regex, Method))
}
