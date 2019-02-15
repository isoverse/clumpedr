#' Append Mass Spectrometer name and some metadata
#'
#' Append mass spectrometer name, as well as a new `broadid` for easy plotting.
#' TODO: Furthermore, add an v44.mV_init column
#'
#' @param dat The data in flattened format.
#' @param masspec The name of the mass spectrometer to be added.
#' @param standards The names of the standards, identified in column 'Identifier 1'.
#' @param other_label The label name to use for anything not in the standards.
#' @param ms_name The column name to use for the mass spec label.
#' @param broadid_name The column name for the broad id (standard name or other).
#' @param group Logical. Group by file_path, file_datetime, Identifier 1, type if `TRUE` (default).
#' @param sort Logical. Arrange by the newly created groups if `TRUE` (default).
#' @export
append_ms_info <- function(dat, masspec, standards = paste0("ETH-", 1:4),
                           other_label = "other", ms_name = quo(masspec),
                           broadid_name = quo(broadid), group = TRUE, sort = TRUE) {
    dat <- dat %>%
        mutate(!!ms_name := masspec) %>%
        mutate(!!broadid_name := ifelse(`Identifier 1` %in% standards,
                                   `Identifier 1`, other_label))
    if (group) {
        dat  <- dat %>%
            group_by(file_path, file_datetime, `Identifier 1`, type)
    }
    if (sort) {
        dat  <- dat %>%
            arrange(file_path, file_datetime, `Identifier 1`, type)
    }
    return(dat)
}
