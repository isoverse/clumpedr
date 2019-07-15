#' Append reference gas delta values
#'
#' @param .data A [tibble][tibble::tibble-package] to append the delta values to.
#' @param .did An iso file, resulting from [isoreader::iso_read_dual_inlet()].
#' @param d13C_PDB_wg \eqn{\delta^{13}C}{δ13C} reference gas value to overwrite.
#' @param d18O_PDBCO2_wg \eqn{\delta^{18}O}{δ18O} reference gas value to overwrite.
#' @export
append_ref_deltas <- function(.data, .did,
                              d13C_PDB_wg = NULL,
                              d18O_PDBCO2_wg = NULL,
                              quiet = default(quiet)) {
  if (!quiet) {
    glue("Info: appending reference gas \u03b4 values from {length(.did)} data file(s)") %>%
      message()
  }

  if (is.null(d13C_PDB_wg) & is.null(d18O_PDBCO2_wg)) {
    out <- .data %>%
      left_join(get_ref_delta(.did), "file_id")
  } else {
    out <- .data %>%
      mutate(d13C_PDB_wg = d13C_PDB_wg,
             d18O_PDBCO2_wg = d18O_PDBCO2_wg)
  }

  out
}

#' Get reference gas delta values
#'
#' @param .did An iso file, resulting from [isoreader::iso_read_dual_inlet()].
#' @export
get_ref_delta <- function(.did) {
  isoreader::iso_get_standards_info(.did, quiet = TRUE) %>%
    pivot_wider(id_cols = .data$file_id, names_from = .data$delta_name, values_from = .data$delta_value) %>%
    select(.data$file_id, d13C_PDB_wg = .data$`d 13C/12C`, d18O_PDBCO2_wg = .data$`d 18O/16O`) %>%
    as_tibble()
}
