#' Append reference gas delta values
#'
#' @param .data A [tibble][tibble::tibble-package] to append the delta values to.
#' @param .did An iso file, resulting from [isoreader::iso_read_dual_inlet()].
#' @param d13C_PDB_wg \eqn{\delta^{13}C} reference gas value to overwrite.
#' @param d18O_PDBCO2_wg \eqn{\delta^{18}O} reference gas value to overwrite.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
#' @returns Same as `.data` but with new columns `d13C_PDB_wg` and `d18O_PDBCO2_wg`.
#' @examples
#' dat <- isoreader::iso_get_raw_data(standards)
#' append_ref_deltas(.data = dat, .did = standards)
#' @export
append_ref_deltas <- function(.data, .did = NULL,
                              ...,
                              d13C_PDB_wg = NULL,
                              d18O_PDBCO2_wg = NULL,
                              quiet = NULL) {
  rlang::check_dots_empty0(...)
  if (!any(class(.data) %in% c("data.frame", "tbl_df", "tbl"))) {
    stop("`.data` must be a data.frame or tibble", call. = FALSE)
  }

  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }

  quiet <- check_quiet(quiet)

  # i had this commented out but I forgot why
  if (!is.null(.did)) {
    if (!any(c("iso_file_list", "iso_file") %in% class(.did))) {
      stop("did must be an 'iso_file' or 'iso_file_list'.", call. = FALSE)
    }
  }

  if (!((is.null(.did) & !is.null(d13C_PDB_wg) & !is.null(d18O_PDBCO2_wg)) |
          (!is.null(.did) & is.null(d13C_PDB_wg) & is.null(d18O_PDBCO2_wg)))) {
    stop("Either did or d13C_PDB_wg and d18O_PDBCO2_wg must be provided.", call. = FALSE)
  }

  if (!quiet) {
    message("Info: collapsing cycles, calculating sample summaries.")
  }

  if (is.null(d13C_PDB_wg) & is.null(d18O_PDBCO2_wg) & !is.null(.did)) {
    if (!quiet) {
      glue("Info: appending reference gas \u03b4 values from {length(.did)} data file(s)") %>%
        message()
    }
    if (!"file_id" %in% colnames(.data)) {
      stop("`.data` must contain the column 'file_id'.", call. = FALSE)
    } else {
      out <- .data %>%
        left_join(get_ref_delta(.did), "file_id")
    }
  } else {
    if (!quiet) {
      glue("Info: appending reference gas \u03b4 values: \u03b413C = {d13C_PDB_wg} and \u03b418O = {d18O_PDBCO2_wg}.") %>%
        message()
    }
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
  if (!requireNamespace("isoreader", quietly = TRUE)) {
    stop("'isoreader' is required to get_ref_delta, please run:\n   remotes::install_github('isoverse/isoreader')",
         call. = FALSE)
    return(invisible(.did))
  }

  isoreader::iso_get_standards(.did, quiet = TRUE) %>%
    filter(.data$delta_name %in% c("d 13C/12C", "d 18O/16O")) %>%
    distinct(.data$file_id, .data$delta_name, .keep_all = TRUE) %>%
    pivot_wider(id_cols = "file_id", names_from = "delta_name", values_from = "delta_value") %>%
    select("file_id", "d 13C/12C", "d 18O/16O") %>%
    rename(d13C_PDB_wg = "d 13C/12C", d18O_PDBCO2_wg = "d 18O/16O") %>%
    as_tibble()
}
