#' Spread intensities from iso_read format into wide format.
#'
#' To be able to calculate ratios per cycle, it is necessary to put the sample
#' and reference as columns next to each other. This function transforms the
#' data into the desired format.
#'
#' @param .data A [tibble][tibble::tibble-package] containing mass intensities per cycle.
#' @param ids Identifying columns that we'll group by.
#' @param our_cols Columns with data values that need to be reshaped.
#' @return A [tibble][tibble::tibble-package] with the sample and reference
#'   gasses side-by-side.
#' @export
# TODO: clean up the arguments
spread_intensities  <- function(.data, ids = NULL, our_cols = NULL, quiet = default(quiet)) {
  # global variables and defaults
  file_id <- mass <- intensity <- type <- cycle <- mir <- value <- standard <- has_drop <- NULL

  if (!quiet)
    message("Info: reshaping data into wide format.")

  if (!"type" %in% colnames(.data))
    stop(glue("Column 'type' not found in {paste(colnames(.data), collapse = ', ')}"))

  if (is.null(ids)) {
    ids <- c("file_id", "cycle")
  }

  if (is.null(our_cols)) {
    our_cols <- c("v44.mV", "v45.mV", "v46.mV", "v47.mV", "v48.mV", "v49.mV", "v54.mV")
                  ## "v44_diff", "v44_drop", "has_drop", "cycle_drop")
  }

  out <- .data %>%
    group_by(file_id) %>%
    # first lengthen it so that each row is one mass / unit / intensity
    pivot_longer(cols = our_cols,
                 names_to = c("mass", "unit"),
                 names_pattern = "v([4-9]{2}).(mV)") %>%
    # then widen it so that sample and ref gas are next to each other for each cycle
    pivot_wider(id_cols = ids,
                names_from = c(type, mass),
                values_from = value) %>%
    # clean up names
    purrr::set_names(~ str_replace_all(., "standard_", "r") %>%
                       str_replace_all("sample_", "s"))
  # NOTE: this is neat, but gets rid of all the extra info in cycle_dis etc.

  # so we add cycle_dis info back, in a very simplified form
  cycle_dis_dfr <- .data %>%
    ungroup() %>%
    select(-one_of(our_cols)) %>%
    pivot_wider(id_cols = ids,
                names_from = "type",
                values_from = c(v44_low, v44_high, v44_drop, drop_before, has_drop)) %>%
    group_by(file_id)

  left_join(out, cycle_dis_dfr, by = c("file_id", "cycle")) %>%
    as_tibble()
}
