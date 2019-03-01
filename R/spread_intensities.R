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
spread_intensities  <- function(.data, ids = NULL, our_cols = NULL, quiet = default(quiet)) {
  if (!quiet)
    message("Info: reshaping data into wide format.")

  if (!"type" %in% colnames(.data))
    stop("Column 'type' not found in ", colnames(.data))

  if (is.null(ids)) {
    ids <- c("file_id", "cycle")
  }

  if (is.null(our_cols)) {
    our_cols <- c("type", "v44.mV", "v45.mV", "v46.mV", "v47.mV", "v48.mV",
                  "v49.mV", "v54.mV")
  }

  out <- .data %>%
    group_by(file_id) %>%
    select(one_of(ids), one_of(our_cols)) %>%
    # gather into tidier format (file_id, cycle, type, mass, intensity)
    gather("mass", intensity, -file_id, -type, -cycle) %>%
    mutate(mass = str_sub(mass, 2, 3),
           type = ifelse(type == "sample", "s", "r")) %>%
    # unite type and mass into s44, r44, etc.
    unite(mir, type, mass, sep = "") %>%
    # spread them back out into format file_id, cycle, s44, s45, ..., r44, r45
    spread(mir, intensity)
  .data %>%
    ungroup() %>%
    select(-one_of(our_cols)) %>%
    right_join(out, ids)
}
