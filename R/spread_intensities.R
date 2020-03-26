#' Spread intensities from iso_read format into wide format.
#'
#' To be able to calculate ratios per cycle, it is necessary to put the sample
#' and reference as columns next to each other. This function transforms the
#' data into the desired format.
#'
#' @param .data A [tibble][tibble::tibble-package] containing mass intensities per cycle.
#' @param ids Identifying columns that we'll group by.
#' @param our_cols Columns with data values that need to be reshaped. Defaults to v44.mV to v54.mV.
#' @return A [tibble][tibble::tibble-package] with the sample and reference
#'   gasses side-by-side.
spread_intensities  <- function(.data, ids = NULL, our_cols = NULL,
                                names_pattern = "v([4-9]{2}).(mV)", quiet = default(quiet)) {
  if (!quiet)
    message("Info: reshaping data into wide format.")

  if (!"type" %in% colnames(.data))
    stop(glue("Column 'type' not found in {paste(colnames(.data), collapse = ', ')}"))

  if (is.null(ids)) {
    ids <- c("file_id", "cycle")
  }

  if (is.null(our_cols)) {
    our_cols <- c("v44.mV", "v45.mV", "v46.mV", "v47.mV", "v48.mV", "v49.mV", "v54.mV")
  }

  out <- .data %>%
    group_by(file_id) %>%
    # first lengthen it so that each row is one mass / unit / intensity
    pivot_longer(cols = our_cols,
                 names_to = c("mass", "unit"),
                 names_pattern = names_pattern) %>%
    # then widen it so that sample and ref gas are next to each other for each cycle
    pivot_wider(id_cols = c("file_id", "cycle"),
                names_from = c(.data$type, .data$mass),
                values_from = .data$value) %>%
    # clean up names
    purrr::set_names(~ str_replace_all(., "standard_", "r") %>%
                       str_replace_all("sample_", "s")) %>%
    ungroup(file_id)
  # NOTE: this is neat, but gets rid of all the extra info in cycle_dis etc.

  # so we add cycle_dis info back
  cycle_dis_dfr <- .data %>%
    select(-one_of(our_cols)) %>%
    pivot_wider(id_cols = ids,
                names_from = .data$type,
                ## NOTE: these are now hardcoded here, and are strictly related
                ## to what's happening in the internals of find_bad_cycles!!
                values_from = c(.data$outlier_cycle_low,
                                .data$outlier_cycle_high,
                                .data$cycle_diff,
                                .data$cycle_drop,
                                .data$cycle_drop_num,
                                .data$outlier_cycle_drop,
                                .data$cycle_has_drop,
                                .data$outlier_cycle)) %>%
    purrr::set_names( ~ str_replace_all(., "^(.*)_standard", "\\1_r44") %>%
                        str_replace_all("^(.*)_sample", "\\1_s44"))

  left_join(out, cycle_dis_dfr, by = ids) %>%
    as_tibble()
}
