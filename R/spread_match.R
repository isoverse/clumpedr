#' Spread and match sample gas and reference gas.
#'
# #' @param .data A [tibble][tibble::tibble-package], resulting from [correct_backgrounds()]
#' @inheritParams match_intensities
#' @export
spread_match <- function(.data, method = "normal", masses = c(44:49, 54), quiet = default(quiet)) {
  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }

  our_cols <- paste0("v", masses, ".mV")
  if (!all(our_cols %in% colnames(.data))) {
    warning(glue::glue("Columns {glue::glue_collapse(our_cols, sep = ', ', last = ', and ')} not found in data."))
    return(tibble(file_id = character()))
  }

  .data %>%
    # TODO: add duplicate check here?
    spread_intensities(our_cols = our_cols, quiet = quiet) %>%
    match_intensities(method = method, masses = masses, quiet = quiet)
}

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
  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }

  if (!quiet)
    message("Info: reshaping data into wide format.")

  if (!"type" %in% colnames(.data))
    stop(glue("Column 'type' not found in {paste(colnames(.data), collapse = ', ')}"))

  if (is.null(ids)) {
    ids <- c("file_id", "cycle", "Analysis")
  }

  if (is.null(our_cols)) {
    our_cols <- c("v44.mV", "v45.mV", "v46.mV", "v47.mV", "v48.mV", "v49.mV", "v54.mV")
  }

  out <- .data %>%
    group_by(.data$file_id, .data$Analysis) %>%
    # first lengthen it so that each row is one mass / unit / intensity
    pivot_longer(cols = our_cols,
                 names_to = c("mass", "unit"),
                 names_pattern = names_pattern) %>%
    # then widen it so that sample and ref gas are next to each other for each cycle
    pivot_wider(id_cols = c("file_id", "cycle", "Analysis"),
                names_from = c("type", "mass"),
                values_from = "value") %>%
    # clean up names
    purrr::set_names(~ str_replace_all(., "standard_", "r") %>%
                       str_replace_all("sample_", "s")) %>%
    ungroup(file_id)
  # NOTE: this is neat, but gets rid of all the extra info in cycle_dis etc.

  # so we add cycle_dis info back
  cycle_dis_dfr <- .data %>%
    select(-one_of(our_cols)) %>%
    pivot_wider(id_cols = ids,
                names_from = "type",
                ## NOTE: these are now hardcoded here, and are strictly related
                ## to what's happening in the internals of find_bad_cycles!!
                values_from = c("outlier_cycle_low",
                                "outlier_cycle_high",
                                "cycle_diff",
                                "cycle_drop",
                                "cycle_drop_num",
                                "outlier_cycle_drop",
                                "cycle_has_drop",
                                "outlier_cycle")) %>%
    purrr::set_names( ~ str_replace_all(., "^(.*)_standard", "\\1_r44") %>%
                        str_replace_all("^(.*)_sample", "\\1_s44"))

  left_join(out, cycle_dis_dfr, by = ids) %>%
    as_tibble()
}

#' Match the reference gas intensity to that of the sample gas
#'
#' This matches the reference gas intensity of mass 44 to the sample gas
#' intensity of mass 44 through linear interpolation (option `method =
#' "linterp"`), and then applies this same offset to the other masses.
#'
#' @param .data A [tibble][tibble::tibble-package] with s44--s49 and r44--r49; output of
#'     [spread_intensities()].
#' @param method "linterp" for linear interpolation, or "normal" for
#'     conventional bracketing of sample gas.
#' @param masses The masses to generate r and s columns from.
match_intensities <- function(.data, method = "normal", masses = c(44:49, 54), quiet = default(quiet)) {
  our_cols <- c(paste0("s", masses), paste0("r", masses))

  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }

  # global variables and defaults
  if (! method %in% c("normal", "linterp"))
    stop("Method '", method, "' should be either 'normal' or 'linterp'")

  if (!quiet)
    glue("Info: matching working gas intensities to sample gas, using method {method}") %>%
      message()

  .data %>%
    when(
      method == "normal" ~
        (.) %>%
        # reference gas cycles bracket sample cycles
        mutate_at(vars(one_of(str_subset(our_cols, "^r"))),
                  list(~ (. + lag(.)) / 2)),
        # mutate(target_cycle_44 = cycle + .5),
      method == "linterp" ~
        (.) %>%
        # find matching intensity of mass 44 reference to sample gas
        mutate(target_cycle_44 = approx(x = .data$r44, y = .data$cycle, xout = .data$s44)$y) %>%
        mutate_at(vars(one_of(str_subset(our_cols, "^r"))),
                  list(~ approx(x = .data$cycle, y = ., xout =.data$target_cycle_44)$y))) %>%
    filter(cycle != 0) %>% # cycle 0 of the ref gas is no longer needed
    # create the summary outlier column
    mutate(cycle_has_drop = cycle_has_drop_s44 | cycle_has_drop_r44,
           outlier_cycle = outlier_cycle_s44 | outlier_cycle_r44)
}
