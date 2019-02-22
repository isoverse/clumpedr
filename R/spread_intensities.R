#' Spread intensities from iso_read format into wide format.
#'
#' To be able to calculate ratios per cycle, it is necessary to put the sample
#' and reference as columns next to each other. This function transforms the
#' data into the desired format.
#'
#' @param dat a data frame or tibble containing mass intensities per cycle
#' @return A [tibble][tibble::tibble-package] with the sample and reference
#'   gasses side-by-side.
#' @export
spread_intensities  <- function(dat, quiet = default(quiet)) {
  if (!quiet)
    message("Info: re-ordering data into wide format.")
  if (!"type" %in% colnames(dat))
    stop("Column 'type' not found in ", colnames(dat))
  out <- dat %>%
    ungroup() %>%
    group_by(file_id) %>%
    select(file_id, type, cycle, v44.mV, v45.mV, v46.mV, v47.mV, v48.mV,
           v49.mV, v54.mV) %>%
    # gather into tidier format (file_id, cycle, type, mass, intensity)
    gather("mass", intensity, v44.mV, v45.mV, v46.mV, v47.mV, v48.mV,
           v49.mV, v54.mV) %>%
    mutate(mass = str_sub(mass, 2, 3)) %>%
    mutate(type = ifelse(type == "sample", "s", "r")) %>%
    # unite type and mass into s44, r44, etc.
    unite(mir, type, mass, sep = "") %>%
    # spread them back out into format file_id, cycle, s44, s45, ..., r44, r45
    spread(mir, intensity)
  ## DONE: do not remove the info columns (e.g. cycle_drop and hasdrop etc.),
  ## only the raw intensities
  ## TODO: or, in stead add nested raw intensities?
  ## TODO: figure out how to give a message with a character vector if (!quiet)
  ## message("Info: removing columns:\n", glue("{paste(names(dat)[!names(dat)
  ## %in% names(out)])}"))
  dat %>%
    ungroup() %>%
    ## TODO: make this more general so that it only gets rid of the type and
    ## raw intensities even when there is no half-cup mass 54 or when they're not in mV
    select(-c(type, v44.mV:v54.mV)) %>%
    right_join(out, c("file_id", "cycle"))
}
