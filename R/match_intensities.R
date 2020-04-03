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
