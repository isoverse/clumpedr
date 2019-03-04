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
#' @export
match_intensities <- function(.data, method = "normal", quiet = default(quiet)) {
  # global variables and defaults
  r44 <- r45 <- r46 <- r47 <- r48 <- r49 <- s44 <- s45 <- s46 <- s47 <- s48 <-
    s49 <- target_cycle_44 <- cycle <- NULL

  if (! method %in% c("normal", "linterp"))
    stop("Method '", method, "' should be either 'normal' or 'linterp'")
  if (!quiet)
    glue("Info: matching working gas intensities to sample gas, using method {method}") %>%
      message()
  .data %>%
    when(
      method == "normal" ~
        (.) %>%
        # target cycle brackets sample cycle
        mutate_at(vars(r44:r49),
                  funs((. + lead(.)) / 2)),
        # mutate(target_cycle_44 = cycle + .5),
      method == "linterp" ~
        (.) %>%
        # find matching intensity of mass 44 reference to sample gas
        mutate(target_cycle_44 = approx(x = r44, y = cycle, xout = s44)$y) %>%
        mutate_at(vars(r44:r49),
                  funs(approx(x = cycle, y = ., xout = target_cycle_44)$y)))
}
