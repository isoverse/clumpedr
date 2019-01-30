#' Match the reference gas intensity to that of the sample gas
#'
#' This matches the reference gas intensity of mass 44 to the sample gas
#' intensity of mass 44 through linear interpolation (option `method =
#' "linterp"`), and then applies this same offset to the other masses.
#'
#' @param dat A dataframe with s44--s49 and r44--r49, output of
#'     `spread_intensities()`.
#' @param method "linterp" for linear interpolation, or "normal" for
#'     conventional bracketing of sample gas.
#' @export
match_intensities <- function(dat, method = "normal", quiet = default(quiet)) {
    if (! method %in% c("normal", "linterp"))
        stop("Method '", method, "' should be either 'normal' or 'linterp'")
    if (!quiet)
        glue("Info: matching working gas intensities to sample gas, using method {method}") %>%
            message()
    dat %>%
        when(
            method == "normal" ~
                (.) %>%
                ## target cycle brackets sample cycle
                mutate_at(vars(r44:r49),
                          funs((. + lead(.)) / 2)),
                ## mutate(target_cycle_44 = cycle + .5),
            method == "linterp" ~
                (.) %>%
                ## find matching intensity of mass 44 reference to sample gas
                mutate(target_cycle_44 = approx(x = r44, y = cycle, xout = s44)$y) %>%
                mutate_at(vars(r44:r49),
                  funs(approx(x = cycle, y = ., xout = target_cycle_44)$y)))
}
