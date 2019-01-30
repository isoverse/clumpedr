#' spread_match
#'
#' Spread and match sample gas and reference gas.
#'
# #' @param dat A [tibble][tibble::tibble-package], resulting from [correct_backgrounds()]
#' @inheritParams spread_intensities
#' @inheritParams match_intensities
spread_match <- function(dat, method = "normal", quiet = default(quiet)) {
    dat %>%
        spread_intensities(quiet = quiet) %>%
        match_intensities(method = method, quiet = quiet)
}
