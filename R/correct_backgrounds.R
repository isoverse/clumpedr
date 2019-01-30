#' Background corrections
#'
#' This function applies a very simple backgorund correction based on the
#' half-cup mass 54 intensity.
#'
#' @param dat The dataframe with raw iso files.
#' @param factor Factor by which to multiply the half-cup before subtraction.
#' @export
correct_backgrounds  <- function(dat, factor,
                                 ## i47 = quo(m47.mV), i54 = quo(m54.mV),
                                 method = "linterp",
                                 quiet = default(quiet)) {
    if (!quiet)
        glue("Info: adding background based on half-mass with factor {factor}") %>%
            message()
    dat %>%
        mutate(v47.mV = v47.mV - factor * v54.mV)
        ## mutate(!!i47 := !!i47 - factor * !!i54)  # with variable names
        ## mutate(s47 = s47 - factor * s54,         # when they're already side-by-side
               ## r47 = r47 - factor * r54)
}
