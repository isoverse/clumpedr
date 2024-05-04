#' Background corrections
#'
#' This function applies a very simple background correction based on the cup
#' 54 intensity and an input scaling factor.
#'
#' @param .data The dataframe with raw iso files.
#' @param factor Factor by which to multiply the half-cup before subtraction.
#' @param i47 Column with mass 47 intensities to correct.
#' @param i54 Column with mass 47.5 intensities to use for correction.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
#' @export
correct_backgrounds  <- function(.data, factor, ...,
                                 i47 = v47.mV, i54 = v54.mV,
                                 quiet = default(quiet)) {
  # global variables and defaults
  v47.mV <- v54.mV <- NULL
  rlang::check_dots_empty0(...)

  if (!quiet)
    glue("Info: adding background based on half-mass with factor {factor}") %>%
      message()

  .data %>%
    mutate({{ i47 }} := {{ i47 }} - (factor * {{ i54 }}))
}
