#' Background corrections
#'
#' This function applies a very simple background correction based on the cup
#' 54 intensity and an input scaling factor.
#'
#' @param .data The dataframe with raw iso files.
#' @param factor Factor by which to multiply the half-cup before subtraction.
#' @param i47 Column with mass 47 intensities to correct. Defaults to `v47.mV`.
#' @param i54 Column with mass 47.5 intensities to use for correction. Defaults to `v54.mV`.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
#' @export
#' @returns Same as `.data` but with altered i47 column.
#' @examples
#' # generate some fake data
#' dat <- tibble::tibble(v47.mV = seq(0, 1, length.out = 10), v54.mV = seq(.1, 0, length.out = 10))
#' # correct the mass 47 using the half-cup
#' dat <- dat |> correct_backgrounds(factor = 1)
#' # or use the example data included
#' isoreader::iso_get_raw_data(standards) |>
#'   correct_backgrounds(1.1)
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
