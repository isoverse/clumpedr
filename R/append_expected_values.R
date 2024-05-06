#' Append expected values
#'
#' Append the expected values for the standards. Defaults to using
#' ETH-1--ETH-3.
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param std_names Names of the standards.
#' @param std_values Expected values of the standards. Defaults to Bernasconi et al., 2021.
#' @param exp Name of the new column that will hold expected values.
#' @param by Name of the standard/sample identifier column.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
#' @returns Same as `.data` but with new column in `exp`.
#' @examples
#' standards |>
#'   isoreader::iso_get_raw_data(include_file_info = "Identifier 1") |>
#'   append_expected_values(std_names = c("ETH-1", "ETH-2", "ETH-3"),
#'                          std_values = c(0.2052, 0.2085, 0.6132),
#'                          exp = expected_D47,
#'                          by = `Identifier 1`)
#' @references
#' Bernasconi, S. M., Daëron, M., Bergmann, K. D., Bonifacie, M., Meckler, A.
#'   N., Affek, H. P., Anderson, N., Bajnai, D., Barkan, E., Beverly, E.,
#'   Blamart, D., Burgener, L., Calmels, D., Chaduteau, C., Clog, M.,
#'   Davidheiser-Kroll, B., Davies, A., Dux, F., Eiler, J., … Ziegler, M.
#'   (2021). InterCarb: A community effort to improve inter-laboratory
#'   standardization of the carbonate clumped isotope thermometer using
#'   carbonate standards. Geochemistry, Geophysics, Geosystems, 22.
#'   <https://doi.org/10.1029/2020GC009588>
#' @export
#' @family empirical transfer functions
append_expected_values <- function(.data,
                                   ...,
                                   std_names = paste0("ETH-", 1:3),  # we don't use ETH-4!
                                   std_values = c(0.2052, 0.2085, 0.6132),
                                   exp = expected_D47,
                                   by = `Identifier 1`,
                                   quiet = NULL) {
  # global variables and defaults
  `Identifier 1` <- expected_D47 <- NULL
  rlang::check_dots_empty0(...)

  if (length(std_names) != length(std_values))
    stop("std_names should be of equal length to std_values.")

  if (anyNA(std_values)) {
    std_names <- std_names[!is.na(std_values)]
    std_values <- std_values[!is.na(std_values)]
  }

  if (is.null(quiet)) {
    quiet <- default(quiet)
  }

  if (!quiet)
    glue("Info: Appending expected values as {quo_name(enquo(exp))} for standards {glue::glue_collapse(unique(std_names), sep = ' ', last = ' and ', width = 30)}") %>%
      message()

  expected_standard_values <- tibble({{ by }} := std_names,
                                     {{ exp }} := std_values)

  by_quo_name <- quo_name(enquo(by))

  .data %>%
    left_join(expected_standard_values, by = by_quo_name)
}
