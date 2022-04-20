#' Empirical Transfer Function
#'
#' Calculate and apply the empirical transfer function (ETF). First append the
#' expected values for the standards, then calculate the ETF per Preparation,
#' then apply the ETF to all the samples and standards.
#'
#' @param .data A [tibble][tibble::tibble-package] resulting from [collapse_cycles()].
#' @inheritParams append_expected_values
#' @inheritParams calculate_etf
#'
#' @details This function is a wrapper for [append_expected_values()],
#'   [calculate_etf()], and [apply_etf()].
#'
#' @family empirical transfer functions
# http://r-pkgs.had.co.nz/man.html#dry2
# TODO: use @describeIn or @rdname to also include the references in the wrapper.
#' @export
empirical_transfer_function <- function(.data,
                                        # columns for append_expected_values
                                        std_names = paste0("ETH-", 1:3),
                                        std_values = c(0.258, 0.256, 0.691) - 0.062, #0.507),
                                        ## outlier = outlier,
                                        raw = D47_raw,
                                        exp = expected_D47,
                                        session = Preparation,
                                        id1 = `Identifier 1`,
                                        # output columns for calculate_etf
                                        etf = etf,
                                        etf_coefs = etf_coefs,
                                        slope = slope,
                                        intercept = intercept,
                                        # for apply_etf
                                        out = D47_etf,
                                        outlier = outlier,
                                        quiet = default(quiet),
                                        parallel = FALSE) {
  # defaults from above
  D47_raw <- expected_D47 <- D47_etf <- `Identifier 1` <- Preparation <- NULL

  if (!quiet)
    glue("Info: calculating and applying Emperical Transfer Function, with {quo_name(enquo(raw))} as a function of {quo_name(enquo(exp))}, for each {quo_name(enquo(session))}.") |>
      message()

  .data |>
    append_expected_values(std_names = std_names, std_values = std_values,
                           exp = {{ exp }}, by = {{ id1 }}, quiet = TRUE) |>
    calculate_etf(raw = {{ raw }}, exp = {{ exp }}, session = {{ session }},
                  etf = {{ etf }}, etf_coefs = {{ etf_coefs }},
                  slope = {{ slope }}, intercept = {{ intercept }},
                  parallel = parallel, quiet = TRUE) |>
    apply_etf(intercept = {{ intercept }}, slope = {{ slope }},
              raw = {{ raw }}, out = {{ out }}, quiet = TRUE)
}
