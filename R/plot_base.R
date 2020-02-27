#' Create a basic plot layer for further expansion.
#'
#' This function creates a ggplot with all of the metadata applied to named
#' default aesthetics. This is only useful when creating interactive plot with
#' [ggplotly][plotly::ggplotly], since all the metadata aesthetics show up when
#' hovering the mouse over a point.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from any of the
#'     processing functions that returns the iso_file_info.
#' @param ... Additional aesthetics to pass to ggplot.
#' @export
plot_base <- function(.data, ...) {
  .data %>%
    ggplot(aes(file_id = file_id, file_path = file_path,
               file_subpath = file_subpath,
               file_datetime = file_datetime, row = Row,
               peak_center = `Peak Center`,
               background = Background, press_adj = Pressadjust,
               ref_refill = `Reference Refill`, line = Line,
               sample = Sample, weight = `Weight [mg]`,
               id1 = `Identifier 1`, analysis = Analysis,
               comment = Comment, prep = Preparation,
               meth = Method,
               ms_int_time = MS_integration_time.s,
               masspec = masspec, broadid = broadid,
               s44_init = s44_init, r44_init = r44_init, ...))
}
