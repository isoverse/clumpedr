#' Create a basic plot layer for further expansion.
#'
#' This function creates a ggplot with all of the metadata applied to named
#' default aesthetics. This is only useful when creating interactive plot with
#' [ggplotly][plotly::ggplotly], since all the metadata aesthetics show up when
#' hovering the mouse over a point.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from any of the
#'     processing functions that returns the metadata.
#' @param ... Additional aesthetics to pass to ggplot.
#' @export
plot_base <- function(.data, ...) {
  .data %>%
    ggplot(aes(colour = .data$broadid, #shape = .data$outlier,
               # the rest are fake aesthetics for plotly
               file_id = .data$.data$file_id, file_path = .data$file_path,
               file_subpath = .data$file_subpath,
               file_datetime = .data$file_datetime, row = .data$Row,
               peak_center = .data$`Peak Center`,
               background = .data$Background, press_adj = .data$Pressadjust,
               ref_refill = .data$`Reference Refill`, line = .data$Line,
               sample = .data$Sample, weight = .data$`Weight [mg]`,
               id1 = .data$`Identifier 1`, analysis = .data$Analysis,
               comment = .data$Comment, prep = .data$Preparation,
               meth = .data$Method, #meas_info = .data$measurement_info,
               ms_int_time = .data$MS_integration_time.s, ## id2 = .data$`Identifier 2`,
               masspec = .data$masspec, broadid = .data$broadid,
               s44_init = .data$s44_init, r44_init = .data$r44_init, ...)) +
    labs(colour = "Broad Identifier")
}
