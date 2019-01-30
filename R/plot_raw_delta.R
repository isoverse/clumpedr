#' Plot raw delta values as a function of measurement time.
#'
#' Cycles are averaged and pointranges indicate one standard deviation offset
#' based on the number of cycles taken into account.
#'
#' @details Note that with δ^13^C and δ^18^O, the pointrange is invisible
#'     because the cycles have so little variance that the line segment usually
#'     falls within the dots.
#'
#' @param dat The input tibble, result of `bulk_and_clumping_deltas()`.
#' @param info The metadata of the raw cycles, used to get the `file_datetime`.
#' @param rawpoints Logical, whether to include individual cycle estimates (defaults to `FALSE`).
#' @param pointalpha The alpha value of the raw points if they are plotted.
#' @export
plot_raw_delta <- function(dat, info, column = quo(D47raw), rawpoints = FALSE,
                           pointalpha = .5, quiet = default(quiet)) {
    if (!quiet)
        glue("Info: generating a plot of raw delta value {quos_to_text(column)} as a function of measurement time.") %>%
            message()
    pl <- dat %>%
        select(file_id, !!column) %>%
        left_join(info, by = "file_id") %>%
        group_by(file_id) %>%
        ggplot(aes(x = file_datetime, y = !!column, colour = broadid,
                   ## these are not actually aesthetics, but if I pass them
                   ## here I can view them in the interactive ggplotly plot.
                   file = file_id,
                   id1 = `Identifier 1`,
                   row = Row, weight = `Weight [mg]`, comment = Comment,
                   prep = Preparation, meth = Method)) +
        stat_summary(fun.y = mean,
                     fun.ymin = function(x) mean(x) - sd(x),
                     fun.ymax = function(x) mean(x) + sd(x),
                     geom = "pointrange")
    if (rawpoints) {
        pl <- pl + geom_point(alpha = pointalpha)
    }

    pl
}
