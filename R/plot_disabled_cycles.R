#' Plot disabled cycles
#'
#' @param dat Dataframe with output from `disable_cycles()`.
#' @family cycle functions
#' @export
plot_disabled_cycles  <- function(dat, min = 1500, max = 50000, quiet = default(quiet)) {
    if (!quiet)
      glue("Info: generating plot of {length(unique(dat$file_id))} data files, of which {length(unique(pull(filter(dat, cycle_dis), file_id)))} have a drop in pressure.") %>%
            message()
    # we need a weird group for the lines
    pld <- dat %>%
        mutate(grp = paste(file_id, type, sep = "_"),
               linegrps = ifelse(hasdrop,
                            ifelse(cycle_dis, "disabled", "has a drop"),
                            "no bad cycles"))
    ## TODO: convert linegrps to factor with desired order

    pld %>%
        ggplot(aes(x = cycle, y = v44.mV, group = grp, colour = linegrps, alpha = linegrps, size = linegrps)) +
        ## cleanly
        geom_line() +
        geom_point() +
        scale_colour_manual(values = c("red", "orange", "gray")) +
        scale_alpha_manual(values = c(.6, .2, .1)) +
        scale_size_manual(values = c(3, 2, 1)) +
        ## scale_shape_manual(values = c(16, NA, NA)) +
        ## separately, so that the disabled ones are plotted on top.
        ## the good cycles
        ## geom_line(colour = "black", alpha = .1, data = filter(pld, !cycle_dis)) +
        ## ## the good part of the bad one
        ## geom_line(colour = "orange", alpha = .6, size = 1.5, data = filter(pld, hasdrop)) +
        ## ## the bad part of the bad one
        ## geom_line(colour = "red", alpha = .5, size = 1.5, data = filter(pld, hasdrop & cycle_dis)) +
        ## omitted points
        ## geom_point(colour = "red", alpha = .9, data = filter(pld, cycle_dis)) +
        geom_hline(yintercept = range(min, max), col = "indianred", size = 2) +
        facet_grid(cols = vars(type))
}
