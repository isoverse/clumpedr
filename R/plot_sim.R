#' Plot output of single simulation
#'
#' This function TODO
#'
#' @param std TODO
#' @param smp TODO
#' @param cis TODO
#' @param stdtable TODO
#' @param smpinfo TODO
#' @param stdev TODO
#' @return A ggplot object.
#' @references
#'   Brandon M. Greenwell and Christine M. Schubert Kabban (2014).
#'   investr: An R Package for Inverse Estimation. The R Journal, 6(1),
#'   90-100.
#'   \url{https://journal.r-project.org/archive/2014-1/greenwell-kabban.pdf}
#' @import ggplot2
#' @import dplyr
#' @export
plot_sim <- function(std, smp, cis, stdtable, smpinfo, rawstdev) {
  # basic plot setup: expected on x-axis, raw measurement on y-axis
  base_plot <- bind_rows(stdtable %>% filter(id %in% std$id), smpinfo) %>%
    ggplot(aes(x = D47.noacid, y = rawcat, col = id)) +
    geom_pointrange(aes(
      ymin = rawcat - stdev / 1e3,
      ymax = rawcat + stdev / 1e3
    ),
    size = 2, linetype = 2, alpha = .1, show.legend = FALSE
    ) +
    # add the input sample measurements
    geom_abline(
      intercept = kintercept, slope = kslope,
      linetype = 2, size = 2, col = "red"
    ) +
    # make it pretty, manual colour scale, samples are black
    scale_colour_manual(
      limits = c(stdtable$id, smpinfo$id, "etf"),
      values = c(stdtable$col, smpinfo$col, "steelblue")
    ) +
    # nice axis labels
    labs(
      colour = "ID", x = Delta[47] ~ expected ~ "(\u2030)",
      y = Delta[47] ~ raw ~ "(\u2030)"
    ) +
    coord_fixed()
  pl <- base_plot +
    # add the calculated ETF
    geom_smooth(aes(group = 1, y = raw),
      colour = "steelblue", method = "lm",
      data = std, fullrange = TRUE
    ) +
    # add segment for mean sample
    geom_segment(aes(
      x = -Inf, xend = mean(D47.fromraw), y = mean(raw),
      yend = mean(raw)
    ), data = smp, col = "gray", alpha = .2) +
    geom_segment(aes(
      x = mean(D47.fromraw), xend = mean(D47.fromraw),
      y = mean(raw), yend = -Inf
    ), data = smp, col = "gray", alpha = .2) +
    # add segment for samples from y-axis to regression line
    ## geom_segment(aes(x = -Inf, xend = D47.fromraw, y = raw, yend = raw),
    ## data = smp, col = "gray", alpha = .2) +
    # add segment for samples from regression line to x-axis
    ## geom_segment(aes(x = D47.fromraw, xend = D47.fromraw, y = raw, yend = -Inf),
    ## data = smp, col = "gray", alpha = .2) +
    # add the raw sample and standard measurements
    geom_violin(aes(x = D47.noacid, y = raw),
      scale = "count",
      width = .1, data = bind_rows(smp, std),
      alpha = .3, position = position_identity()
    ) +
    geom_point(aes(x = D47.noacid, y = raw),
      shape = 1,
      data = bind_rows(smp, std)
    ) +
    # the 95% CI for the raw values of the samples
    geom_pointrange(aes(
      x = at, y = mean,
      ymin = lwr, ymax = upr, col = id
    ),
    data = cis %>% filter(dir == "raw"), size = 1.4
    ) +
    # TODO: choose from below
    # compared to the 95% confidence interval of the regression at this point
    # compared to the 95% prediction interval of the regression at this point??
    ## add the converted average and 95% CI in the x-axis direction
    geom_errorbarh(aes(y = at, x = mean, xmin = lwr, xmax = upr),
      data = cis %>% filter(dir == "exp"), colour = smpinfo$col, height = 0, size = 1.4
    ) +
    geom_point(aes(x = mean, y = at),
      data = cis %>% filter(dir == "exp"), colour = smpinfo$col, size = 3
    )
  return(pl)
}
