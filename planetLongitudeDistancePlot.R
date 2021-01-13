# Title     : Planets longitude distance based on slow planets plots.
# Objective : Plot all planets longitude distance against one planet to help visualize all planets aspects
#             angles interactions to note when a given angle is strengthen.
# Created by: pablocc
# Created on: 13/01/2021

planetLongitudeDistancePlotTheme <- function() {
  list(
    theme_black(),
    theme(panel.spacing = unit(c(0, 0, 0, 0), "null")),
    theme(plot.margin = unit(c(0, 0, 0, 0), "null")),
    theme(panel.grid = element_blank()),
    theme(panel.border = element_blank()),
    theme(plot.margin = unit(c(0, 0, 0, 0), "null")),
    theme(panel.spacing = unit(c(0, 0, 0, 0), "null")),
    theme(axis.ticks = element_blank()),
    theme(axis.text = element_blank()),
    theme(axis.title = element_blank()),
    theme(axis.line = element_blank()),
    theme(legend.position = "none"),
    theme(axis.ticks.length = unit(0, "null")),
    theme(axis.ticks.margin = unit(0, "null")),
    theme(legend.margin = unit(0, "null")),
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()),
    theme(axis.title.y = element_blank()))
}

#' Provides ggplot horizontal reference zone lines for the modern astrology angles.
aspectsReferenceZonesLines <- function() {
  list(
    geom_hline(yintercept = 165, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 155, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 145, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 140, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 130, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 125, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 115, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 100, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 80, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 65, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 55, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 50, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 40, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 35, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 25, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 15, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7)
  )
}

planetsLongitudesDistanceAxesCustomize <- function(dataDates, dateBreaks, chartPeriod) {
  list(
    geom_vline(xintercept = dataDates, linetype = "dashed", color = "white", size = 0.6, alpha = 0.7),
    scale_y_continuous(breaks = seq(0, 180, by = 10)),
    scale_x_date(date_breaks = dateBreaks, limits = chartPeriod)
  )
}

#' Plot all planets longitude distance from Uranus.
#' @param planetsPositionTable Daily planets position data table.
planetsLongitudeDistanceForUranusPlot <- function(planetsPositionTable) {
  p <- ggplot(data = dailyPlanetsResearch) +
    geom_point(aes(x = Date, y = SUURLON, size = 1), colour = "yellow", alpha = 0.5, size = 1) +
    geom_point(aes(x = Date, y = MOURLON, size = 1), colour = "black", alpha = 0.6, size = 1) +
    geom_point(aes(x = Date, y = MEURLON, size = 1), colour = "orange", alpha = 0.5, size = 1) +
    geom_point(aes(x = Date, y = VEURLON, size = 1), colour = "pink", alpha = 0.7, size = 1) +
    geom_point(aes(x = Date, y = MAURLON, size = 1), colour = "red", alpha = 0.6, size = 1) +
    geom_point(aes(x = Date, y = JUURLON, size = 1), colour = "palegreen3", alpha = 0.6, size = 1) +
    geom_point(aes(x = Date, y = SAURLON, size = 1), colour = "gray", alpha = 0.6, size = 1) +
    geom_point(aes(x = Date, y = NNURLON, size = 1), colour = "mediumaquamarine", alpha = 0.6, size = 1) +
    aspectsReferenceZonesLines() +
    planetsLongitudesDistanceAxesCustomize() +
    planetLongitudeDistancePlotTheme()
}
