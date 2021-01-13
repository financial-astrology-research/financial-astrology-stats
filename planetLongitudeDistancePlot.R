# Title     : Planets longitude distance based on slow planets plots.
# Objective : Plot all planets longitude distance against one planet to help visualize all planets aspects
#             angles interactions to note when a given angle is strengthen.
# Created by: pablocc
# Created on: 13/01/2021

library(ggplot2)

source("./fileSystemUtilities.R")
source("./planetAspectsDataPrepare.R")

ggplotDarkTheme <- function(base_size = 12) {
  theme_grey(base_size = base_size) %+replace%
    theme(
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size * 0.8, color = "white", lineheight = 0.9),
      axis.text.y = element_text(size = base_size * 0.8, color = "white", lineheight = 0.9),
      axis.ticks = element_line(color = "white", size = 0.2),
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      legend.background = element_rect(color = NA, fill = "black"),
      legend.key = element_rect(color = "white", fill = "black"),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size * 0.8, color = "white"),
      legend.title = element_text(size = base_size * 0.8, face = "bold", hjust = 0, color = "white"),
      legend.position = "right",
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = "vertical",
      legend.box = NULL,
      panel.background = element_rect(fill = "black", color = NA),
      panel.border = element_rect(fill = NA, color = "white"),
      panel.grid.major = element_line(color = "grey35"),
      panel.grid.minor = element_line(color = "grey20"),
      panel.spacing = unit(0.5, "lines"),
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size * 0.8, color = "white"),
      strip.text.y = element_text(size = base_size * 0.8, color = "white", angle = -90),
      plot.background = element_rect(color = "black", fill = "black"),
      plot.title = element_text(size = base_size * 1.2, color = "white"),
      plot.margin = unit(rep(1, 4), "lines")
    )
}

planetLongitudeDistancePlotTheme <- function() {
  list(
    ggplotDarkTheme(),
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
    theme(axis.ticks.length = unit(0, "null")),
    theme(axis.ticks.margin = unit(0, "null")),
    theme(axis.text.x = element_text(angle = 90, size = 10))
  )
}

#' Provides ggplot horizontal reference lines for all modern astrology angles.
aspectsReferenceZonesLines <- function() {
  list(
    geom_hline(yintercept = 180, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 150, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 135, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 120, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 103, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 90, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 52, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 60, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 45, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 30, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
    geom_hline(yintercept = 0, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7)
  )
}

planetsLongitudesDistanceAxesCustomize <- function(dateBreaks, dateRangeLimits) {
  todayDate <- Sys.Date()
  list(
    geom_vline(xintercept = todayDate, linetype = "dashed", color = "white", size = 0.6, alpha = 0.7),
    labs(x = "Date", y = "Distance Angle"),
    scale_y_continuous(breaks = seq(0, 180, by = 5)),
    scale_x_date(date_breaks = dateBreaks, date_labels = "%b %Y", limits = dateRangeLimits)
  )
}

#' Plot all planets longitude distance from Uranus.
#' @param planetPositionsTable Daily planets position data table.
planetsLongitudeDistanceForUranusPlot <- function(planetPositionsTable) {
  dateBreaks <- "1 month"
  dateRangeLimits <- c(Sys.Date() - (365 * 3), Sys.Date())
  planetPositionsTableFiltered <- planetPositionsTable[
    Date >= dateRangeLimits[1] & Date <= Sys.Date(),
  ]

  distancesPlot <- ggplot(data = planetPositionsTableFiltered) +
    geom_point(aes(x = Date, y = SUURLON, size = 1), colour = "yellow", alpha = 0.5, size = 1) +
    geom_point(aes(x = Date, y = MOURLON, size = 1), colour = "black", alpha = 0.6, size = 1) +
    geom_point(aes(x = Date, y = MEURLON, size = 1), colour = "orange", alpha = 0.5, size = 1) +
    geom_point(aes(x = Date, y = VEURLON, size = 1), colour = "pink", alpha = 0.7, size = 1) +
    geom_point(aes(x = Date, y = MAURLON, size = 1), colour = "red", alpha = 0.6, size = 1) +
    geom_point(aes(x = Date, y = JUURLON, size = 1), colour = "palegreen3", alpha = 0.6, size = 1) +
    geom_point(aes(x = Date, y = SAURLON, size = 1), colour = "gray", alpha = 0.6, size = 1) +
    geom_point(aes(x = Date, y = URNELON, size = 1), colour = "mediumaquamarine", alpha = 0.6, size = 1) +
    aspectsReferenceZonesLines() +
    planetsLongitudesDistanceAxesCustomize(dateBreaks, dateRangeLimits) +
    planetLongitudeDistancePlotTheme()

  targetFileName <- paste0(visualizationsDataDestinationPath(), "planets_longitude_distance_from_uranus.png")
  ggsave(
    filename = targetFileName,
    plot = distancesPlot,
    width = 40,
    units = "cm"
  )

  cat("Plot saved to:", targetFileName)
}

planetPositionsTable <- loadPlanetsPositionTable("daily")
planetPositionsTable <- planetLongitudesDistanceDataAugment(planetPositionsTable, modernPlanets())
planetsLongitudeDistanceForUranusPlot(planetPositionsTable)