# Title     : Daily planet events with asset frequency statistics report.
# Objective : Support the daily astrological events effects analysis based on asset historical price effect statistics.
# Created by: pablocc
# Created on: 29/01/2021

library(magrittr)

source("./fileSystemUtilities.R")

#' Load daily planets positions data table from CSV.
#' @return Daily planets positions data table.
dailyPlanetsPositionLoad <- function() {
  planetPositionsPathFileName <- paste0(astroDataDestinationPath(), "daily_planets_positions_long.csv")
  fread(planetPositionsPathFileName)
}

#' Report the planets zodiacal positions with historical asset price effect frequencies.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet positions with price effect frequencies report table.
dailyPlanetsSignsReport <- function(symbolID) {
  nowDate <- Sys.Date()
  sourceFileName <- paste(symbolID, "planet_zodsign", "buy_sell_count_freq_stats", sep = "-")
  statsPathFileName <- paste0(statsDataDestinationPath(symbolID), sourceFileName, ".csv")
  frequencyTable <- fread(statsPathFileName)
  frequencyTable[, pID := substr(PlanetZodSign, 1, 2)]
  frequencyTable[, zsign := substr(PlanetZodSign, 4, 6)]
  dailyPlanetsPosition <- dailyPlanetsPositionLoad()
  reportPlanetsPosition <- dailyPlanetsPosition[Date == nowDate, c('Date', 'pID', 'zsign')]
  dailyReportTable <- merge(reportPlanetsPosition, frequencyTable, by = c('pID', 'zsign'))
  dailyReportTable[, c('pID', 'zsign') := NULL]
}

dailyPlanetsSignsReport("BTC-USD") %>% print()