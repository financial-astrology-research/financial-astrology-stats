# Title     : Daily planet events with asset frequency statistics report.
# Objective : Support the daily astrological events effects analysis based on asset historical price effect statistics.
# Created by: pablocc
# Created on: 29/01/2021

library(data.table)
library(magrittr)

source("./fileSystemUtilities.R")

#' Load daily planets positions data table from CSV.
#' @return Daily planets positions data table.
dailyPlanetsPositionLoad <- function() {
  planetPositionsPathFileName <- paste0(astroDataDestinationPath(), "daily_planets_positions_long.csv")
  fread(planetPositionsPathFileName)
}

#' Load daily planets aspects data table from CSV.
#' @return Daily planets positions data table.
dailyPlanetsAspectsLoad <- function() {
  planetsAspectsPathFileName <- paste0(astroDataDestinationPath(), "aspects_all_planets_pablo_aspects_set_long.csv")
  fread(planetsAspectsPathFileName)
}

#' Report the planets zodiacal positions with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet positions with price effect frequencies report table.
dailyPlanetsSignsReport <- function(reportDate, symbolID) {
  sourceFileName <- paste(symbolID, "planet_zodsign", "buy_sell_count_freq_stats", sep = "-")
  statsPathFileName <- paste0(statsDataDestinationPath(symbolID), sourceFileName, ".csv")
  frequencyTable <- fread(statsPathFileName)
  frequencyTable[, pID := substr(PlanetZodSign, 1, 2)]
  frequencyTable[, zsign := substr(PlanetZodSign, 4, 6)]
  dailyPlanetsPosition <- dailyPlanetsPositionLoad()
  reportPlanetsPosition <- dailyPlanetsPosition[Date == reportDate, c('Date', 'pID', 'zsign')]
  dailyReportTable <- merge(reportPlanetsPosition, frequencyTable, by = c('pID', 'zsign'))
  dailyReportTable[, c('pID', 'zsign') := NULL]
}

#' Report the planets aspects with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet positions with price effect frequencies report table.
dailyPlanetsAspectsReport <- function(reportDate, symbolID) {
  sourceFileName <- paste(symbolID, "planets_aspects", "buy_sell_count_freq_stats", sep = "-")
  statsPathFileName <- paste0(statsDataDestinationPath(symbolID), sourceFileName, ".csv")
  frequencyTable <- fread(statsPathFileName)
  frequencyTable[, pX := substr(PlanetsAspect, 1, 2)]
  frequencyTable[, pY := substr(PlanetsAspect, 3, 4)]
  frequencyTable[, aspect := substr(PlanetsAspect, 6, 10)]
  dailyPlanetsAspects <- dailyPlanetsAspectsLoad()
  reportPlanetsPosition <- dailyPlanetsAspects[Date == reportDate,]
  # Filter only the exact orb aspects.
  reportPlanetsPosition <- reportPlanetsPosition[orb <= 1]
  dailyReportTable <- merge(
    reportPlanetsPosition,
    frequencyTable,
    by = c('pX', 'pY', 'aspect')
  )
  dailyReportTable[, c('pX', 'pY', 'aspect', 'origin') := NULL]
  dailyReportTable[order(Date, orb)]
}

symbolID <- readline("Enter an asset symbol: ")
reportDate <- readline("Enter a date in YYYY-DD-MM format: ")
reportDate <- try(as.Date(reportDate, format="%Y-%m-%d"))
if("try-error" %in% class(reportDate) || is.na(reportDate)) {
  stop("The date is invalid, please try again.")
}

cat("\nDAILY PLANET ZODIAC SIGN POSITION:\n\n")
dailyPlanetsSignsReport(reportDate, symbolID) %>% print()
cat("\nDAILY PLANETS ASPECTS:\n\n")
dailyPlanetsAspectsReport(reportDate, symbolID) %>% print()
