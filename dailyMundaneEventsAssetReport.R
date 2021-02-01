# Title     : Daily planet events with asset frequency statistics report.
# Objective : Support the daily astrological events effects analysis based on asset historical price effect statistics.
# Created by: pablocc
# Created on: 29/01/2021

library(data.table)
library(magrittr)
library(memoise)

source("./configUtils.R")
source("./fileSystemUtilities.R")

#' Load CSV data table.
#' @param pathFileName CSV file name including absolute or relative path.
#' @return A data table.
dataTableRead <- function(pathFileName) {
  dataTable <- fread(pathFileName)
  columnNames <- colnames(dataTable)
  # Set date as primary key when column exists.
  if ("Date" %in% columnNames) {
    setkey(dataTable, Date)
  }

  return(dataTable)
}

#' Memoized file read with memory cache (persist during current session).
memoFileRead <- memoise(dataTableRead)

#' Load daily planets positions data table from CSV.
#' @return Daily planets positions data table.
dailyMundaneEventsPositionLoad <- function() {
  planetPositionsPathFileName <- paste0(astroDataDestinationPath(), "daily_planets_positions_long.csv")
  memoFileRead(planetPositionsPathFileName)
}

#' Load daily planets aspects data table from CSV.
#' @return Daily planets positions data table.
dailyMundaneEventsAspectsLoad <- function() {
  planetsAspectsPathFileName <- paste0(astroDataDestinationPath(), "aspects_all_planets_pablo_aspects_set_long.csv")
  memoFileRead(planetsAspectsPathFileName)
}

#' Filter data table rows by date value and return select columns.
#' @param dataTable Data table to subset from.
#' @param filterDate Date to use for filtering.
#' @param selectColNames Columns names to return.
#' @return Data table subset with rows that match Date and selected columns.
dataTableDateColsFilter <- function(dataTable, filterDate, selectColNames = NULL) {
  if (is.null(selectColNames)) {
    selectColNames <- colnames(dataTable)
  }

  # Convert date to string for faster lookup through index.
  dataTable[Date == as.character(filterDate), ..selectColNames]
}

#' Report the planets zodiacal positions with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet positions with price effect frequencies report table.
dailyMundaneEventsSignsReport <- function(reportDate, symbolID) {
  sourceFileName <- paste(symbolID, "planet_zodsign", "buy_sell_count_freq_stats", sep = "-")
  statsPathFileName <- paste0(statsDataDestinationPath(symbolID), sourceFileName, ".csv")
  frequencyTable <- copy(memoFileRead(statsPathFileName))
  frequencyTable[, pID := substr(PlanetZodSign, 1, 2)]
  frequencyTable[, ZodSignID := substr(PlanetZodSign, 4, 6)]
  frequencyTable[, PlanetZodSign := NULL]
  dailyMundaneEventsPosition <- dailyMundaneEventsPositionLoad()
  reportPlanetsPosition <- dataTableDateColsFilter(
    dailyMundaneEventsPosition,
    reportDate,
    c('Date', 'pID', 'ZodSign')
  )
  dailyReportTable <- merge(reportPlanetsPosition, frequencyTable, by = c('pID', 'ZodSignID'))
}

#' Report the planets speed phase with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet speed phase with price effect frequencies report table.
dailyMundaneEventsSpeedPhaseReport <- function(reportDate, symbolID) {
  sourceFileName <- paste(symbolID, "planet_speed", "buy_sell_count_freq_stats", sep = "-")
  statsPathFileName <- paste0(statsDataDestinationPath(symbolID), sourceFileName, ".csv")
  frequencyTable <- copy(memoFileRead(statsPathFileName))
  frequencyTable[, pID := substr(PlanetSpeedPhase, 1, 2)]
  frequencyTable[, SpeedModeID := substr(PlanetSpeedPhase, 4, 6)]
  dailyMundaneEventsPosition <- dailyMundaneEventsPositionLoad()
  reportPlanetsPosition <- dataTableDateColsFilter(
    dailyMundaneEventsPosition,
    reportDate,
    c('Date', 'pID', 'Speed', 'SpeedMode')
  )
  dailyReportTable <- merge(reportPlanetsPosition, frequencyTable, by = c('pID', 'SpeedModeID'))
  dailyReportTable[, c('pID', 'SpeedMode', 'PlanetSpeedPhase') := NULL]
}

#' Report the planets aspects with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet positions with price effect frequencies report table.
dailyMundaneEventsAspectsReport <- function(reportDate, symbolID) {
  sourceFileName <- paste(symbolID, "planets_aspects", "buy_sell_count_freq_stats", sep = "-")
  statsPathFileName <- paste0(statsDataDestinationPath(symbolID), sourceFileName, ".csv")
  frequencyTable <- copy(memoFileRead(statsPathFileName))
  frequencyTable[, pX := substr(PlanetsAspect, 1, 2)]
  frequencyTable[, pY := substr(PlanetsAspect, 3, 4)]
  frequencyTable[, aspect := substr(PlanetsAspect, 6, 10)]
  frequencyTable[, PlanetsAspect := NULL]
  dailyMundaneEventsAspects <- dailyMundaneEventsAspectsLoad()
  reportPlanetsAspects <- dataTableDateColsFilter(dailyMundaneEventsAspects, reportDate)
  # Filter only the exact orb aspects.
  reportPlanetsAspects <- reportPlanetsAspects[minOrb <= 1]
  dailyReportTable <- merge(
    reportPlanetsAspects,
    frequencyTable,
    by = c('pX', 'pY', 'aspect')
  )
  dailyReportTable[, c('pX', 'pY', 'aspect', 'origin') := NULL]
  dailyReportTable[order(Date, minOrb)]
}

#' Generate all planets daily setup with asset price effect stats.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
dailyMundaneEventsReport <- function(reportDate, symbolID) {
  cat("\nDAILY PLANET SPEED PHASE:\n\n")
  dailyMundaneEventsSpeedPhaseReport(reportDate, symbolID) %>% print()
  cat("\nDAILY PLANET ZODIAC SIGN POSITION:\n\n")
  dailyMundaneEventsSignsReport(reportDate, symbolID) %>% print()
  cat("\nDAILY PLANETS ASPECTS:\n\n")
  dailyMundaneEventsAspectsReport(reportDate, symbolID) %>% print()
}

#' Interactive input to specify symbol ID and date used for daily planets report.
interactiveDailyMundaneEventsReport <- function() {
  symbolID <- readline("Enter an asset symbol, default to BTCUSD when empty: ")
  if (symbolID == "") {
    symbolID <- "BTC-USD"
  }

  reportDate <- readline("Enter a date in YYYY-DD-MM format, default to now date when empty: ")
  reportDate <- try(as.Date(reportDate, format = "%Y-%m-%d"))
  if ("try-error" %in% class(reportDate) || is.na(reportDate)) {
    reportDate <- Sys.Date()
  }

  dailyMundaneEventsReport(reportDate, symbolID)
}

#' Generate N future daily planets report for all watchlist assets.
nDailyMundaneEventsReport <- function(daysN = 7) {
  startDate <- Sys.Date()
  endDate <- Sys.Date() + (daysN-1)
  reportDates <- seq(startDate, endDate, by = "1 day")
  watchList <- assetsWatchList()
  options(width=200)

  for (symbolID in watchList$SymbolID) {
    for (idx in seq_along(reportDates)) {
      reportDate <- reportDates[idx]
      cat("Generating", symbolID, "mundane events report for date ", as.character(reportDate), "\n")
      targetPathFile <- paste0(mundaneEventsDestinationPath(symbolID, reportDate), reportDate, ".txt")
      sink(targetPathFile)
      dailyMundaneEventsReport(reportDates[idx], symbolID)
      sink()
    }
  }
}
