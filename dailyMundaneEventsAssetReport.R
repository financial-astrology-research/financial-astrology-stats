# Title     : Daily planet events with asset frequency statistics report.
# Objective : Support the daily astrological events effects analysis based on asset historical price effect statistics.
# Created by: pablocc
# Created on: 29/01/2021

library(data.table)
library(magrittr)
library(stringr)

source("./configUtils.R")
source("./fileSystemUtilities.R")
source("./dataLoadUtils.R")

#' Select columns from frequency stats report table.
#' @param reportTable Asset frequency stats report table.
#' @param columnNames Factor parts column names to select.
#' @return Report data table selected columns data.
frequencyStatsColumnsSelect <- function(reportTable, factorParts) {
  selectCols <- c('Date', factorParts, 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  reportTable[,
    .SD,
    .SDcols = selectCols
  ]
}

#' Sorb frequency stats report table by columns.
#' @param reportTable Asset frequency stats report table.
#' @param columnNames Sort column.
#' @return Report data table sorted by column names.
frequencyStatsColumnSort <- function(reportTable, columnNames) {
  reportTable[order(get(columnNames))]
}

#' Load frequency stats table.
#' @param dailyMundaneEventsTable Daily mundane events table to extract date report from.
#' @param symbolID Symbol ID to load frequency stats for.
#' @param statsID Frequency stats identifier.
#' @param factorID Factor column ID that was used to compute the frequency stats.
#' @param factorParts Factor parts names to assign on ID destructure.
#' @param reportDate Date to generate report for.
#' @return Frequency stats data table.
assetPriceEffectFrequencyStatsReport <- function(
  dailyMundaneEventsTable, symbolID, statsID, factorID, factorParts, reportDate
) {
  frequencyTable <- assetPriceEffectFrequencyStatsLoad(
    symbolID,
    statsID,
    factorID,
    factorParts
  )

  reportDateEvents <- dataTableDateEqualFilter(
    dailyMundaneEventsTable,
    reportDate
  )

  merge(reportDateEvents, frequencyTable, by = factorParts)
}

#' Report the planets rythm positions with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet rythm positions with price effect frequencies report table.
dailyMundaneEventsRythmReport <- function(reportDate, symbolID) {
  dailyPlanetPositionLoad() %>%
    assetPriceEffectFrequencyStatsReport(
      symbolID,
      "planet_quality",
      'PlanetQuality',
      c('pID', 'QualityID'),
      reportDate
    ) %>%
    frequencyStatsColumnsSelect(c('Planet', 'Quality'))
}

#' Report the planets element positions with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet element positions with price effect frequencies report table.
dailyMundaneEventsElementReport <- function(reportDate, symbolID) {
  dailyPlanetPositionLoad() %>%
    assetPriceEffectFrequencyStatsReport(
      symbolID,
      "planet_element",
      'PlanetElement',
      c('pID', 'ElementID'),
      reportDate
    ) %>%
    frequencyStatsColumnsSelect(c('Planet', 'Element'))
}

#' Report the planets zodiac sign positions with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet zodiac sign positions with price effect frequencies report table.
dailyMundaneEventsZodSignsReport <- function(reportDate, symbolID) {
  dailyPlanetPositionLoad() %>%
    assetPriceEffectFrequencyStatsReport(
      symbolID,
      "planet_zodsign",
      'PlanetZodSign',
      c('pID', 'ZodSignN', 'ZodSignID'),
      reportDate
    ) %>%
    frequencyStatsColumnsSelect(c('Planet', 'ZodSign'))
}

#' Report the planets speed phase with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet speed phase with price effect frequencies report table.
dailyMundaneEventsSpeedPhaseReport <- function(reportDate, symbolID) {
  dailyPlanetPositionLoad() %>%
    assetPriceEffectFrequencyStatsReport(
      symbolID,
      "planet_speed",
      'PlanetSpeedPhase',
      c('pID', 'SpeedPhaseID'),
      reportDate
    ) %>%
    frequencyStatsColumnsSelect(c('Planet', 'Speed', 'SpeedPhase'))
}

#' Report the planets aspects with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet positions with price effect frequencies report table.
dailyMundaneEventsAspectsReport <- function(reportDate, symbolID) {
  dailyPlanetAspectsLoad() %>%
    assetPriceEffectFrequencyStatsReport(
      symbolID,
      "planets_aspects",
      'PlanetsAspect',
      c('pX', 'pY', 'aspect'),
      reportDate
    ) %>%
    frequencyStatsColumnsSelect(
      c('meanOrb', 'minOrb', 'maxOrb', 'startHour', 'endHour', 'exactHour', 'effHours', 'PlanetX', 'Aspect', 'PlanetY')
    ) %>%
    frequencyStatsColumnSort('exactHour')
}

#' Report the planets decans positions with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet decans with price effect frequencies report table.
dailyMundaneEventsDecansReport <- function(reportDate, symbolID) {
  dailyPlanetPositionLoad() %>%
    assetPriceEffectFrequencyStatsReport(
      symbolID,
      "planet_decan",
      'PlanetDecan',
      c('pID', 'ZodSignN', 'DecanID'),
      reportDate
    ) %>%
    frequencyStatsColumnsSelect(c('Planet', 'Decan'))
}

#' Report the planets arab mansions positions with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet arab mansions with price effect frequencies report table.
dailyMundaneEventsArabMansionReport <- function(reportDate, symbolID) {
  dailyPlanetPositionLoad() %>%
    assetPriceEffectFrequencyStatsReport(
      symbolID,
      "planet_arab_mansion",
      'PlanetArabMansion',
      c('pID', 'ArabMansionID'),
      reportDate
    ) %>%
    frequencyStatsColumnsSelect(c('Planet', 'ArabMansion'))
}

#' Report the planets vedic mansions positions with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Planet vedic mansions with price effect frequencies report table.
dailyMundaneEventsVedicMansionReport <- function(reportDate, symbolID) {
  dailyPlanetPositionLoad() %>%
    assetPriceEffectFrequencyStatsReport(
      symbolID,
      "planet_vedic_mansion",
      'PlanetVedicMansion',
      c('pID', 'VedicMansionID'),
      reportDate
    ) %>%
    frequencyStatsColumnsSelect(c('Planet', 'VedicMansion'))
}

#' Report the moon phases with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Moon phases with price effect frequencies report table.
dailyMundaneEventsMoonPhaseReport <- function(reportDate, symbolID) {
  dailyMoonPhasesLoad() %>%
    assetPriceEffectFrequencyStatsReport(
      symbolID,
      'moon_phase',
      'MoonPhaseID',
      'MoonPhaseID',
      reportDate
    ) %>%
    frequencyStatsColumnsSelect(c('MoonPhase'))
}

#' Report the moon phases in zodiac signs with historical asset price effect frequencies.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Moon phases in zodiac signs with price effect frequencies report table.
dailyMundaneEventsMoonPhaseZodSignsReport <- function(reportDate, symbolID) {
  dailyMoonPhasesLoad() %>%
    assetPriceEffectFrequencyStatsReport(
      symbolID,
      'moon_phase_zod_sign',
      'MoonPhaseZodSignID',
      c('MoonPhaseID', 'ZodSignN', 'ZodSignID'),
      reportDate
    ) %>%
    frequencyStatsColumnsSelect(c('MoonPhase', 'ZodSign'))
}

#' Report the top performers machine learning models predictions.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
#' @return Models expected percent change or probabilities and signal report table.
dailyMundaneEventsPredictionsReport <- function(reportDate, symbolID) {
  modelsPerformanceReport <- dataTableRead(
    modelsLatestPerformancePathFileNameGet()
  )

  allPredictions <- data.table()
  topPerformers <- modelsPerformanceReport[Symbol == symbolID][order(-Rank)] %>% head(5)
  for (predictionsFileName in topPerformers$PredictFile) {
    predictionsTable <- modelPredictionsLoad(predictionsFileName)
    columnNames <- colnames(predictionsTable)
    selectColumns <- columnNames[grep("EffUp|DiffPred|EffPred", columnNames)] %>% c('Date', .)
    reportDatePredictions <- predictionsTable[Date == reportDate, ..selectColumns]
    reportDatePredictions[, ModelID := str_replace(predictionsFileName, '.csv', '')]
    setcolorder(reportDatePredictions, c('ModelID', selectColumns))
    pColNames <- c('P1', 'P2', 'P3', 'P4', 'P5')
    if (length(reportDatePredictions) == 6) {
      pColNames <- c('P1', 'P2', 'P3')
    }

    setnames(reportDatePredictions, c('ModelID', 'Date', pColNames, 'Signal'))
    allPredictions <- rbind(allPredictions, reportDatePredictions)
  }

  if (nrow(allPredictions) == 0) {
    allPredictions <- data.table(cbind(Model = "Not available"))
  }

  return(allPredictions)
}

#' Generate all planets daily setup with asset price effect stats.
#' @param reportDate The date to generate the report for.
#' @param symbolID Symbol ID to report frequencies for.
dailyMundaneEventsReport <- function(reportDate, symbolID) {
  cat("\nPLANET SPEED PHASE:\n\n")
  dailyMundaneEventsSpeedPhaseReport(reportDate, symbolID) %>% print()
  cat("\nPLANET RYTHM:\n\n")
  dailyMundaneEventsRythmReport(reportDate, symbolID) %>% print()
  cat("\nPLANET ELEMENT:\n\n")
  dailyMundaneEventsElementReport(reportDate, symbolID) %>% print()
  cat("\nPLANET ZODIAC SIGN:\n\n")
  dailyMundaneEventsZodSignsReport(reportDate, symbolID) %>% print()
  cat("\nPLANET DECAN:\n\n")
  dailyMundaneEventsDecansReport(reportDate, symbolID) %>% print()
  cat("\nPLANET ARAB MANSION:\n\n")
  dailyMundaneEventsArabMansionReport(reportDate, symbolID) %>% print()
  cat("\nPLANET VEDIC MANSION:\n\n")
  dailyMundaneEventsVedicMansionReport(reportDate, symbolID) %>% print()
  cat("\nLAST MOON PHASE:\n\n")
  dailyMundaneEventsMoonPhaseReport(reportDate, symbolID) %>% print()
  cat("\nLAST MOON PHASE IN ZODIAC SIGN:\n\n")
  dailyMundaneEventsMoonPhaseZodSignsReport(reportDate, symbolID) %>% print()
  cat("\nPLANETS ASPECTS:\n\n")
  dailyMundaneEventsAspectsReport(reportDate, symbolID) %>% print()
  cat("\nMACHINE LEARNING PREDICTIONS:\n\n")
  dailyMundaneEventsPredictionsReport(reportDate, symbolID) %>% print(row.names = F)
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
  endDate <- Sys.Date() + (daysN - 1)
  reportDates <- seq(startDate, endDate, by = "1 day")
  watchList <- assetsWatchList()
  options(width = 200)

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
