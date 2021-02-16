# Title     : Data tables load utilities.
# Objective : Ease the load of data tables.
# Created by: pablocc
# Created on: 08/02/2021

library(data.table)
library(magrittr)
library(memoise)
library(plyr)

source("./fileSystemUtilities.R")

#' Filter data table rows by date.
#' @param dataTable Data table to subset from.
#' @param filterDate Date to use for filtering.
#' @return Data table subset with rows that match Date and selected columns.
dataTableDateEqualFilter <- function(dataTable, filterDate) {
  dataTable[Date == as.character(filterDate)]
}

#' Filter data table rows by date greater than.
#' @param dataTable Data table to subset from.
#' @param filterDate Date to use for filtering.
#' @return Data table subset with rows that match Date and selected columns.
dataTableDateGreaterFilter <- function(dataTable, filterDate) {
  dataTable[Date > as.character(filterDate)]
}

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

#' Load asset price effect frequency stats table for a given factor.
#' @param symbolID Symbol ID to load frequency stats for.
#' @param statsID Frequency stats identifier.
#' @param factorID Factor column ID that was used to compute the frequency stats.
#' @param factorParts Factor parts names to assign on ID destructure.
#' @return Frequency stats data table.
assetPriceEffectFrequencyStatsLoad <- function(symbolID, statsID, factorID = NULL, factorParts = NULL) {
  sourceFileName <- paste(symbolID, statsID, "buy_sell_count_freq_stats", sep = "-")
  statsPathFileName <- paste0(statsDataDestinationPath(symbolID), sourceFileName, ".csv")
  frequencyTable <- copy(memoFileRead(statsPathFileName))
  if (!is.null(factorID) & !is.null(factorParts)) {
    if (length(factorParts) > 1) {
      frequencyTable[, c(factorParts) := tstrsplit(get(factorID), "_", fixed = T)]
    }
    else {
      frequencyTable[, c(factorParts) := get(factorID)]
    }
  }
}

#' Load daily planets positions data table from CSV.
#' @return Daily planets positions data table.
dailyPlanetPositionLoad <- function() {
  destinationPathFileName <- paste0(astroDataDestinationPath(), "daily_planets_positions_long.csv")
  dailyPlanetPosition <- memoFileRead(destinationPathFileName)
  dailyPlanetPosition[, Date := as.Date(Date)]
}

#' Load daily moon phase (new/full) and zodiac sign position data table from CSV.
#' @return Daily moon phase positions data table.
dailyMoonPhasesLoad <- function() {
  destinationPathFileName <- paste0(astroDataDestinationPath(), "daily_moon_phase_positions.csv")
  dailyMoonPhases <- memoFileRead(destinationPathFileName)
  dailyMoonPhases[, Date := as.Date(Date)]
}

#' Load daily planets aspects data table from CSV.
#' @return Daily planets positions data table.
dailyPlanetAspectsLoad <- function() {
  destinationPathFileName <- paste0(astroDataDestinationPath(), "aspects_all_planets_pablo_aspects_set_long.csv")
  dailyPlanetAspects <- memoFileRead(destinationPathFileName)
  dailyPlanetAspects[, Date := as.Date(Date)]
}

#' Load models predictions data table from CSV.
#' @return Daily predictions data table.
modelPredictionsLoad <- function(predictionsFileName) {
  destinationPathFileName <- paste0(modelsPredictionDestinationPath(), predictionsFileName)
  modelPredictions <- memoFileRead(destinationPathFileName)
  modelPredictions[, Date := as.Date(Date)]
  modelPredictions[, YearMonth := format(Date, "%Y-%m")]
  modelPredictions[, EffPred := as.factor(EffPred)]

  # Normalize factors that are case sensitive for comparison.
  categoryLevelsMap <- function(EffPred) {
    categoryLevels <- c("Buy", "Sell")
    mapvalues(EffPred, tolower(categoryLevels), categoryLevels, warn_missing = F)
  }

  modelPredictions[, EffPred := categoryLevelsMap(EffPred)]
}

#' Load models predictions metadata table from CSV.
#' @return Models predictions metadata data table.
modelPredictionsMetadataLoad <- function() {
  destinationPathFileName <- paste0(modelsPerformanceDestinationPath(), "models_prediction_metadata.csv")
  memoFileRead(destinationPathFileName)
}

#' Load asset augmented (price derivations features) data table from CSV.
#' @return Asset price augmented data table.
assetAugmentedDataLoad <- function(symbolID, startDate = NULL) {
  destinationPathFileName <- paste0(assetsDataDestinationPath(), symbolID, "--augmented.csv")
  assetDataTable <- memoFileRead(destinationPathFileName)
  assetDataTable[, Date := as.Date(Date)]

  if (is.null(startDate)) {
    return(assetDataTable)
  }

  assetDataTable[Date >= startDate]
}
