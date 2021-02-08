# Title     : Data tables load utilities.
# Objective : Ease the load of data tables.
# Created by: pablocc
# Created on: 08/02/2021

library(data.table)
library(memoise)

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
    frequencyTable[, c(factorParts) := tstrsplit(get(factorID), "_", fixed = T)]
  }
}

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

#' Load models predictions data table from CSV.
#' @return Daily predictions data table.
modelPredictionsLoad <- function(predictionsFileName) {
  modelPredictions <- memoFileRead(paste0(modelsPredictionDestinationPath(), predictionsFileName))
  modelPredictions[, Date := as.Date(Date)]
  modelPredictions[, YearMonth := format(Date, "%Y-%m")]
}

#' Load models predictions metadata table from CSV.
#' @return Models predictions metadata data table.
modelPredictionsMetadataLoad <- function() {
  destinationPathFileName <- paste0(modelsPerformanceDestinationPath(), "models_prediction_metadata.csv")
  memoFileRead(destinationPathFileName)
}

#' Load asset augmented (price derivations features) data table from CSV.
#' @return Asset price augmented data table.
assetAgumentedDataLoad <- function(symbolID, startDate = NULL) {
  destinationPathFileName <- paste0(assetsDataDestinationPath(), symbolID, "--augmented.csv")
  assetDataTable <- memoFileRead(destinationPathFileName)
  assetDataTable[, Date := as.Date(Date)]

  if (!is.null(startDate)) {
    assetDataTable <- assetDataTable[Date >= startDate]
  }
}
