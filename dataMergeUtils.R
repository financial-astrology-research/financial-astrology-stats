# Title     : Data merge utitlies.
# Objective : Ease the combination of planets positions / aspects and asset price tables.
# Created by: pablocc
# Created on: 26/01/2021

library(data.table)

source("./fileSystemUtilities.R")

#' Merge daily planet longitude position and asset price augmented table.
#' @param symbolID Symbol ID of the asset data to process.
#' @return Daily planets position + asset prices data table.
planetsPositionsAssetPricesDataMerge <- function(symbolID) {
  assetPriceAugmentedFileName <- paste0(assetsDataDestinationPath(), symbolID, "--augmented.csv")
  assetPriceAugmentedTable <- fread(assetPriceAugmentedFileName)
  positionsFileName <- "daily_planets_positions_long"
  dailyPlanetsPositionsTable <- fread(paste0("./data/", positionsFileName, ".csv"))
  merge(dailyPlanetsPositionsTable, assetPriceAugmentedTable, by = c('Date'))
}

#' Merge moon phases and asset price augmented table.
#' @param symbolID Symbol ID of the asset data to process.
#' @return Daily moon phases + asset prices data table.
moonPhaseAssetPricesDataMerge <- function(symbolID) {
  assetPriceAugmentedFileName <- paste0(assetsDataDestinationPath(), symbolID, "--augmented.csv")
  assetPriceAugmentedTable <- fread(assetPriceAugmentedFileName)
  positionsFileName <- "daily_moon_phase_positions"
  dailyMoonPhaseTable <- fread(paste0("./data/", positionsFileName, ".csv"))
  merge(dailyMoonPhaseTable, assetPriceAugmentedTable, by = c('Date'))
}
