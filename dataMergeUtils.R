# Title     : Data merge utitlies.
# Objective : Ease the combination of planets positions / aspects and asset price tables.
# Created by: pablocc
# Created on: 26/01/2021

library(data.table)

source("./fileSystemUtilities.R")

#' Merge daily planet longitude position and asset prices augmented tables and export to CSV.
#' @param symbolID Symbol ID of the asset data to process.
#' @return Boolean vector with TRUE for success data augmentation and FALSE for failed.
planetsPositionsAssetPricesDataMerge <- function(symbolID) {
  assetPriceAugmentedFileName <- paste0(assetsDataDestinationPath(), symbolID, "--augmented.csv")
  assetPriceAugmentedTable <- fread(assetPriceAugmentedFileName)
  positionsFileName <- "daily_planets_positions_long"
  dailyPlanetsPositionsTable <- fread(paste0("./data/", positionsFileName, ".csv"))
  merge(dailyPlanetsPositionsTable, assetPriceAugmentedTable, by = c('Date'))
}
