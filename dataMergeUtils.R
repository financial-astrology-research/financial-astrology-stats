# Title     : Data merge utitlies.
# Objective : Ease the combination of planets positions / aspects and asset price tables.
# Created by: pablocc
# Created on: 26/01/2021

library(data.table)

source("./fileSystemUtilities.R")
source("./dataLoadUtils.R")

#' Merge daily planet longitude position and asset price augmented table.
#' @param symbolID Symbol ID of the asset data to process.
#' @return Daily planets position + asset prices data table.
planetsPositionsAssetPricesDataMerge <- function(symbolID) {
  merge(
    dailyPlanetPositionLoad(),
    assetAugmentedDataLoad(symbolID),
    by = 'Date'
  )
}

#' Merge moon phases and asset price augmented table.
#' @param symbolID Symbol ID of the asset data to process.
#' @return Daily moon phases + asset prices data table.
moonPhaseAssetPricesDataMerge <- function(symbolID) {
  merge(
    dailyMoonPhasesLoad(),
    assetAugmentedDataLoad(symbolID),
    by = 'Date'
  )
}
