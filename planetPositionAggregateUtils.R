# Title     : Planet position aggregation utilities.
# Objective : Support aggregation of planet position essential energy to research generalization of effects.
# Created by: pablocc
# Created on: 12/02/2021

source("./factorAggregateUtils.R")

#' Aggregate daily planet positions variable count by factor.
#' @param dailyPlanetPositionTable Daily planet positions long data table.
#' @param byFactor The factor to use for count aggregate.
#' @return Planet positions variable count by factor data table.
dailyPlanetVariableByFactorCount <- function(dailyPlanetPositionTable, byFactor) {
  # Prevent mutation of original table.
  dailyPlanetPositionTableCopy <- copy(dailyPlanetPositionTable)
  dailyFactorCountAggregate(dailyPlanetPositionTableCopy, byFactor, byFactor)
}

#' Prepare daily planet in elements count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet in elements count factors with asset price data table.
planetElementsCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetPositionLoad() %>%
    dailyPlanetVariableByFactorCount("ElementID") %>%
    factorCountAssetPriceMerge("PlanetElement", symbolID)
}

#' Prepare daily planet in qualities count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet in qualities count factors with asset price data table.
planetQualitiesCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetPositionLoad() %>%
    dailyPlanetVariableByFactorCount("QualityID") %>%
    factorCountAssetPriceMerge("PlanetQuality", symbolID)
}
