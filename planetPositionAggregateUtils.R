# Title     : Planet position aggregation utilities.
# Objective : Support aggregation of planet position essential energy to research generalization of effects.
# Created by: pablocc
# Created on: 12/02/2021

source("./factorAggregateUtils.R")

#' Aggregate daily planet positions factor as total count.
#' @param dailyPlanetPositionTable Daily planet positions long data table.
#' @param byFactor The factor to use for count aggregate.
#' @return Planet positions count by factor data table.
dailyPlanetElementsByFactorCount <- function(dailyPlanetPositionTable, byFactor) {
  # Prevent mutation of original table.
  dailyPlanetPositionTableCopy <- copy(dailyPlanetPositionTable)
  dailyFactorCountAggregate(dailyPlanetPositionTableCopy, "ElementID", byFactor)
}

#' Prepare daily planet in elements count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet in elements count factors with asset price data table.
planetElementsCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetPositionLoad() %>%
    dailyPlanetElementsByFactorCount("ElementID") %>%
    factorCountAssetPriceMerge("PlanetElement", symbolID)
}
