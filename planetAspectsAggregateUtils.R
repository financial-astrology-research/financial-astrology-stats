# Title     : Planet aspects aggregation utilities.
# Objective : Support different ways of aspects aggregation to research generalization of the effects.
# Created by: pablocc
# Created on: 10/02/2021

library(data.table)

source("./dataLoadUtils.R")
source("factorAggregateUtils.R")

#' Aggregate aspects as total count from planet receiver.
#' @param dailyPlanetAspects Daily planet aspects long data table.
#' @param byFactor The factor to use for count aggregate.
#' @return Receiver planet aspects count by aspect type data table.
dailyPlanetAspectsByFactorCount <- function(dailyAspectsTable, byFactor) {
  # Prevent mutation of original table.
  dailyAspectsTableCopy <- copy(dailyAspectsTable)
  dailyFactorCountAggregate(dailyAspectsTableCopy, "aspect", byFactor)
}

#' Prepare daily planet receiver aspects count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet receiver aspects count with asset price data table.
planetReceiverAspectsCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetAspectsLoad() %>%
    dailyPlanetAspectsByFactorCount("pY") %>%
    factorCountAssetPriceMerge("pID", symbolID)
}

#' Prepare daily planet receiver aspect types count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet receiver aspect types count with asset price data table.
planetReceiverAspectTypesCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetAspectsLoad() %>%
    dailyPlanetAspectsByFactorCount("pY + aspect") %>%
    factorCountAssetPriceMerge("PlanetAspect", symbolID)
}

#' Prepare daily planet emitter aspects count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet emitter aspects count with asset price data table.
planetEmitterAspectsCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetAspectsLoad() %>%
    dailyPlanetAspectsByFactorCount("pX") %>%
    factorCountAssetPriceMerge("pID", symbolID)
}

#' Prepare daily planet emitter aspect types count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet emitter aspects types count with asset price data table.
planetEmitterAspectTypesCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetAspectsLoad() %>%
    dailyPlanetAspectsByFactorCount("pX + aspect") %>%
    factorCountAssetPriceMerge("PlanetAspect", symbolID)
}

#' Prepare daily planet (emitted and received) aspects count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet aspects count with asset price data table.
planetAspectsCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetAspectsLoad() %>%
  melt(
    id.var = c('Date', 'aspect'),
    variable.name = 'origin',
    value.name = 'pID',
    measure.var = c('pX', 'pY'),
  ) %>%
    dailyPlanetAspectsByFactorCount("pID") %>%
    factorCountAssetPriceMerge("pID", symbolID)
}

#' Prepare daily planet (emitted and received) aspect types count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet aspect types count with asset price data table.
planetAspectTypesCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetAspectsLoad() %>%
    melt(
      id.var = c('Date', 'aspect'),
      variable.name = 'origin',
      value.name = 'pID',
      measure.var = c('pX', 'pY'),
    ) %>%
    dailyPlanetAspectsByFactorCount("pID + aspect") %>%
    factorCountAssetPriceMerge("PlanetAspect", symbolID)
}
