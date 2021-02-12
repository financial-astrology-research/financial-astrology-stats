# Title     : Planet aspects aggregation utilities.
# Objective : Support different ways of aspects aggregation to research generalization of the effects.
# Created by: pablocc
# Created on: 10/02/2021

library(data.table)
library(gtools)

source("./dataLoadUtils.R")

#' Transform a categorical count table into binary (0,1) feature activation table, >=1 is 1 <=0 is 0.
#' @param countTable A data table where features count events occurrences.
#' @return An events count table transformed to binary representation.
countTableBinaryTransform <- function(countTable) {
  featureColumnNames <- names(countTable)[-1]
  countTable[,
    c(featureColumnNames) := lapply(.SD, function(x) ifelse(x > 1, 1, 0)),
    .SDcols = featureColumnNames
  ]
}

#' Aggregate data table factor by events count.
#' @param dataTable A data table.
#' @param byFactor The factor to use for count aggregate.
factorDailyAggregateCount <- function(dataTable, byFactor) {
  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dataTable,
    formula(paste('Date ~', byFactor)),
    fun.aggregate = length,
    value.var = "aspect",
    fill = 0
  )
  setDT(dailyAspectsCount)

  dailyAspectsCount[, Date := as.Date(Date)]
}

#' Aggregate aspects as total count from planet receiver.
#' @param dailyPlanetAspects Daily planet aspects long data table.
#' @param byFactor The factor to use for count aggregate.
#' @return Receiver planet aspects count by aspect type data table.
dailyPlanetAspectsByFactorCount <- function(dailyAspectsTable, byFactor) {
  # Prevent mutation of original table.
  dailyAspectsTableCopy <- copy(dailyAspectsTable)
  factorDailyAggregateCount(dailyAspectsTableCopy, byFactor)
}

#' Cut in count range groups the aspect counts features.
#' @param aspectsCountTable Aspect count wide table.
#' @param countColumnNames Count features column names.
#' @return Aspects count table counts as range groups factors.
aspectsCountTableWideCut <- function(aspectsCountTable, countColumnNames) {
  # Group counts in quantiles.
  aspectsCountTable[,
    c(countColumnNames) := lapply(.SD, function(x) quantcut(x, q = 4)),
    .SDcols = countColumnNames
  ]
}

#' Transform aspect count wide table to long table.
#' @param aspectsCountTable Aspect count wide table.
#' @param variableName Variable name that results from long transformation.
#' @param countColumnNames Count features column names.
#' @return Aspect count long table.
aspectsCountTableLongTransform <- function(aspectsCountTable, variableName, countColumnNames) {
  melt(
    aspectsCountTable,
    id.var = 'Date',
    variable.name = variableName,
    value.name = 'CountRange',
    measure.var = countColumnNames,
  )
}

#' Mere planets aspect count with asset price table.
#' @param aspectsCountTable Aspect count wide table.
#' @param variableName Variable name that results from long transformation.
#' @param symbolID Asset symbol ID.
#' @return Planet aspects count with asset price data table.
planetAspectsCountAssetPriceMerge <- function(aspectsCountTable, variableName, symbolID) {
  assetAugmentedData <- assetAugmentedDataLoad(symbolID)
  columnNames <- colnames(aspectsCountTable)
  countColumnNames <- columnNames[2:length(columnNames)]
  aspectsCountTableWideCut(aspectsCountTable, countColumnNames)

  merge(
    aspectsCountTableLongTransform(aspectsCountTable, variableName, countColumnNames),
    assetAugmentedData,
    by = "Date"
  )
}

#' Prepare daily planet receiver aspects count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet receiver aspects count with asset price data table.
planetReceiverAspectsCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetAspectsLoad() %>%
    dailyPlanetAspectsByFactorCount("pY") %>%
    planetAspectsCountAssetPriceMerge("pID", symbolID)
}

#' Prepare daily planet receiver aspect types count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet receiver aspect types count with asset price data table.
planetReceiverAspectTypesCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetAspectsLoad() %>%
    dailyPlanetAspectsByFactorCount("pY + aspect") %>%
    planetAspectsCountAssetPriceMerge("PlanetAspect", symbolID)
}

#' Prepare daily planet emitter aspects count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet emitter aspects count with asset price data table.
planetEmitterAspectsCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetAspectsLoad() %>%
    dailyPlanetAspectsByFactorCount("pX") %>%
    planetAspectsCountAssetPriceMerge("pID", symbolID)
}

#' Prepare daily planet emitter aspect types count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet emitter aspects types count with asset price data table.
planetEmitterAspectTypesCountAssetPricePrepare <- function(symbolID) {
  dailyPlanetAspectsLoad() %>%
    dailyPlanetAspectsByFactorCount("pX + aspect") %>%
    planetAspectsCountAssetPriceMerge("PlanetAspect", symbolID)
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
    planetAspectsCountAssetPriceMerge("pID", symbolID)
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
    planetAspectsCountAssetPriceMerge("PlanetAspect", symbolID)
}
