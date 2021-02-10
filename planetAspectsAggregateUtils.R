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
  dataTableCopy <- copy(dataTable)
  dataTableCopy <- dataTableCopy[, c(byFactor) := as.character(get(byFactor))]
  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dataTableCopy,
    Date ~ get(byFactor),
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
  # Prepare categorical variables for correct labeling on table wide transformation.
  dailyAspectsTable[, aspect := as.character(paste("a", aspect, sep = ""))]
  factorDailyAggregateCount(dailyAspectsTable, byFactor)
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
#' @param countColumnNames Count features column names.
#' @return Aspect count long table.
aspectsCountTableLongTransform <- function(aspectsCountTable, countColumnNames) {
  melt(
    aspectsCountTable,
    id.var = 'Date',
    variable.name = 'pID',
    value.name = 'CountRange',
    measure.var = countColumnNames,
  )
}

#' Mere planets aspect count with asset price table.
#' @param aspectsCountTable Aspect count wide table.
#' @param symbolID Asset symbol ID.
#' @return Planet aspects count with asset price data table.
planetAspectsCountAssetPriceMerge <- function(aspectsCountTable, symbolID) {
  assetAugmentedData <- assetAgumentedDataLoad(symbolID)
  columnNames <- colnames(aspectsCountTable)
  countColumnNames <- columnNames[2:length(columnNames)]
  aspectsCountTableWideCut(aspectsCountTable, countColumnNames)

  merge(
    aspectsCountTableLongTransform(aspectsCountTable, countColumnNames),
    assetAugmentedData,
    by = "Date"
  )
}

#' Prepare daily planet receiver aspects count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet aspects count with asset price data table.
planetReceiverAspectsCountAssetPricePrepare <- function(symbolID) {
  dailyMundaneEventsAspectsLoad() %>%
    dailyPlanetAspectsByFactorCount("pY") %>%
    planetAspectsCountAssetPriceMerge(symbolID)
}

#' Prepare daily planet emitter aspects count range factors with asset augmented data.
#' @param symbolID Asset symbol ID.
#' @return Planet aspects count with asset price data table.
planetEmitterAspectsCountAssetPricePrepare <- function(symbolID) {
  dailyMundaneEventsAspectsLoad() %>%
    dailyPlanetAspectsByFactorCount("pX") %>%
    planetAspectsCountAssetPriceMerge(symbolID)
}
