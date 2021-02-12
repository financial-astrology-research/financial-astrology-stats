# Title     : Data table factor aggregate utilities.
# Objective : Support data table factors aggregation.
# Created by: pablocc
# Created on: 12/02/2021

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
#' @param variableName The value variable name to aggregate.
#' @param byFactor The factor to use for count aggregate.
dailyFactorCountAggregate <- function(dataTable, variableName, byFactor) {
  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dataTable,
    formula(paste('Date ~', byFactor)),
    fun.aggregate = length,
    value.var = variableName,
    fill = 0
  )
  setDT(dailyAspectsCount)

  dailyAspectsCount[, Date := as.Date(Date)]
}

#' Cut in count range groups the aspect counts features.
#' @param aspectsCountTable Aspect count wide table.
#' @param countColumnNames Count features column names.
#' @return Aspects count table counts as range groups factors.
factorCountTableWideCut <- function(aspectsCountTable, countColumnNames) {
  # Group counts in quantiles.
  aspectsCountTable[,
    c(countColumnNames) := lapply(.SD, function(x) quantcut(x, q = 4)),
    .SDcols = countColumnNames
  ]
}

#' Transform factor count wide table to long table.
#' @param countTable Factor count wide table.
#' @param variableName Variable name that results from long transformation.
#' @param countColumnNames Count features column names.
#' @return Factor count long table.
factorCountTableLongTransform <- function(countTable, variableName, countColumnNames) {
  melt(
    countTable,
    id.var = 'Date',
    variable.name = variableName,
    value.name = 'CountRange',
    measure.var = countColumnNames,
  )
}

#' Merge factors count with asset price table.
#' @param aspectsCountTable Factor count wide table.
#' @param variableName Variable name that results from long transformation.
#' @param symbolID Asset symbol ID.
#' @return Factor count with asset price data table.
factorCountAssetPriceMerge <- function(aspectsCountTable, variableName, symbolID) {
  assetAugmentedData <- assetAugmentedDataLoad(symbolID)
  columnNames <- colnames(aspectsCountTable)
  countColumnNames <- columnNames[2:length(columnNames)]
  factorCountTableWideCut(aspectsCountTable, countColumnNames)

  merge(
    factorCountTableLongTransform(aspectsCountTable, variableName, countColumnNames),
    assetAugmentedData,
    by = "Date"
  )
}