# Title     : Planet aspects aggregation utilities.
# Objective : Support different ways of aspects aggregation to research generalization of the effects.
# Created by: pablocc
# Created on: 10/02/2021

library(data.table)

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

#' Aggregate aspects as total count of planet receiver.
#' @param dailyPlanetAspects Daily planet aspects long data table.
#' @return Receiver planet aspects count by aspect type data table.
dailyPlanetReceiverAspectCount <- function(dailyAspectsTable) {
  # Prepare categorical variables for correct labeling on table wide transformation.
  dailyAspectsTable <- dailyAspectsTable[, aspect := as.character(paste("a", aspect, sep = ""))]
  dailyAspectsTable <- dailyAspectsTable[, pY := as.character(pY)]
  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dailyAspectsTable,
    Date ~ pY,
    fun.aggregate = length,
    value.var = "aspect",
    fill = 0
  )
  setDT(dailyAspectsCount)
  dailyAspectsCount[, Date := as.Date(Date)]

  return(dailyAspectsCount)
}

#' Prepare daily planet receiver aspects count range factors with asset augmented data.
planetReceiverAspectsCountAssetPricePrepare <- function(symbolID) {
  dailyAspectsTable <- dailyMundaneEventsAspectsLoad()
  dailyPlanetReceiverAspectsCount <- dailyPlanetReceiverAspectCount(dailyAspectsTable)
  assetAugmentedData <- assetAgumentedDataLoad(symbolID)
  columnNames <- colnames(dailyPlanetReceiverAspectsCount)
  selectColumns <- columnNames[2:length(columnNames)]

  # Group counts in cuts of 3 aspects.
  cutLabels <- c('00-00', '01-03', '04-07', '08-20')
  countCuts <- c(-1, 0, 3, 7, 20)
  dailyPlanetReceiverAspectsCount[,
    c(selectColumns) := lapply(.SD, function(x) cut(x, countCuts, cutLabels, include.lowest = T)),
    .SDcols = selectColumns
  ]

  dailyPlanetReceiverAspectsCountLong <- melt(
    dailyPlanetReceiverAspectsCount,
    id.var = 'Date',
    variable.name = 'pID',
    value.name = 'CountRange',
    measure.var = selectColumns
  )

  merge(
    dailyPlanetReceiverAspectsCountLong,
    assetAugmentedData,
    by = "Date"
  )
}
