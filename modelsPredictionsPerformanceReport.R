# Title     : Generate models predictions accuracy and stability performance report.

library(data.table)
library(magrittr)
library(plyr)
library(psych)

source("./dataLoadUtils.R")
source("./fileSystemUtilities.R")
source("./planetAspectsAssetsPriceDataPrepare.R")

#' Get models predictions filenames list.
#' @return Filenames (without path) list.
modelsPredictionsFileNameList <- function() {
  list.files(modelsPredictionDestinationPath(), pattern = "*.csv")
}

#' Generate models predictions metadata file.
predictionsMetadataCreate <- function() {
  destinationPathFileName <- paste0(modelsPerformanceDestinationPath(), "models_prediction_metadata.csv")
  predictFilesMetadataPrepare <- function() {
    lapply(modelsPredictionsFileNameList(), function(predictFile) {
      predictFileInfo <- file.info(paste0(modelsPredictionDestinationPath(), predictFile))
      createDate <- predictFileInfo$mtime
      list(
        ModelID = predictFile,
        CreateDate = createDate
      )
    })
  }

  if (!file.exists(destinationPathFileName)) {
    metadataTable <- rbindlist(predictFilesMetadataPrepare())
    fwrite(metadataTable, destinationPathFileName)
    cat("Models metadata file exported to: ", destinationPathFileName, "\n")
  }
}

#' Calculate monthly data (daily) predictions accuracy and prevalence.
#' @param monthlyData Model predictions data table rows for a given year/month.
#' @return List with number rows (N), Accuracy and Prevalence metrics.
accuracyCalculate <- function(monthlyData) {
  categoryLevels <- c("Buy", "Sell")
  confusionData <- table(
    actualclass = factor(monthlyData$OHLCEff, levels = categoryLevels),
    predictedclass = factor(monthlyData$EffPred, levels = categoryLevels)
  ) %>% caret::confusionMatrix()

  accuracy <- confusionData$overall['Accuracy']
  PValue <- confusionData$overall['AccuracyPValue']
  prevalence <- confusionData$byClass['Prevalence']

  list(
    N = nrow(monthlyData),
    Accuracy = accuracy,
    PValue = PValue,
    Prevalence = prevalence
  )
}

#' Extract symbol ID from predictions filename.
#' @param predictionsFileName Model predictions filename (without path).
#' @return Symbol ID.
predictionsFileNameSymbolIdExtract <- function(predictionsFileName) {
  fileNameParts <- unlist(strsplit(predictionsFileName, "-"))
  ifelse(
    fileNameParts[2] == 'predict',
    fileNameParts[1],
    paste(fileNameParts[1], fileNameParts[2], sep = "-")
  )
}

#' Get model predictions creation date from models metadata table.
#' @param predictionsFileName The model predictions filename.
#' @return Model predictions create date.
modelPredictionsCreateDateGet <- function(predictionsFileName) {
  modelPredictionsMetadata <- modelPredictionsMetadataLoad()
  metadata <- modelPredictionsMetadata[ModelID == predictionsFileName]
  createDate <- ''

  if (nrow(metadata) > 0) {
    createDate <- metadata$CreateDate
  }

  return(createDate)
}

#' Load and combine model predictions and asset price effect actuals data table.
#' @return Model predictions with price effect actuals data table.
modelPredictionsWithActualsLoad <- function(predictionsFileName, startDate) {
  symbolId <- predictionsFileNameSymbolIdExtract(predictionsFileName)
  assetDataTable <- assetAugmentedDataLoad(symbolId, startDate)
  modelPredictions <- modelPredictionsLoad(predictionsFileName)
  modelPredictions <- merge(
    assetDataTable[, c('Date', 'OHLCMid', 'OHLCEff')],
    modelPredictions,
    by = "Date"
  )
}

#' Calculate machine learning model predictions performance metrics.
#' @param predictionsFileName Model predictions filename to calculate metrics for.
#' @return Data table with model predictions performance metrics.
predictionsPerformanceMetricsCalculate <- function(predictionsFileName) {
  cat("Processing: ", predictionsFileName, "\n")
  symbolId <- predictionsFileNameSymbolIdExtract(predictionsFileName)
  createDate <- modelPredictionsCreateDateGet(predictionsFileName)
  startDate <- as.Date(format(Sys.Date() - 210, "%Y-%m-01"))
  modelPredictions <- modelPredictionsWithActualsLoad(predictionsFileName, startDate)
  #modelPredictions <- modelPredictions[Date <= as.Date("2021-03-15")]
  # Calculate accuracy by year/month days observations.
  accuracyTest <- modelPredictions[, accuracyCalculate(.SD), by = list(YearMonth)]
  # Filter months that don't have at least N observations yet.
  accuracyTest <- accuracyTest[N >= 10]
  # Calculate descriptive statistics for Accuracy / Prevalence.
  descriptives6m <- round(describe(head(accuracyTest[, c('Accuracy', 'PValue', 'Prevalence')], 6)), 3)
  descriptives3m <- round(describe(tail(accuracyTest[, c('Accuracy', 'PValue', 'Prevalence')], 3)), 3)
  descriptives2m <- round(describe(tail(accuracyTest[, c('Accuracy', 'PValue', 'Prevalence')], 2)), 3)
  descriptives1m <- round(describe(tail(accuracyTest[, c('Accuracy', 'PValue', 'Prevalence')], 1)), 3)
  prodDays <- as.numeric(difftime(Sys.Date(), as.Date(createDate), units = "days"))

  reportData <- data.table(
    PredictFile = predictionsFileName,
    Symbol = symbolId,
    Created = createDate,
    ProdDays = prodDays,
    Acc6m = descriptives6m$mean[1],
    Acc3m = descriptives3m$mean[1],
    Acc2m = descriptives2m$mean[1],
    Acc1m = descriptives1m$mean[1],
    AccSD6m = descriptives6m$sd[1],
    AccSD3m = descriptives3m$sd[1],
    AccSD2m = descriptives2m$sd[1],
    PVal6m = descriptives6m$mean[2],
    PVal3m = descriptives3m$mean[2],
    PVal2m = descriptives2m$mean[2],
    PVal1m = descriptives1m$mean[2],
    Prev6m = descriptives6m$mean[3],
    Prev3m = descriptives3m$mean[3],
    Prev2m = descriptives2m$mean[3],
    Prev1m = descriptives1m$mean[3],
    PrevSD6m = descriptives6m$sd[3],
    PrevSD3m = descriptives3m$sd[3],
    PrevSD2m = descriptives2m$sd[3]
  )

  reportData$Rank <- with(
    reportData,
    ((Acc6m  + Acc3m   + Acc2m) / 3) / (1 + AccSD6m)^3
  )

  return(reportData)
}

#' Generate machine learning models performance report and save into CSV table.
modelsPredictionsPerformanceReport <- function() {
  planetsAspectsAssetsPriceDataPrepare()
  predictionsMetadataCreate()
  testResults <- lapply(modelsPredictionsFileNameList(), predictionsPerformanceMetricsCalculate) %>%
    rbindlist() %>%
    setDT()
  testResults <- testResults[order(Symbol, -Rank)]

  reportDate <- format(Sys.Date(), "%Y-%m-%d")
  modelsPredictSummaryFilename <- paste0(
    modelsPerformanceDestinationPath(),
    "models-predict-performance-", reportDate, ".csv"
  )

  fwrite(testResults, modelsPredictSummaryFilename)
  cat("Models performance report exported to:", modelsPredictSummaryFilename, "\n")
}

modelsPredictionsPerformanceReport()