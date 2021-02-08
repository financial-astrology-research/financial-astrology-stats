# Title     : Generate models predictions accuracy and stability performance report.

library(data.table)
library(magrittr)
library(plyr)
library(psych)

source("./dataLoadUtils.R")
source("./fileSystemUtilities.R")
source("./planetAspectsAssetsPriceDataPrepare.R")

#' Generate models predictions metadata file.
predictionsMetadataCreate <- function() {
  destinationPathFileName <- paste0(modelsPerformanceDestinationPath(), "models_prediction_metadata.csv")
  predictFilesMetadataPrepare <- function() {
    predictFiles <- list.files(modelsPredictionDestinationPath(), pattern = "*.csv")
    lapply(predictFiles, function(predictFile) {
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
  prevalence <- confusionData$byClass['Prevalence']

  list(
    N = nrow(monthlyData),
    Accuracy = accuracy,
    Prevalence = prevalence
  )
}

#' Extract symbol ID from predictions filename.
#' @param predictionsFileName Model predictions filename (without path).
#' @return Symbol ID.
predictionsFileNameSymbolIdExtract <- function(predictionsFileName) {
  fileNameParts <- unlist(strsplit(predictionsFileName, "-"))
  paste(fileNameParts[1], fileNameParts[2], sep = "-")
}

#' Calculate machine learning model predictions performance metrics.
#' @param predictionsFileName Model predictions filename to calculate metrics for.
#' @return Data table with model predictions performance metrics.
predictionsPerformanceMetricsCalculate <- function(predictionsFileName) {
  cat("Processing: ", predictionsFileName, "\n")
  symbolId <- predictionsFileNameSymbolIdExtract(predictionsFileName)
  startDate <- as.Date(format(Sys.Date() - 210, "%Y-%m-01"))
  assetDataTable <- fread(paste0("./data/tmp/", symbolId, "--augmented.csv"))
  assetDataTable[, Date := as.Date(Date)]
  # Filter the period of model unseen data, not used for training.
  assetDataTable <- assetDataTable[Date >= startDate]
  modelPredictions <- modelPredictionsLoad(predictionsFileName)
  modelPredictions <- merge(
    assetDataTable[, c('Date', 'OHLCMid', 'OHLCEff')],
    modelPredictions,
    by = "Date"
  )
  # Normalize factors that are case sensitive for comparison.
  categoryLevels <- c("Buy", "Sell")
  modelPredictions[,
    EffPred := mapvalues(EffPred, tolower(categoryLevels), categoryLevels)
  ]

  accuracyTest <- modelPredictions[, accuracyCalculate(.SD), by = list(YearMonth)]
  # Filter months that don't have at least N observations yet.
  accuracyTest <- accuracyTest[N >= 10]
  # Calculate descriptive statistics for Accuracy / Prevalence.
  descriptives6m <- round(describe(head(accuracyTest[, c('Accuracy', 'Prevalence')], 6)), 3)
  descriptives3m <- round(describe(tail(accuracyTest[, c('Accuracy', 'Prevalence')], 3)), 3)
  descriptives2m <- round(describe(tail(accuracyTest[, c('Accuracy', 'Prevalence')], 2)), 3)
  descriptives1m <- round(describe(tail(accuracyTest[, c('Accuracy', 'Prevalence')], 1)), 3)
  createDate <- predictFileInfo$mtime
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
    Prev6m = descriptives6m$mean[2],
    Prev3m = descriptives3m$mean[2],
    Prev2m = descriptives2m$mean[2],
    Prev1m = descriptives1m$mean[2],
    PrevSD6m = descriptives6m$sd[2],
    PrevSD3m = descriptives3m$sd[2],
    PrevSD2m = descriptives2m$sd[2]
  )

  reportData$Rank <- with(
    reportData,
    ((Acc3m / (1 + AccSD3m)^2) +
      (Acc2m / (1 + AccSD2m)^2) +
      Acc1m) / 3
  )

  return(reportData)
}

planetsAspectsAssetsPriceDataPrepare()
predictionsMetadataCreate()
predictionsFileNames <- list.files(modelsPredictionDestinationPath(), pattern = "*.csv")
testResults <- setDT(rbindlist(lapply(predictionsFileNames, predictionsPerformanceMetricsCalculate)))
testResults <- testResults[order(Symbol, -Rank)]

reportDate <- format(Sys.Date(), "%Y-%m-%d")
modelsPredictSummaryFilename <- paste0(
  modelsPerformanceDestinationPath(),
  "models-predict-performance-", reportDate, ".csv"
)

fwrite(testResults, modelsPredictSummaryFilename)
cat("Models performance report exported to:", modelsPredictSummaryFilename, "\n")