# Title     : Generate models predictions accuracy and stability performance report.

library(magrittr)
library(psych)
library(data.table)

source("./fileSystemUtilities.R")
source("./planetAspectsAssetsPriceDataPrepare.R")

testPredictAccuracy <- function(predictFilename) {
  cat("Processing: ", predictFilename, "\n")
  filenameParts <- unlist(strsplit(predictFilename, "-"))
  symbolId <- paste(filenameParts[1], filenameParts[2], sep = "-")
  startDate <- as.Date(format(Sys.Date() - 210, "%Y-%m-01"))
  securityDataTest <- fread(paste0("./data/tmp/", symbolId, "--augmented.csv"))
  securityDataTest[, Date := as.Date(Date)]
  # Filter the period of model unseen data, not used for training.
  securityDataTest <- securityDataTest[Date >= startDate]

  predictPath <- paste0(modelsPredictionDestinationPath(), predictFilename)
  predictFileInfo <- file.info(predictPath)
  dailyIndicator <- fread(predictPath)

  dailyIndicator[, Date := as.Date(Date)]
  dailyIndicator[, YearMonth := format(Date, "%Y-%m")]
  dailyIndicator <- merge(
    securityDataTest[, c('Date', 'OHLCMid', 'OHLCEff')],
    dailyIndicator,
    by = "Date"
  )

  calculateAccuracy <- function(monthlyData) {
    categoryLevels = c("buy", "sell")
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

  accuracyTest <- dailyIndicator[, calculateAccuracy(.SD), by = "YearMonth"]
  # Filter months that don't have at least N observations yet.
  accuracyTest <- accuracyTest[N >= 7]

  # Calculate descriptive statistics for Accuracy / Prevalence.
  descriptives6m <- round(describe(head(accuracyTest[, c('Accuracy', 'Prevalence')], 6)), 3)
  descriptives3m <- round(describe(tail(accuracyTest[, c('Accuracy', 'Prevalence')], 3)), 3)
  descriptives2m <- round(describe(tail(accuracyTest[, c('Accuracy', 'Prevalence')], 2)), 3)
  descriptives1m <- round(describe(tail(accuracyTest[, c('Accuracy', 'Prevalence')], 1)), 3)
  createDate <- predictFileInfo$mtime
  prodDays <- as.numeric(difftime(Sys.Date(), as.Date(createDate), units = "days"))

  reportData <- data.table(
    PredictFile = predictFilename,
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
    ((Acc3m / (1 + AccSD3m) ^ 2) + (Acc2m / (1 + AccSD2m) ^ 2) + Acc1m) / 3
  )

  return(reportData)
}

watchListPriceDataFetch()
predictFiles <- list.files(modelsPredictionDestinationPath(), pattern = "*.csv")
testResults <- setDT(rbindlist(lapply(predictFiles, testPredictAccuracy)))
testResults <- testResults[order(Symbol, -Rank)]

# Remove models predictions with low rank (poor accuracy / stability).
worstPredictFiles <- testResults[Rank <= 0.5]$PredictFile
if (length(worstPredictFiles) > 0) {
  deleteResults <- worstPredictFiles %>%
    paste0(modelsPredictionDestinationPath(), .) %>%
    file.remove()
}

reportDate <- format(Sys.Date(), "%Y-%m-%d")
modelsPredictSummaryFilename <- paste0(
  modelsPerformanceDestinationPath(),
  "models-predict-performance-", reportDate, ".csv"
)

fwrite(testResults, modelsPredictSummaryFilename)
cat("Models performance report exported to:", modelsPredictSummaryFilename, "\n")