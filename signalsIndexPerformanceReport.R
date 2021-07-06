# Title     : Test monthly accuracy of signals index predictions using latest price.
# Objective : Review the performance of signals index detailed by month since August 2020.
# Created by: pablocc
# Created on: 29/05/2021

library(psych)

source("./configUtils.R")
source("./dataLoadUtils.R")
source("./fileSystemUtilities.R")

options(width=150)

assetPredictionsTest <- function(symbolID) {
  startDate <- "2020-10-01"
  calculateAccuracy <- function(monthlyData) {
    categoryLevels = c("Buy", "Sell")
    confusionData <- table(
      actualclass = factor(monthlyData$OHLCEff, levels = categoryLevels),
      predictedclass = factor(monthlyData$Action, levels = categoryLevels)
    ) %>% caret::confusionMatrix()

    accuracy <- confusionData$overall['Accuracy']
    prevalence <- confusionData$byClass['Prevalence']

    list(Accuracy = accuracy, Prevalence = prevalence, N = nrow(monthlyData))
  }

  indicatorFile <- paste("ml", symbolID, "daily-index", sep = "-")
  indicatorPathFile <- paste(modelsSignalsIndexDestinationPath(), indicatorFile, ".csv", sep = "")

  if (!file.exists(indicatorPathFile)) {
    return(NULL);
  }

  modelsPredictions <- fread(indicatorPathFile)
  modelsPredictions[, Date := as.Date(Date)]
  modelsPredictions[, YearMonth := format(Date, "%Y-%m")]
  assetDataTable <- assetAugmentedDataLoad(symbolID, startDate)
  modelsPredictionsActuals <- merge(
    assetDataTable[, c('Date', 'OHLCMid', 'OHLCEff')],
    modelsPredictions,
    by = "Date"
  )

  categoryLevelsMap <- function(EffPred) {
    categoryLevels <- c("Buy", "Sell")
    mapvalues(EffPred, tolower(categoryLevels), categoryLevels, warn_missing = F)
  }

  modelsPredictionsActuals[, Action := categoryLevelsMap(Action)]

  cat("\n", symbolID, "signals index performance test by month:", "\n")
  accuracyTest <- modelsPredictionsActuals[, calculateAccuracy(.SD), by = "YearMonth"]
  print(accuracyTest)
  describe(accuracyTest[, c('Accuracy', 'Prevalence')]) %>% print()
}

symbolsList <- assetsWatchList()
targetPathFile <- paste0(modelsSignalsIndexPerformanceDestinationPath(), 'signals_index_monthly_performance', ".txt")
cat("", targetPathFile, append = F)
cat("\n", "Generating signals index performance test by month to ", targetPathFile, "\n")
sink(targetPathFile)
testResults <- lapply(symbolsList$SymbolID, assetPredictionsTest)
testAccuracy <- rbindlist(lapply(testResults, function(testResult) testResult[c("Accuracy"),]))
testPrevalence <- rbindlist(lapply(testResults, function(testResult) testResult[c("Prevalence"),]))

# Portfolio performance.
portfolioAccuracy = mean(testAccuracy$mean)
portfolioSD = mean(testAccuracy$sd)
portfolioPrevalence = mean(testPrevalence$mean)
portfolioSD = mean(testPrevalence$sd)

cat("\n\n##########################################\n")
cat("Portfolio accuracy: ", portfolioAccuracy, "SD:", portfolioAccuracySD)
cat("        prevalence: ", portfolioPrevalence, "SD:", portfolioPrevalenceSD)
cat("\n##########################################\n")

sink()
