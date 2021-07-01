# Title     : Compute a signals index for cryptocurrencies top 5 performing ML models predictions.
# Objective : Provide an aggregated cryptocurrencies signals that allow detection of dominant price direction.
# Created by: pablocc
# Created on: 27/02/2021

library(data.table)
library(ggplot2)
library(plyr)
library(stringr)

source('./dataLoadUtils.R')
source('./fileSystemUtilities.R')
source('plotUtils.R')

exportCols <- c('Date', 'EffPred')

# Min/Max normalization.
minMaxNormalize <- function(x) {
  return(round((x - min(x)) / (max(x) - min(x)), 4))
}

signalsPathFileNameGet <- function(symbolId) {
  targetFilename <- paste('ml', symbolId, 'daily.csv', sep = '-')
  paste0(modelsSignalsIndexDestinationPath(), targetFilename)
}

consensusSignalsPathFileNameGet <- function(symbolId) {
  targetFilename <- paste('ml', symbolId, 'daily-consensus.csv', sep = '-')
  paste0(modelsSignalsIndexDestinationPath(), targetFilename)
}

signalsIndexTargetPathFileNameGet <- function(indexName) {
  targetFile <- paste('ml', indexName, 'index.csv', sep = '-')
  indexPathFile <- paste0(modelsSignalsIndexDestinationPath(), targetFile)
}

modelPredictionsSignalsGet <- function(predictFilename) {
  sourceFilePath <- paste(modelsPredictionDestinationPath(), predictFilename, sep = '/')
  predictionTable <- fread(sourceFilePath)
  predictionTable[, ..exportCols]
}

signalsIndexPlot <- function(signalsIndex, indexName) {
  indexPathFileName <- signalsIndexTargetPathFileNameGet(indexName)
  plotPathFileName <- str_replace(indexPathFileName, '.csv', '.png')
  signalsIndex[, BuyForce := cumsum(buy - sell)]
  indexPlot <- ggplot(data = signalsIndex[Date >= Sys.Date() - 120,]) +
    geom_line(aes(x = Date, y = BuyForce), colour = 'white') +
    scale_x_date(date_breaks = '7 days', date_labels = '%Y-%m-%d') +
    labs(
      title = paste(indexName, 'ML models buy/sell signals cumulative sum'),
      x = 'Buy/Sell signals cumulative count'
    ) +
    ggplotDarkTheme() +
    theme(axis.text.x = element_text(angle = 90, size = 14))

  ggsave(
    filename = plotPathFileName,
    plot = indexPlot,
    width = 50,
    height = 15,
    units = 'cm',
    scale = 1.5,
    dpi = 72
  )

  cat('Plot saved to:', plotPathFileName, '\n')
}

signalsIndexCalculate <- function(dailySignals, byFormula, indexName) {
  setDT(dailySignals)
  setkey(dailySignals, 'Date')
  dailySignals[, Date := as.Date(Date)]
  dailySignals[, YearMonth := format(Date, '%Y-%m')]
  dailySignals[, YearWeek := format(Date, '%Y-%V')]

  signalsIndex <- dcast(
    dailySignals,
    byFormula,
    fun.aggregate = SIT::count,
    value.var = 'EffPred',
    fill = 0
  )
  setDT(signalsIndex)

  # Calculate index signal based on the majority of all symbols signals side.
  signalsIndex[,
    Action := ifelse(buy >= sell, 'buy', 'sell')
  ]

  signalsIndex[,
    ActionID := mapvalues(Action, c('buy', 'sell'), c(1, 0), warn_missing = F)
  ]

  indexPathFileName <- signalsIndexTargetPathFileNameGet(indexName)
  fwrite(signalsIndex, indexPathFileName)
  cat('Signals count index exported to:', indexPathFileName, '\n')

  return(signalsIndex)
}

vectorChunkSplit <- function(x, nChunks) {
  split(x, cut(seq_along(x), nChunks, labels = FALSE))
}

symbolSignalsFlattenExport <- function(fileHandler, signalsIndex, symbolID) {
  startDate <- as.Date('2021-01-01')
  signalActions <- signalsIndex[Date >= startDate]$ActionID
  itemsPerChunk <- 200
  nChunks <- ceiling(length(signalActions) / itemsPerChunk)
  signalChunks <- vectorChunkSplit(signalActions, nChunks)

  baseSymbol <- str_replace(symbolID, '-USD', '')
  variableDefinition <- paste0('varip int[] ', baseSymbol, ' = array.from(')
  write(variableDefinition, fileHandler, append = T)
  for (index in names(signalChunks)) {
    chunkContent <- paste0('\t ', str_flatten(signalChunks[[index]], collapse = ','), ',')
    write(chunkContent, fileHandler, append = T)
  }

  write('\t )', fileHandler, append = T)
  cat(baseSymbol, 'signals exported', '\n')
}

symbolPredictionsIndex <- function(symbolID) {
  topModelsN <- 7
  modelsPerformanceReport <- dataTableRead(
    modelsLatestPerformancePathFileNameGet()
  )

  topPerformers <- modelsPerformanceReport[Symbol == symbolID][order(-Rank)] %>% head(topModelsN)
  symbolPredictions <- rbindlist(lapply(topPerformers$PredictFile, modelPredictionsSignalsGet))
  dailyIndexName <- paste(symbolID, 'daily', sep = '-')
  signalsIndex <- signalsIndexCalculate(
    symbolPredictions,
    'Date ~ EffPred',
    dailyIndexName
  )
  signalsIndexPlot(signalsIndex, dailyIndexName)
  symbolSignalsFlattenExport(fileHandler, signalsIndex, symbolID)

  signalsIndexCalculate(
    symbolPredictions,
    'YearWeek ~ EffPred',
    paste(symbolID, 'weekly', sep = '-')
  )

  signalsIndexCalculate(
    symbolPredictions,
    'YearMonth ~ EffPred',
    paste(symbolID, 'monthly', sep = '-')
  )

  return(symbolPredictions)
}

assetsModelsPredictionsSignalIndexPrepare <- function() {
  reportPathFile <- modelsLatestPerformancePathFileNameGet()
  cat("Using report: ", reportPathFile, "\n")
  modelsPerformanceReport <- dataTableRead(reportPathFile)

  # Calculate a buy/sell signal count index for all machine learning assets predictions.
  symbolsIDS <- unique(modelsPerformanceReport$Symbol)
  allPredictions <- rbindlist(lapply(symbolsIDS, symbolPredictionsIndex))
  dailyIndexName <- 'all-daily'
  signalsIndexCalculate(
    allPredictions,
    'Date ~ EffPred',
    dailyIndexName
  ) %>% signalsIndexPlot(dailyIndexName)

  signalsIndexCalculate(
    allPredictions,
    'YearWeek ~ EffPred',
    'all-weekly'
  )

  signalsIndexCalculate(
    allPredictions,
    'YearMonth ~ EffPred',
    'all-monthly'
  )
}

openPineScriptSignalsFile <- function() {
  indexPathFile <- paste0(modelsSignalsIndexDestinationPath(), 'pine-script-signals.txt')
  # Ensure the file is empty.
  cat("", indexPathFile, append = F)
  file(indexPathFile, "w")
}

fileHandler <- openPineScriptSignalsFile()
assetsModelsPredictionsSignalIndexPrepare()
close(fileHandler)