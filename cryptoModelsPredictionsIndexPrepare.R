# Title     : Compute a signals index for cryptocurrencies top 5 performing ML models predictions.
# Objective : Provide an aggregated cryptocurrencies signals that allow detection of dominant price direction.
# Created by: pablocc
# Created on: 27/02/2021

library(data.table)

source('./fileSystemUtilities.R')
source('./dataLoadUtils.R')

exportCols <- c('Date', 'EffPred')

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
  indexFilePath <- paste0(modelsSignalsIndexDestinationPath(), targetFile)
}

modelPredictionsSignalsGet <- function(predictFilename) {
  sourceFilePath <- paste(modelsPredictionDestinationPath(), predictFilename, sep = '/')
  predictionTable <- fread(sourceFilePath)
  predictionTable[, ..exportCols]
}

signalsIndexCalculate <- function(dailySignals, byFormula, indexName) {
  setDT(dailySignals)
  setkey(dailySignals, 'Date')
  dailySignals[, Date := as.Date(Date)]
  dailySignals[, YearMonth := format(Date, '%Y-%m')]
  dailySignals[, YearWeek := format(Date, '%Y-%V')]

  allSymbolsIndex <- dcast(
    dailySignals,
    byFormula,
    fun.aggregate = SIT::count,
    value.var = 'EffPred',
    fill = 0
  )
  setDT(allSymbolsIndex)

  # Calculate index signal based on the majority of all symbols signals side.
  allSymbolsIndex[,
    Action := ifelse(buy > sell, 'buy', ifelse(buy == sell, 'neutral', 'sell'))
  ]

  indexPathFileName <- signalsIndexTargetPathFileNameGet(indexName)
  fwrite(allSymbolsIndex, indexPathFileName)
  cat('Signals count index exported to:', indexPathFileName, '\n')
}

symbolPredictionsIndex <- function(symbolID) {
  modelsPerformanceReport <- dataTableRead(
    modelsLatestPerformancePathFileNameGet()
  )

  topPerformers <- modelsPerformanceReport[Symbol == symbolID][order(-Rank)] %>% head(5)
  symbolPredictions <- rbindlist(lapply(topPerformers$PredictFile, modelPredictionsSignalsGet))
  signalsIndexCalculate(
    symbolPredictions,
    'Date ~ EffPred',
    paste(symbolID, 'daily', sep = '-')
  )

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
  modelsPerformanceReport <- dataTableRead(
    modelsLatestPerformancePathFileNameGet()
  )

  # Calculate a buy/sell signal count index for all machine learning assets predictions.
  symbolsIDS <- unique(modelsPerformanceReport$Symbol)
  allPredictions <- rbindlist(lapply(symbolsIDS, symbolPredictionsIndex))

  signalsIndexCalculate(
    allPredictions,
    'Date ~ EffPred',
    'all-daily'
  )

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

assetsModelsPredictionsSignalIndexPrepare()

sectorSignalsIndexPrepare <- function() {
  prepareConsensusPredictionCSV <- function(predictFilename) {
    cat('Processing: ', predictFilename, '\n')
    filenameParts <- unlist(strsplit(predictFilename, '-'))
    symbolId <- paste(filenameParts[1], filenameParts[2], 'T', sep = '')
    targetFilePath <- signalsPathFileNameGet(symbolId)
    indexFilePath <- signalsIndexTargetPathFileNameGet('daily')
    symbolIndicator <- fread(targetFilePath)
    indexIndicator <- fread(indexFilePath)
    consensusIndicator <- merge(symbolIndicator, indexIndicator[, c('Date', 'Action')], by = 'Date')
    setnames(consensusIndicator, c('Date', 'SymbolAction', 'IndexAction'))

    # Hold when symbol indicator signal differs from index indicator.
    consensusIndicator[SymbolAction != IndexAction, EffPred := 'hold']
    consensusIndicator[SymbolAction == IndexAction | SymbolAction == 'neutral', EffPred := SymbolAction]
    consensusTargetFilePath <- consensusSignalsPathFileNameGet(symbolId)
    fwrite(consensusIndicator[, ..exportCols], consensusTargetFilePath)
    cat('Consensus indicator exported to:', consensusTargetFilePath, '\n')

    return(consensusTargetFilePath)
  }

  consensusFiles <- lapply(predictionsList$filename, prepareConsensusPredictionCSV)
}

