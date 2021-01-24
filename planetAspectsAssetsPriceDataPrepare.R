# Title     : Compose daily planetary aspects / assets price data table.
# Objective : Prepare daily assets prices with planetary aspects data table for statistical research.
# Created by: pablocc
# Created on: 10/01/2021

library(data.table)
library(quantmod)

source("./fileSystemUtilities.R")

#' Provides assets data destination path.
assetsDataDestinationPath <- function() {
  destinationPath <- paste0(normalizePath('./data/tmp'), "/")
}

#' Load watchlist CSV config file with assets symbols to watch.
assetsWatchList <- function() {
  listFilePath <- normalizePath('./config/watchlist.csv')
  symbolsList <- fread(listFilePath, header = F)
  setnames(symbolsList, c('SymbolID'))
}

#' Fetch asset price data from a given data provider source.
#' @param symbolID Data source asset symbol ID.
#' @param source Data provider source ID supported by quantmod.
#' @return Boolean vector with TRUE for success symbol data download and FALSE for failed.
assetPriceDataFetch <- function(symbolID, source = "yahoo") {
  # Date to retrieve asset price since if available, oldest data source records are around this year.
  startDate = as.Date("1970-01-01")
  maxRetry <- 5

  for (t in 1:maxRetry) {
    tryCatch({
      cat("Downloading", symbolID, "- attempt: #", t, "of", maxRetry, "\n")
      symbolDataFrame <- getSymbols(
        symbolID,
        src = "yahoo",
        from = startDate,
        env = NULL,
        return.class = 'data.frame'
      )

      symbolDataFrame <- cbind(rownames(symbolDataFrame), symbolDataFrame)
      # Adjust the prices for splits / dividends
      symbolDataFrame[, 2:7] <- adjustOHLC(symbolDataFrame[, 2:7], use.Adjusted = T)
      names(symbolDataFrame) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adj Close')
      setDT(symbolDataFrame)

      # Store to tmp data directory.
      targetFileName <- paste0(assetsDataDestinationPath(), symbolID, ".csv")
      write.csv(symbolDataFrame, file = targetFileName, row.names = F)
      cat("Exported asset price data to:", targetFileName, "\n")
      return(TRUE)
    }, error = function(e) {
      print(e)
      return(FALSE)
    })
  }
}

#' Process watch list assets price data retrieval.
watchListPriceDataFetch <- function() {
  symbolsList <- assetsWatchList()
  result <- lapply(symbolsList$SymbolID, assetPriceDataFetch)
}

#' Process asset price data load and price derivations calculation.
#' @param symbolID Asset symbol ID as identified in data source.
#' @return Boolean vector with TRUE for success data augmentation and FALSE for failed.
assetPriceDataPriceAugment <- function(symbolID) {
  dateFormat = "%Y-%m-%d"
  mapricefs <- 2
  mapricesl <- 4
  assetPriceTable <- fread(
    paste0(assetsDataDestinationPath(), symbolID, ".csv")
  )

  # Filter observations with missing open price.
  assetPriceTable <- assetPriceTable[!is.na(Open)]
  assetPriceTable[, Date := as.Date(Date, format = dateFormat)]
  setkey(assetPriceTable, 'Date')

  # Calculate daily mid prices based on OHLC combinations.
  assetPriceTable[, OHLCMid := (Open + High + Low + Close) / 4]
  assetPriceTable[, HLCMid := (High + Low + Close) / 3]
  assetPriceTable[, HLMid := (High + Low) / 2]

  # Calculate fast / slow MAs for the different mid prices.
  assetPriceTable[, OHLCMAF := SMA(OHLCMid, n = mapricefs)]
  assetPriceTable[, OHLCMAS := SMA(OHLCMid, n = mapricesl)]
  assetPriceTable[, HLCMAF := SMA(HLCMid, n = mapricefs)]
  assetPriceTable[, HLCMAS := SMA(HLCMid, n = mapricesl)]
  assetPriceTable[, HLMAF := SMA(HLMid, n = mapricefs)]
  assetPriceTable[, HLMAS := SMA(HLMid, n = mapricesl)]

  # Calculate moving average momentum.
  assetPriceTable[, OHLCMom := OHLCMAF - OHLCMAS]
  assetPriceTable[, HLCMom := HLCMAF - HLCMAS]
  assetPriceTable[, HLMom := HLMAF - HLMAS]

  # Calculate mid price differences.
  assetPriceTable[, diffOHLC := Delt(OHLCMid, k = 1)]
  assetPriceTable[, difflogOHLC := Delt(OHLCMid, k = 1, type = "log")]
  assetPriceTable[, diffOxHL := Delt(Open, HLMid, k = 0)]
  assetPriceTable[, difflogOxHL := Delt(Open, HLMid, k = 0, type = "log")]
  assetPriceTable[, diffOxHLC := Delt(Open, HLCMid, k = 0)]
  assetPriceTable[, difflogOxHLC := Delt(Open, HLCMid, k = 0, type = "log")]
  assetPriceTable[, difflogHLMASxF := Delt(HLMAS, HLMAF, k = 0, type = "log")]
  assetPriceTable[, difflogHLCMASxF := Delt(HLCMAS, HLCMAF, k = 0, type = "log")]

  # Normalize price daily change to Z scores and center data.
  assetPriceTable[, zdiffOHLC := scale(diffOHLC, center = T)]
  assetPriceTable[, zdifflogOHLC := scale(difflogOHLC, center = T)]
  assetPriceTable[, zdifflogHLMASxF := scale(difflogHLMASxF, center = T)]
  assetPriceTable[, zdifflogHLCMASxF := scale(difflogHLCMASxF, center = T)]
  assetPriceTable[, zdiffOxHL := scale(diffOxHL, center = T)]
  assetPriceTable[, zdifflogOxHL := scale(difflogOxHL, center = T)]
  assetPriceTable[, zdiffOxHLC := scale(diffOxHLC, center = T)]
  assetPriceTable[, zdifflogOxHLC := scale(difflogOxHLC, center = T)]

  # Filter the initial days needed by MAs max period where MA value cannot be determined.
  assetPriceTable <- assetPriceTable[!is.na(OHLCMAF) & !is.na(difflogHLMASxF)]
  labelCuts <- c(-1000000, 0, 1000000)
  labels <- c('sell', 'buy')
  assetPriceTable[, OHLCEff := cut(diffOHLC, labelCuts, labels = labels, right = FALSE)]
  assetPriceTable[, OHLCMomEff := cut(OHLCMom, labelCuts, labels = labels, right = FALSE)]
  assetPriceTable[, HLCMomEff := cut(HLCMom, labelCuts, labels = labels, right = FALSE)]
  assetPriceTable[, HLMomEff := cut(HLMom, labelCuts, labels = labels, right = FALSE)]
  assetPriceTable[, OxHLEff := cut(diffOxHL, labelCuts, labels = labels, right = FALSE)]
  assetPriceTable[, OxHLCEff := cut(diffOxHLC, labelCuts, labels = labels, right = FALSE)]

  # Store augmented asset price table into temporal data directory.
  targetFileName <- paste0(assetsDataDestinationPath(), symbolID, "--augmented.csv")
  fwrite(assetPriceTable, targetFileName)
  cat("Exported augmented asset price data to:", targetFileName, "\n")

  # TODO: Implement error handling on write and return false on failure.
  return(TRUE)
}

#' Merge daily planet aspects and asset prices augmented tables and export to CSV.
#' @param symbolID Symbol ID of the asset data to process.
#' @return Boolean vector with TRUE for success data augmentation and FALSE for failed.
planetAspectsAssetPricesDataMerge <- function(symbolID) {
  # TODO: Separate merge from export logic.
  assetPriceAugmentedFileName <- paste0(assetsDataDestinationPath(), symbolID, "--augmented.csv")
  assetPriceAugmentedTable <- fread(assetPriceAugmentedFileName)
  aspectFileName <- "aspects_modern_planets_pablo_aspects_set_long"
  planetAspectLong <- fread(paste0("./data/", aspectFileName, ".csv"))

  planetAspectsAssetPricesTable <- merge(planetAspectLong, assetPriceAugmentedTable, by = c('Date'))
  targetFileName <- paste0(assetsDataDestinationPath(), symbolID, "--", aspectFileName, ".csv")
  fwrite(planetAspectsAssetPricesTable, targetFileName)
  cat("Exported aspects / asset price merged data to:", targetFileName, "\n")

  # TODO: Implement error handling on write and return false on failure.
  return(TRUE)
}

#' Process planet aspects / assets price data preparation.
planetsAspectsAssetsPriceDataPrepare <- function() {
  watchListPriceDataFetch()
  symbolsList <- assetsWatchList()
  augmentResult <- lapply(symbolsList$SymbolID, assetPriceDataPriceAugment)
  mergeResult <- lapply(symbolsList$SymbolID, planetAspectsAssetPricesDataMerge)
}

planetsAspectsAssetsPriceDataPrepare()