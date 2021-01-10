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
#' @param symbol Data source asset symbol ID.
#' @param source Data provider source ID supported by quantmod.
#' @return Boolean vector with TRUE for success symbol data download and FALSE for failed.
assetPriceDataFetch <- function(symbol, source = "yahoo") {
  # Date to retrieve asset price since if available, oldest data source records are around this year.
  startDate = as.Date("1970-01-01")
  maxRetry <- 5

  for (t in 1:maxRetry) {
    tryCatch({
      cat("Downloading", symbol, "- attempt: #", t, "of", maxRetry, "\n")
      symbolDataFrame <- getSymbols(
        symbol,
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
      targetFileName <- paste0(assetsDataDestinationPath(), symbol, ".csv")
      write.csv(symbolDataFrame, file = targetFileName, row.names = F)
      cat("Sucessfully saved the asset data to:", targetFileName, "\n")
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

watchListPriceDataFetch()