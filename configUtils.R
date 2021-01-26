# Title     : Configuration utilities.
# Objective : Support customization of some computations.
# Created by: pablocc
# Created on: 26/01/2021

#' Load watchlist CSV config file with assets symbols to watch.
assetsWatchList <- function() {
  listFilePath <- normalizePath('./config/watchlist.csv')
  symbolsList <- fread(listFilePath, header = F)
  setnames(symbolsList, c('SymbolID'))
}
