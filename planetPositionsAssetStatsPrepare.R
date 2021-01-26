# Title     : Compute planet position longitude / asset price descriptive and frquency statistics.
# Objective : Generate CSV files with the historical planet possition asset price behavior to find significant
#             astrological rules that can be used for trading signals.
# Created by: pablocc
# Created on: 26/01/2021

source("./configUtils.R")
source("./dataMergeUtils.R")
source("./fileSystemUtilities.R")

#' Prepare planets zodiac sign / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets zodiac sign price side category frequency statistics table.
planetZodSignAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  variableEffectCountLong <- planetPositionAssetTable[,
    data.table(table(OHLCEff)), by = "PlanetZodSign"
  ]

  effectCountWide <- dcast(
    variableEffectCountLong,
    PlanetZodSign ~ OHLCEff,
    value.var = "N",
    fill = 0
  )

  # Total days count.
  effectCountWide[, daysN := buy + sell]
  # Compute buy/sell days percentage frequency.
  effectCountWide[,
    c("BuyDays%", "SellDays%") :=
      as.list(round(prop.table(c(buy, sell)), 2)),
    by = "PlanetZodSign"
  ]
}

#' Persist stats data table into target path and file destination.
#' @param dataTable The data table to persist.
#' @param targetFileName The destination file name without extension.
dataTableStatsExport <- function(dataTable, targetFileName) {
  targetFileName <- paste0(statsDataDestinationPath(), targetFileName, ".csv")
  fwrite(dataTable, targetFileName)
  cat("Stats table exported to:", targetFileName, "\n")
}

planetPositionsAssetStatsPrepare <- function() {
  watchList <- assetsWatchList()
  for (symbolID in watchList$SymbolID) {
    planetsPositionsAssetPriceTable <- planetsPositionsAssetPricesDataMerge(symbolID)
    planetsPositionsAssetPriceTable[, PlanetZodSign := paste0(pID, "_", zsign)]
    planetsZodSignFrequencyStats <- planetZodSignAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)

    dataTableStatsExport(
      planetsZodSignFrequencyStats,
      paste0(symbolID, "-planet_zodsign", "-buy_sell_count_freq_stats")
    )
  }
}

planetPositionsAssetStatsPrepare()