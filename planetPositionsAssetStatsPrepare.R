# Title     : Compute planet position longitude / asset price descriptive and frquency statistics.
# Objective : Generate CSV files with the historical planet possition asset price behavior to find significant
#             astrological rules that can be used for trading signals.
# Created by: pablocc
# Created on: 26/01/2021

source("./configUtils.R")
source("./dataMergeUtils.R")
source("./fileSystemUtilities.R")

#' Calculate asset price category frequency by a given factor variable.
#' @param factorAssetTable Factor variable with asset prices long table.
#' @return Factor / asset price (buy / sell) category frequency statistics table.
factorAssetPriceFrequencyCount <- function (factorAssetTable, byFactor) {
  variableEffectCountLong <- factorAssetTable[,
    data.table(table(OHLCEff)), by = byFactor
  ]

  effectCountWide <- dcast(
    variableEffectCountLong,
    formula(paste0(byFactor, " ~ OHLCEff")),
    value.var = "N",
    fill = 0
  )

  # Total days count.
  effectCountWide[, daysN := buy + sell]
  # Compute buy/sell days percentage frequency.
  effectCountWide[,
    c("BuyDays%", "SellDays%") :=
      as.list(round(prop.table(c(buy, sell)), 2)),
    by = byFactor
  ]
}

#' Prepare planets zodiac sign / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets zodiac sign price category frequency statistics table.
planetZodSignAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetZodSign := paste0(pID, "_", zsign)]
  factorAssetPriceFrequencyCount(planetPositionAssetTable, "PlanetZodSign")
}

#' Prepare planets triplicity / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets triplicity price category frequency statistics table.
planetTriplicityAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetTriplicity := paste0(pID, "_", triplicity)]
  factorAssetPriceFrequencyCount(planetPositionAssetTable, "PlanetTriplicity")
}

#' Prepare planets element / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets element price category frequency statistics table.
planetElementAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetElement := paste0(pID, "_", element)]
  factorAssetPriceFrequencyCount(planetPositionAssetTable, "PlanetElement")
}

#' Persist stats data table into target path and file destination.
#' @param dataTable The data table to persist.
#' @param targetFileName The destination file name without extension.
dataTableStatsExport <- function(dataTable, targetFileName) {
  targetFileName <- paste0(statsDataDestinationPath(), targetFileName, ".csv")
  fwrite(dataTable, targetFileName)
  cat("Stats table exported to:", targetFileName, "\n")
}

#' Calculate planets positions / asset price effect statistics.
planetPositionsAssetStatsPrepare <- function() {
  watchList <- assetsWatchList()
  for (symbolID in watchList$SymbolID) {
    planetsPositionsAssetPriceTable <- planetsPositionsAssetPricesDataMerge(symbolID)

    planetsZodSignFrequencyStats <- planetZodSignAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      planetsZodSignFrequencyStats,
      paste0(symbolID, "-planet_zodsign", "-buy_sell_count_freq_stats")
    )

    planetsTriplicityFrequencyStats <- planetTriplicityAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      planetsTriplicityFrequencyStats,
      paste0(symbolID, "-planet_triplicity", "-buy_sell_count_freq_stats")
    )

    planetsTriplicityFrequencyStats <- planetElementAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      planetsTriplicityFrequencyStats,
      paste0(symbolID, "-planet_element", "-buy_sell_count_freq_stats")
    )
  }
}
