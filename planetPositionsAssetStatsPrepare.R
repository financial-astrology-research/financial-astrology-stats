# Title     : Compute planet position longitude / asset price descriptive and frquency statistics.
# Objective : Generate CSV files with the historical planet possition asset price behavior to find significant
#             astrological rules that can be used for trading signals.
# Created by: pablocc
# Created on: 26/01/2021

source("./configUtils.R")
source("./dataExportUtils.R")
source("./dataMergeUtils.R")
source("./fileSystemUtilities.R")
source("./idsExpandUtils.R")

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
  frequencyTable <- factorAssetPriceFrequencyCount(planetPositionAssetTable, "PlanetZodSign")
  pID <- substr(frequencyTable$PlanetZodSign, 1, 2)
  zodSignID <- substr(frequencyTable$PlanetZodSign, 4, 6)
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, ZodSign := zodSignIdToNameMap(zodSignID)]
  setcolorder(
    frequencyTable,
    c('PlanetZodSign', 'Planet', 'ZodSign', 'buy', 'sell', 'daysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planets triplicity / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets triplicity price category frequency statistics table.
planetTriplicityAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetTriplicity := paste0(pID, "_", triplicity)]
  frequencyTable <- factorAssetPriceFrequencyCount(planetPositionAssetTable, "PlanetTriplicity")
  pID <- substr(frequencyTable$PlanetTriplicity, 1, 2)
  triplicityID <- substr(frequencyTable$PlanetTriplicity, 4, 6)
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, Triplicity := triplicityIdToNameMap(triplicityID)]
  setcolorder(
    frequencyTable,
    c('PlanetTriplicity', 'Planet', 'Triplicity', 'buy', 'sell', 'daysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planets element / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets element price category frequency statistics table.
planetElementAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetElement := paste0(pID, "_", element)]
  frequencyTable <- factorAssetPriceFrequencyCount(planetPositionAssetTable, "PlanetElement")
  pID <- substr(frequencyTable$PlanetElement, 1, 2)
  elementID <- substr(frequencyTable$PlanetElement, 4, 6)
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, Element := elementIdToNameMap(elementID)]
  setcolorder(
    frequencyTable,
    c('PlanetElement', 'Planet', 'Element', 'buy', 'sell', 'daysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planets polarity / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets polarity price category frequency statistics table.
planetPolarityAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetPolarity := paste0(pID, "_", Polarity)]
  frequencyTable <- factorAssetPriceFrequencyCount(planetPositionAssetTable, "PlanetPolarity")
  pID <- substr(frequencyTable$PlanetPolarity, 1, 2)
  polarityID <- substr(frequencyTable$PlanetPolarity, 4, 6)
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, Polarity := polarityIdToNameMap(polarityID)]
  setcolorder(
    frequencyTable,
    c('PlanetPolarity', 'Planet', 'Polarity', 'buy', 'sell', 'daysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planets speed mode / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets polarity price category frequency statistics table.
planetSpeedModeAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetSpeedPhase := paste0(pID, "_", speedmode)]
  frequencyTable <- factorAssetPriceFrequencyCount(planetPositionAssetTable, "PlanetSpeedPhase")
  pID <- substr(frequencyTable$PlanetSpeedPhase, 1, 2)
  speedPhaseID <- substr(frequencyTable$PlanetSpeedPhase, 4, 6)
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, SpeedPhase := speedPhaseIdToNameMap(speedPhaseID)]
  setcolorder(
    frequencyTable,
    c('PlanetSpeedPhase', 'Planet', 'SpeedPhase', 'buy', 'sell', 'daysN', 'BuyDays%', 'SellDays%')
  )
}

#' Calculate planets positions / asset price effect statistics.
planetPositionsAssetStatsPrepare <- function() {
  watchList <- assetsWatchList()
  for (symbolID in watchList$SymbolID) {
    planetsPositionsAssetPriceTable <- planetsPositionsAssetPricesDataMerge(symbolID)

    planetsZodSignFrequencyStats <- planetZodSignAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      planetsZodSignFrequencyStats,
      paste(symbolID, "planet_zodsign", "buy_sell_count_freq_stats", sep = "-")
    )

    planetsPolarityFrequencyStats <- planetPolarityAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      planetsPolarityFrequencyStats,
      paste(symbolID, "planet_polarity", "buy_sell_count_freq_stats", sep = "-")
    )

    planetsTriplicityFrequencyStats <- planetTriplicityAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      planetsTriplicityFrequencyStats,
      paste(symbolID, "planet_triplicity", "buy_sell_count_freq_stats", sep = "-")
    )

    planetsElementFrequencyStats <- planetElementAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      planetsElementFrequencyStats,
      paste(symbolID, "planet_element", "buy_sell_count_freq_stats", sep = "-")
    )

    planetsSpeedModeFrequencyStats <- planetSpeedModeAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      planetsSpeedModeFrequencyStats,
      paste(symbolID, "planet_speed", "buy_sell_count_freq_stats", sep = "-")
    )
  }
}
