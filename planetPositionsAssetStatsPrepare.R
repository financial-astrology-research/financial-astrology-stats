# Title     : Compute planet position longitude / asset price descriptive and frquency statistics.
# Objective : Generate CSV files with the historical planet possition asset price behavior to find significant
#             astrological rules that can be used for trading signals.
# Created by: pablocc
# Created on: 26/01/2021

library(data.table)

source("./configUtils.R")
source("./dataExportUtils.R")
source("./dataMergeUtils.R")
source("./fileSystemUtilities.R")
source("./idsExpandUtils.R")
source("./planetPositionAggregateUtils.R")

#' Calculate asset price category frequency by a given factor variable.
#' @param factorAssetTable Factor variable with asset prices long table.
#' @return Factor / asset price (buy / sell) category frequency statistics table.
factorAssetPriceEffectFrequencyCount <- function (factorAssetTable, byFactor) {
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
  effectCountWide[, DaysN := Buy + Sell]
  # Compute buy/sell days percentage frequency.
  effectCountWide[,
    c("BuyDays%", "SellDays%") :=
      as.list(round(prop.table(c(Buy, Sell)), 2)),
    by = byFactor
  ]
}

#' Prepare planets zodiac sign / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets zodiac sign / price category frequency statistics table.
planetZodSignAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetZodSign := paste(pID, ZodSignN, ZodSignID, sep = "_")]
  frequencyTable <- factorAssetPriceEffectFrequencyCount(planetPositionAssetTable, "PlanetZodSign")
  pID <- substr(frequencyTable$PlanetZodSign, 1, 2)
  zodSignID <- substr(frequencyTable$PlanetZodSign, 4, 6)
  frequencyTable[, c('pID', 'ZodSignN', 'ZodSignID') := tstrsplit(PlanetZodSign, "_", fixed = T)]
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, ZodSign := zodSignIdToNameMap(ZodSignID)]
  frequencyTable[, c('pID', 'ZodSignN', 'ZodSignID') := NULL]
  setcolorder(
    frequencyTable,
    c('PlanetZodSign', 'Planet', 'ZodSign', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planets decan / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets decan / price category frequency statistics table.
planetDecanAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[,
    PlanetDecan := paste(pID, ZodSignN, DecanID, sep = "_")
  ]

  frequencyTable <- factorAssetPriceEffectFrequencyCount(planetPositionAssetTable, "PlanetDecan")
  frequencyTable[, c("pID", "ZodSignN", "decanID") := tstrsplit(PlanetDecan, "_", fixed = T)]
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, Decan := decanIdToNameMap(decanID)]
  frequencyTable[, c("pID", "ZodSignN", "decanID") := NULL]
  frequencyTable[order(PlanetDecan)]
  setcolorder(
    frequencyTable,
    c('PlanetDecan', 'Planet', 'Decan', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planets quality / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets quality / price category frequency statistics table.
planetQualityAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetQuality := paste0(pID, "_", QualityID)]
  frequencyTable <- factorAssetPriceEffectFrequencyCount(planetPositionAssetTable, "PlanetQuality")
  pID <- substr(frequencyTable$PlanetQuality, 1, 2)
  qualityID <- substr(frequencyTable$PlanetQuality, 4, 6)
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, Quality := qualityIdToNameMap(qualityID)]
  setcolorder(
    frequencyTable,
    c('PlanetQuality', 'Planet', 'Quality', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planets element / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets element / price category frequency statistics table.
planetElementAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetElement := paste0(pID, "_", ElementID)]
  frequencyTable <- factorAssetPriceEffectFrequencyCount(planetPositionAssetTable, "PlanetElement")
  pID <- substr(frequencyTable$PlanetElement, 1, 2)
  elementID <- substr(frequencyTable$PlanetElement, 4, 6)
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, Element := elementIdToNameMap(elementID)]
  setcolorder(
    frequencyTable,
    c('PlanetElement', 'Planet', 'Element', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planets polarity / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets polarity / price category frequency statistics table.
planetPolarityAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetPolarity := paste0(pID, "_", PolarityID)]
  frequencyTable <- factorAssetPriceEffectFrequencyCount(planetPositionAssetTable, "PlanetPolarity")
  pID <- substr(frequencyTable$PlanetPolarity, 1, 2)
  polarityID <- substr(frequencyTable$PlanetPolarity, 4, 6)
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, Polarity := polarityIdToNameMap(polarityID)]
  setcolorder(
    frequencyTable,
    c('PlanetPolarity', 'Planet', 'Polarity', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planets speed mode / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets speed phase / price category frequency statistics table.
planetSpeedPhaseAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetSpeedPhase := paste0(pID, "_", SpeedPhaseID)]
  frequencyTable <- factorAssetPriceEffectFrequencyCount(planetPositionAssetTable, "PlanetSpeedPhase")
  pID <- substr(frequencyTable$PlanetSpeedPhase, 1, 2)
  speedPhaseID <- substr(frequencyTable$PlanetSpeedPhase, 4, 6)
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, SpeedPhase := speedPhaseIdToNameMap(speedPhaseID)]
  setcolorder(
    frequencyTable,
    c('PlanetSpeedPhase', 'Planet', 'SpeedPhase', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planets vedic moon mansion / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets vedic mansion / price category frequency statistics table.
planetVedicMansionAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetVedicMansion := paste0(pID, "_", VedicMansionID)]
  frequencyTable <- factorAssetPriceEffectFrequencyCount(planetPositionAssetTable, "PlanetVedicMansion")
  frequencyTable[, c("pID", "VedicMansionID") := tstrsplit(PlanetVedicMansion, "_", fixed = T)]
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, VedicMansion := vedicMansionIdToNameMap(VedicMansionID)]
  frequencyTable[, c('pID', 'VedicMansionID') := NULL]
  setcolorder(
    frequencyTable,
    c('PlanetVedicMansion', 'Planet', 'VedicMansion', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planets arab moon mansion / asset price side (buy / sell) frequency statistics.
#' @param planetPositionAssetTable Daily planets positions with asset prices long table.
#' @return Planets arab mansion / price category frequency statistics table.
planetArabMansionAssetPriceSideFrequencyPrepare <- function(planetPositionAssetTable) {
  planetPositionAssetTable[, PlanetArabMansion := paste0(pID, "_", ArabMansionID)]
  frequencyTable <- factorAssetPriceEffectFrequencyCount(planetPositionAssetTable, "PlanetArabMansion")
  frequencyTable[, c("pID", "ArabMansionID") := tstrsplit(PlanetArabMansion, "_", fixed = T)]
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, ArabMansion := arabMansionIdToNameMap(ArabMansionID)]
  frequencyTable[, c('pID', 'ArabMansionID') := NULL]
  setcolorder(
    frequencyTable,
    c('PlanetArabMansion', 'Planet', 'ArabMansion', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare moon phase / asset price side (buy / sell) frequency statistics.
#' @param moonPhaseAssetTable Daily moon phase with asset prices table.
#' @return Planets moon phase / price category frequency statistics table.
moonPhaseAssetPriceSideFrequencyPrepare <- function(moonPhaseAssetTable) {
  frequencyTable <- factorAssetPriceEffectFrequencyCount(moonPhaseAssetTable, "MoonPhaseID")
  frequencyTable[, MoonPhase := moonPhaseIdToNameMap(MoonPhaseID)]
  setcolorder(
    frequencyTable,
    c('MoonPhaseID', 'MoonPhase', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare moon phase within zodiac sign / asset price side (buy / sell) frequency statistics.
#' @param moonPhaseAssetTable Daily moon phase with asset prices table.
#' @return Planets moon phase / price category frequency statistics table.
moonPhaseZodSignAssetPriceSideFrequencyPrepare <- function(moonPhaseAssetTable) {
  moonPhaseAssetTable[,
    MoonPhaseZodSignID := paste(MoonPhaseID, ZodSignN, ZodSignID, sep = "_")
  ]

  frequencyTable <- factorAssetPriceEffectFrequencyCount(moonPhaseAssetTable, "MoonPhaseZodSignID")
  frequencyTable[, c("MoonPhaseID", "ZodSignN", "ZodSignID") := tstrsplit(MoonPhaseZodSignID, "_", fixed = T)]
  frequencyTable[, MoonPhase := moonPhaseIdToNameMap(MoonPhaseID)]
  frequencyTable[, ZodSign := zodSignIdToNameMap(ZodSignID)]
  frequencyTable[, c("MoonPhaseID", "ZodSignN", "ZodSignID") := NULL]
  setcolorder(
    frequencyTable,
    c('MoonPhaseZodSignID', 'MoonPhase', 'ZodSign', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planet in elment count / asset price side (buy / sell) frequency statistics.
#' @param planetElementCountAssetPrice Planet element count with asset prices table.
#' @return Planet in element count category frequency statistics table.
planetElementsCountFrequencyPrepare <- function(planetElementCountAssetPrice) {
  planetElementCountAssetPrice[,
    PlanetElementCountRange := paste(PlanetElement, CountRange, sep = "_")
  ]

  frequencyTable <- factorAssetPriceEffectFrequencyCount(
    planetElementCountAssetPrice,
    "PlanetElementCountRange"
  )

  frequencyTable[, c("Element", "CountRange") := tstrsplit(PlanetElementCountRange, "_", fixed = T)]
  frequencyTable[, Element := elementIdToNameMap(Element)]
  setcolorder(
    frequencyTable,
    c('PlanetElementCountRange', 'Element', 'CountRange', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
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

    planetsDecanFrequencyStats <- planetDecanAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      planetsDecanFrequencyStats,
      paste(symbolID, "planet_decan", "buy_sell_count_freq_stats", sep = "-")
    )

    planetsPolarityFrequencyStats <- planetPolarityAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      planetsPolarityFrequencyStats,
      paste(symbolID, "planet_polarity", "buy_sell_count_freq_stats", sep = "-")
    )

    planetsQualityFrequencyStats <- planetQualityAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      planetsQualityFrequencyStats,
      paste(symbolID, "planet_quality", "buy_sell_count_freq_stats", sep = "-")
    )

    planetsElementFrequencyStats <- planetElementAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      planetsElementFrequencyStats,
      paste(symbolID, "planet_element", "buy_sell_count_freq_stats", sep = "-")
    )

    planetElementsCountFrequencyStats <- planetElementsCountAssetPricePrepare(symbolID) %>%
      planetElementsCountFrequencyPrepare()
    dataTableStatsExport(
      symbolID,
      planetElementsCountFrequencyStats,
      paste(symbolID, "planet_elements", "buy_sell_count_freq_stats", sep = "-")
    )

    planetsSpeedPhaseFrequencyStats <- planetSpeedPhaseAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      planetsSpeedPhaseFrequencyStats,
      paste(symbolID, "planet_speed", "buy_sell_count_freq_stats", sep = "-")
    )

    planetsArabMoonMansionFrequencyStats <- planetArabMansionAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      planetsArabMoonMansionFrequencyStats,
      paste(symbolID, "planet_arab_mansion", "buy_sell_count_freq_stats", sep = "-")
    )

    planetsVedicMoonMansionFrequencyStats <- planetVedicMansionAssetPriceSideFrequencyPrepare(planetsPositionsAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      planetsVedicMoonMansionFrequencyStats,
      paste(symbolID, "planet_vedic_mansion", "buy_sell_count_freq_stats", sep = "-")
    )
  }
}

#' Calculate moon phases / asset price effect statistics.
moonPhaseAssetStatsPrepare <- function() {
  watchList <- assetsWatchList()
  for (symbolID in watchList$SymbolID) {
    moonPhaseAssetPriceTable <- moonPhaseAssetPricesDataMerge(symbolID)

    moonPhaseFrequencyStats <- moonPhaseAssetPriceSideFrequencyPrepare(moonPhaseAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      moonPhaseFrequencyStats,
      paste(symbolID, "moon_phase", "buy_sell_count_freq_stats", sep = "-")
    )

    moonPhaseZodSignFrequencyStats <- moonPhaseZodSignAssetPriceSideFrequencyPrepare(moonPhaseAssetPriceTable)
    dataTableStatsExport(
      symbolID,
      moonPhaseZodSignFrequencyStats,
      paste(symbolID, "moon_phase_zod_sign", "buy_sell_count_freq_stats", sep = "-")
    )
  }
}
planetPositionsAssetStatsPrepare()