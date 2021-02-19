# Title     : Compute planet aspects / asset price descriptive and frequency statistics.
# Objective : Generate CSV files with the historical planet aspects asset price behavior statistics
#             to support exploratory analysis of aspects price change effects.
# Created by: pablocc
# Created on: 10/01/2021

library(data.table)
library(psych)

source("./dataExportUtils.R")
source("./dataLoadUtils.R")
source("./fileSystemUtilities.R")
source("./idsExpandUtils.R")
source("./planetAspectsAggregateUtils.R")
source("./planetPositionsAssetStatsPrepare.R")

#' Prepare planet aspects / asset price side (buy / sell) frequency statistics.
#' @param planetAspectsAssetPricesTable Daily planets aspects with asset prices table.
#' @return Planets aspect price side category frequency statistics table.
planetAspectsAssetPriceEffectFrequencyPrepare <- function(planetAspectsAssetPricesTable) {
  planetsAspectEffectCountLong <- planetAspectsAssetPricesTable[,
    data.table(table(OHLCEff)), by = "PlanetsAspect"
  ]

  frequencyTable <- dcast(
    planetsAspectEffectCountLong,
    PlanetsAspect ~ OHLCEff,
    value.var = "N",
    fill = 0
  )

  # Total days count.
  frequencyTable[, DaysN := Buy + Sell]
  # Compute buy/sell days percentage frequency.
  frequencyTable[,
    c("BuyDays%", "SellDays%") :=
      as.list(round(prop.table(c(Buy, Sell)), 2)),
    by = "PlanetsAspect"
  ]

  idParts <- c('pX', 'pY', 'aspect')
  frequencyTable[, c(idParts) := tstrsplit(PlanetsAspect, "_", fixed = T)]
  frequencyTable[, PlanetX := planetIdToNameMap(pX)]
  frequencyTable[, PlanetY := planetIdToNameMap(pY)]
  frequencyTable[, Aspect := aspectIdToNameMap(aspect)]
  frequencyTable[, c(idParts) := NULL]
  setcolorder(
    frequencyTable,
    c('PlanetsAspect', 'PlanetX', 'PlanetY', 'Aspect', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

#' Calculate planet aspects asset price descriptive statistics.
#' @param planetAspectsAssetPricesTable Daily planets aspects with asset prices table.
#' @return Planets aspect price descriptive statistics table.
planetAspectsAssetPriceDescriptivesPrepare <- function(planetAspectsAssetPricesTable) {
  planetAspectsAssetPricesTable[, round(describe(diffOHLC), 3), by = "PlanetsAspect"]
}

#' Prepare planet aspects count / asset price side (buy / sell) frequency statistics.
#' @param planetAspectsCountAssetPrice Planet aspects count with asset prices table.
#' @return Planet receiver aspects count category frequency statistics table.
planetAspectsCountFrequencyPrepare <- function(planetAspectsCountAssetPrice) {
  planetAspectsCountAssetPrice[,
    PlanetAspectsCount := paste(pID, CountRange, sep = "_")
  ]

  frequencyTable <- factorAssetPriceEffectFrequencyCount(
    planetAspectsCountAssetPrice,
    "PlanetAspectsCount"
  )

  frequencyTable[, c("pID", "CountRange") := tstrsplit(PlanetAspectsCount, "_", fixed = T)]
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, c("pID") := NULL]
  setcolorder(
    frequencyTable,
    c('PlanetAspectsCount', 'Planet', 'CountRange', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

#' Prepare planet aspect type count / asset price side (buy / sell) frequency statistics.
#' @param planetAspectsCountAssetPrice Planet aspect types count with asset prices table.
#' @return Planet aspect types count category frequency statistics table.
planetAspectTypesCountFrequencyPrepare <- function(planetAspectsCountAssetPrice) {
  planetAspectsCountAssetPrice[,
    PlanetAspectCountRange := paste(PlanetAspect, CountRange, sep = "_")
  ]

  frequencyTable <- factorAssetPriceEffectFrequencyCount(
    planetAspectsCountAssetPrice,
    "PlanetAspectCountRange"
  )

  frequencyTable[, c("pID", "Aspect", "CountRange") := tstrsplit(PlanetAspectCountRange, "_", fixed = T)]
  frequencyTable[, Planet := planetIdToNameMap(pID)]
  frequencyTable[, Aspect := aspectIdToNameMap(Aspect)]
  frequencyTable[, c("pID") := NULL]
  setcolorder(
    frequencyTable,
    c('PlanetAspectCountRange', 'Planet', 'Aspect', 'CountRange', 'Buy', 'Sell', 'DaysN', 'BuyDays%', 'SellDays%')
  )
}

planetAspectsAssetStatsPrepare <- function() {
  # TODO: Extract data tmp path composition to FS utilities and use here.
  planetAspectsAssetPricesFiles <- list.files("./data/tmp", pattern = "*aspects_set_long.csv")

  for (planetAspectsAssetPricesFile in planetAspectsAssetPricesFiles) {
    filenameParts <- unlist(strsplit(planetAspectsAssetPricesFile, "--"))
    symbolID <- filenameParts[1]
    planetAspectsAssetPricesTable <- fread(paste0("./data/tmp/", planetAspectsAssetPricesFile))
    planetAspectsAssetPricesTable[, pX := substr(origin, 1, 2)]
    planetAspectsAssetPricesTable[, pY := substr(origin, 3, 4)]
    planetAspectsAssetPricesTable[, PlanetsAspect := paste(pX, pY, aspect, sep = "_")]

    dataTableStatsExport(
      symbolID,
      planetAspectsAssetPriceEffectFrequencyPrepare(planetAspectsAssetPricesTable),
      paste(symbolID, "planets_aspects", "buy_sell_count_freq_stats", sep = "-")
    )

    planetReceiverAspectsCountAssetPrice <- planetReceiverAspectsCountAssetPricePrepare(symbolID)
    dataTableStatsExport(
      symbolID,
      planetAspectsCountFrequencyPrepare(planetReceiverAspectsCountAssetPrice),
      paste(symbolID, "planet_receiver_aspects", "buy_sell_count_freq_stats", sep = "-")
    )

    planetReceiverAspectTypesCountAssetPrice <- planetReceiverAspectTypesCountAssetPricePrepare(symbolID)
    dataTableStatsExport(
      symbolID,
      planetAspectTypesCountFrequencyPrepare(planetReceiverAspectTypesCountAssetPrice),
      paste(symbolID, "planet_receiver_aspect_types", "buy_sell_count_freq_stats", sep = "-")
    )

    planetEmitterAspectsCountAssetPrice <- planetEmitterAspectsCountAssetPricePrepare(symbolID)
    dataTableStatsExport(
      symbolID,
      planetAspectsCountFrequencyPrepare(planetEmitterAspectsCountAssetPrice),
      paste(symbolID, "planet_emitter_aspects", "buy_sell_count_freq_stats", sep = "-")
    )

    planetEmitterAspectTypesCountAssetPrice <- planetEmitterAspectTypesCountAssetPricePrepare(symbolID)
    dataTableStatsExport(
      symbolID,
      planetAspectTypesCountFrequencyPrepare(planetEmitterAspectTypesCountAssetPrice),
      paste(symbolID, "planet_emitter_aspect_types", "buy_sell_count_freq_stats", sep = "-")
    )

    planetAspectsCountAssetPrice <- planetAspectsCountAssetPricePrepare(symbolID)
    dataTableStatsExport(
      symbolID,
      planetAspectsCountFrequencyPrepare(planetAspectsCountAssetPrice),
      paste(symbolID, "planet_aspects", "buy_sell_count_freq_stats", sep = "-")
    )

    planetAspectTypesCountAssetPrice <- planetAspectTypesCountAssetPricePrepare(symbolID)
    dataTableStatsExport(
      symbolID,
      planetAspectTypesCountFrequencyPrepare(planetAspectTypesCountAssetPrice),
      paste(symbolID, "planet_aspect_types", "buy_sell_count_freq_stats", sep = "-")
    )

    dataTableStatsExport(
      symbolID,
      planetAspectsAssetPriceDescriptivesPrepare(planetAspectsAssetPricesTable),
      paste(symbolID, "planets_aspects", "price_descriptive_stats", sep = "-")
    )
  }
}

natalTransitAspectsAssetPricesPrepare <- function(symbolID) {
  symbolIdParts <- unlist(strsplit(symbolID, "-"))
  isCurrencyPair <- length(symbolIdParts) == 2
  # When is currency pair natal transits are based on base currency chart.
  natalSymbolId <- ifelse(isCurrencyPair, symbolIdParts[1], symbolID)
  dailyNatalTransitAspectsLong <- assetNatalTransitAspectsLoad(natalSymbolId)
  dailyAssetAugmentedData <- assetAugmentedDataLoad(symbolID)
  merge(dailyNatalTransitAspectsLong, dailyAssetAugmentedData, by = 'Date')
}

natalTransitAspectsFrequencyStatsPrepare <- function(symbolID) {
  natalTransitAspectsAssetPricesTable <- natalTransitAspectsAssetPricesPrepare(symbolID)
  natalTransitAspectsAssetPricesTable[, pX := substr(origin, 1, 2)]
  natalTransitAspectsAssetPricesTable[, pY := substr(origin, 3, 4)]
  natalTransitAspectsAssetPricesTable[, PlanetsAspect := paste(pX, pY, aspect, sep = "_")]
  dataTableStatsExport(
    symbolID,
    planetAspectsAssetPriceEffectFrequencyPrepare(natalTransitAspectsAssetPricesTable),
    paste(symbolID, "transit_aspects", "buy_sell_count_freq_stats", sep = "-")
  )
}

natalTransitAspectsFrequencyStatsPrepare("BTC-USD")
