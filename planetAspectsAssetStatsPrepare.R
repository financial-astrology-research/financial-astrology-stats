# Title     : Compute planet aspects / asset price descriptive and frequency statistics.
# Objective : Generate CSV files with the historical planet aspects asset price behavior statistics
#             to support exploratory analysis of aspects price change effects.
# Created by: pablocc
# Created on: 10/01/2021

library(data.table)
library(psych)

source("./fileSystemUtilities.R")
source("./idsExpandUtils.R")

#' Prepare planet aspects / asset price side (buy / sell) frequency statistics.
#' @param planetAspectsAssetPricesTable Daily planets aspects with asset prices table.
#' @return Planets aspect price side category frequency statistics table.
planetAspectsAssetPriceSideFrequencyPrepare <- function(planetAspectsAssetPricesTable) {
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
  frequencyTable[, daysN := buy + sell]
  # Compute buy/sell days percentage frequency.
  frequencyTable[,
    c("BuyDays%", "SellDays%") :=
      as.list(round(prop.table(c(buy, sell)), 2)),
    by = "PlanetsAspect"
  ]

  pX <- substr(frequencyTable$PlanetsAspect, 1, 2)
  pY <- substr(frequencyTable$PlanetsAspect, 3, 4)
  aspect <- substr(frequencyTable$PlanetsAspect, 6, 10)
  frequencyTable[, PlanetX := planetIdToNameMap(pX)]
  frequencyTable[, PlanetY := planetIdToNameMap(pY)]
  frequencyTable[, Aspect := aspectIdToNameMap(aspect)]
  setcolorder(
    frequencyTable,
    c('PlanetsAspect', 'PlanetX', 'PlanetY', 'Aspect', 'buy', 'sell', 'daysN', 'BuyDays%', 'SellDays%')
  )
}

#' Calculate planet aspects asset price descriptive statistics.
#' @param planetAspectsAssetPricesTable Daily planets aspects with asset prices table.
#' @return Planets aspect price descriptive statistics table.
planetAspectsAssetPriceDescriptivesPrepare <- function(planetAspectsAssetPricesTable) {
  planetAspectsAssetPricesTable[, round(describe(diffOHLC), 3), by = "PlanetsAspect"]
}

planetAspectsAssetStatsPrepare <- function() {
  # TODO: Extract data tmp path composition to FS utilities and use here.
  planetAspectsAssetPricesFiles <- list.files("./data/tmp", pattern = "*aspects_set_long.csv")

  for (planetAspectsAssetPricesFile in planetAspectsAssetPricesFiles) {
    filenameParts <- unlist(strsplit(planetAspectsAssetPricesFile, "--"))
    symbolID <- filenameParts[1]
    planetAspectsAssetPricesTable <- fread(paste0("./data/tmp/", planetAspectsAssetPricesFile))
    planetAspectsAssetPricesTable[, PlanetsAspect := paste0(origin, "_", aspect)]

    planetAspectsEffectCountWide <- planetAspectsAssetPriceSideFrequencyPrepare(planetAspectsAssetPricesTable)
    dataTableStatsExport(
      symbolID,
      planetAspectsEffectCountWide,
      paste(symbolID, "planets_aspects", "buy_sell_count_freq_stats", sep = "-")
    )

    planetAspectsPriceDescriptives <- planetAspectsAssetPriceDescriptivesPrepare(planetAspectsAssetPricesTable)
    dataTableStatsExport(
      symbolID,
      planetAspectsPriceDescriptives,
      paste(symbolID, "planets_aspects", "price_descriptive_stats", sep = "-")
    )
  }
}
