# Title     : Compute planet aspects / asset price descriptive and frequency statistics.
# Objective : Generate CSV files with the historical planet aspects asset price behavior statistics
#             to support exploratory analysis of aspects price change effects.
# Created by: pablocc
# Created on: 10/01/2021

library(data.table)
library(psych)

source("./fileSystemUtilities.R")

#' Provides assets stats report destination path.
assetsDataDestinationPath <- function() {
  paste0(normalizePath('./stats'), "/")
}

#' Prepare planet aspects / asset price side (buy / sell) frequency statistics.
#' @param planetAspectsAssetPricesTable Daily planets aspects with asset prices table.
#' @return Planets aspect price side category frequency statistics table.
planetAspectsAssetPriceSideFrequencyPrepare <- function(planetAspectsAssetPricesTable) {
  planetsAspectEffectCountLong <- planetAspectsAssetPricesTable[,
    data.table(table(OHLCEff)), by = "PlanetsAspect"
  ]

  planetAspectsEffectCountWide <- dcast(
    planetsAspectEffectCountLong,
    PlanetsAspect ~ OHLCEff,
    value.var = "N",
    fill = 0
  )

  # Total days count.
  planetAspectsEffectCountWide[, daysN := buy + sell]
  # Compute buy/sell days percentage frequency.
  planetAspectsEffectCountWide[,
    c("BuyDays%", "SellDays%") :=
      as.list(round(prop.table(c(buy, sell)), 2)),
    by = "PlanetsAspect"
  ]
}

#' Calculate planet aspects asset price descriptive statistics.
#' @param planetAspectsAssetPricesTable Daily planets aspects with asset prices table.
#' @return Planets aspect price descriptive statistics table.
planetAspectsAssetPriceDescriptivesPrepare <- function(planetAspectsAssetPricesTable) {
  planetAspectsAssetPricesTable[, round(describe(diffOHLC), 3), by = "PlanetsAspect"]
}

#' Persist stats data table into target path and file destination.
#' @param dataTable The data table to persist.
#' @param targetFileName The destination file name without extension.
dataTableStatsExport <- function(dataTable, targetFileName) {
  targetFileName <- paste0(statsDataDestinationPath(), targetFileName, ".csv")
  fwrite(dataTable, targetFileName)
  cat("Stats table exported to:", targetFileName, "\n")
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
      planetAspectsEffectCountWide,
      paste0(symbolID, "-buy_sell_count_freq_stats")
    )

    planetAspectsPriceDescriptives <- planetAspectsAssetPriceDescriptivesPrepare(planetAspectsAssetPricesTable)
    dataTableStatsExport(
      planetAspectsPriceDescriptives,
      paste0(symbolID, "-price_descriptive_stats")
    )
  }
}

planetAspectsAssetStatsPrepare()
