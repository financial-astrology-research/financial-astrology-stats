# Title     : Compute planet aspects / asset price descriptive and frequency statistics.
# Objective : Generate CSV files with the historical planet aspects asset price behavior statistics
#             to support exploratory analysis of aspects price change effects.
# Created by: pablocc
# Created on: 10/01/2021

library(data.table)

source("./fileSystemUtilities.R")

#' Provides assets stats report destination path.
assetsDataDestinationPath <- function() {
  paste0(normalizePath('./stats'), "/")
}

planetAspectsAssetStatsPrepare <- function() {
  # TODO: Extract data tmp path composition to FS utilities and use here.
  planetAspectsAssetPricesFiles <- list.files("./data/tmp", pattern = "*aspects_set_long.csv")

  for (planetAspectsAssetPricesFile in planetAspectsAssetPricesFiles) {
    filenameParts <- unlist(strsplit(planetAspectsAssetPricesFile, "--"))
    symbolID <- filenameParts[1]

    planetAspectsAssetPricesTable <- fread(paste0("./data/tmp/", planetAspectsAssetPricesFile))
    planetAspectsAssetPricesTable[, PlanetsAspect := paste0(origin, "_", aspect)]
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

    targetFileName <- paste0("./stats/", symbolID, "-buy_sell_count_freq_stats", ".csv")
    fwrite(planetAspectsEffectCountWide, targetFileName)

    cat(
      symbolID,
      "- Aspects price change effect category frequency stats exported:",
      targetFileName,
      "\n"
    )
  }
}

planetAspectsAssetStatsPrepare()

