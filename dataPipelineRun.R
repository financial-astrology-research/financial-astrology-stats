# Title     : Process the data pipeline steps.
# Objective : Collect watchlist assets prices, price derivatives augmentation, assets price / daily aspects merge,
#             produce frequency and descriptive statistics for each asset.
# Created by: pablocc
# Created on: 25/01/2021

source("./planetAspectsDataPrepare.R")
source("./planetAspectsAssetsPriceDataPrepare.R")
source("./planetAspectsAssetStatsPrepare.R")
source("./planetPositionDataPrepare.R")
source("./planetPositionsAssetStatsPrepare.R")

# Clean tmp directory to ensure all tables are up to date.
unlink("./data/tmp/*.csv")
# Daily aspects table only with modern astrology planets.
allPlanetsPabloAspectsDailyAspectsTableExport()
# Prepare daily planets lingitude positions and derivatives.
dailyPlanetsPositionTablePrepare()
# Prepare merged daily planets aspects + watchlist assets price CSV tables.
planetsAspectsAssetsPriceDataPrepare()
# Produce aspects / price statistics CSV tables for watchlist assets.
planetAspectsAssetStatsPrepare()
# Produce planet positions / price statistics CSV tables for watchlist assets.
planetPositionsAssetStatsPrepare()
