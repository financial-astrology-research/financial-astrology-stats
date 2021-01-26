# Title     : Process the data pipeline steps.
# Objective : Collect watchlist assets prices, price derivatives augmentation, assets price / daily aspects merge,
#             produce frequency and descriptive statistics for each asset.
# Created by: pablocc
# Created on: 25/01/2021

source("./planetAspectsDataPrepare.R")
source("./planetAspectsAssetsPriceDataPrepare.R")
source("./planetAspectsAssetStatsPrepare.R")
source("./planetPositionDataPrepare.R")

# Daily aspects table only with modern astrology planets.
modernPlanetsPabloAspectsDailyAspectsTableExport()
# Daily aspects table with modern astrology planets and asteroids Ceres, Vesta, Chiron and the North Node.
modernPlanetsCEVSCHNNPabloAspectsDailyAspectsTableExport()
# Prepare daily planets lingitude positions and derivatives.
dailyPlanetsPositionTablePrepare()
# Prepare merged daily planets aspects + watchlist assets price CSV tables.
planetsAspectsAssetsPriceDataPrepare()
# Produce aspects / price statistics CSV tables for watchlist assets.
planetAspectsAssetStatsPrepare()
# Produce planet positions / price statistics CSV tables for watchlist assets.
planetPositionsAssetStatsPrepare()
