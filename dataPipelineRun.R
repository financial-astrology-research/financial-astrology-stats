# Title     : Process the data pipeline steps.
# Objective : Collect watchlist assets prices, price derivatives augmentation, assets price / daily aspects merge,
#             produce frequency and descriptive statistics for each asset.
# Created by: pablocc
# Created on: 25/01/2021

source("./dailyMundaneEventsAssetReport.R")
source("./moonPhasesDataPrepare.R")
source("./planetAspectsAssetStatsPrepare.R")
source("./planetAspectsAssetsPriceDataPrepare.R")
source("./planetAspectsDataPrepare.R")
source("./planetPositionDataPrepare.R")
source("./planetPositionsAssetStatsPrepare.R")

# Clean tmp directory to ensure all tables are up to date.
unlink("./data/tmp/*.csv")
# Daily aspects table only with modern astrology planets.
allPlanetsPabloAspectsDailyAspectsTableExport()
# Prepare daily planets lingitude positions and derivatives.
dailyPlanetsPositionTablePrepare()
# Prepare daily moon phase and sign location occurrence.
dailyMoonPhaseTablePrepare()
# Prepare merged daily planets aspects + watchlist assets price CSV tables.
planetsAspectsAssetsPriceDataPrepare()
# Produce aspects / price statistics CSV tables for watchlist assets.
planetAspectsAssetStatsPrepare()
# Produce planet positions / price statistics CSV tables for watchlist assets.
planetPositionsAssetStatsPrepare()
# Produce moon phase / price statistics CSV tables for watchlist assets.
moonPhaseAssetStatsPrepare()
# Produce next upcoming N days mundane events assets report for watchlist symbols.
nDailyMundaneEventsReport(30)
