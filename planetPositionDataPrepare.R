# Title     : Calculate planet position price effect statistics for watchlist assets.
# Objective : Explore the effect of planet position by: sign polarity, element, triplicity
#             and zodiac sign.
# Created by: pablocc
# Created on: 26/01/2021

library(plyr)

source("./fileSystemUtilities.R")
source("./planetAspectsDataPrepare.R")

#' Augment planet positions data table with zodiacal sign.
#' @param Planet longitude positions long data table.
#' @return Daily planets position table augmented with zodiacal sign.
zodiacSignPositionTableAugment <- function(planetLongitudeTableLong) {
  zsignN <- seq(1, 12)
  zsignName <- c(
    'ARI',
    'TAU',
    'GEM',
    'CAN',
    'LEO',
    'VIR',
    'LIB',
    'SCO',
    'SAG',
    'CAP',
    'AQU',
    'PIS'
  )

  # Prevent zero division.
  planetLongitudeTableLong[lon == 0, lon := 0.1]
  # Calculate zodiacal signs for each planet longitude.
  planetLongitudeTableLong[, zsign := ceiling(lon / 30)]
  planetLongitudeTableLong[, zsign := mapvalues(zsign, zsignN, zsignName)]
}

#' Prepare daily planets longitude position and categorical derivatives: polarity, triplicity, element, sign, etc.
#' @return Daily planets position data table that includes: longitude and it's categorical derivatives.
dailyPlanetsPositionTablePrepare <- function() {
  cat("Preparing daily planets position table.\n")
  planetPositionsTable <- loadPlanetsPositionTable("daily")
  colNames <- colnames(planetPositionsTable)
  longitudeColNames <- colNames[grep("^..LON", colNames)]
  planetLongitudeTableLong <- melt(
    planetPositionsTable,
    id.var = "Date",
    measure.var = longitudeColNames
  )

  # Extract planet code ID.
  planetLongitudeTableLong[, pID := substr(variable, 1, 2)]
  planetLongitudeTableLong[, variable := NULL]
  # Customize columns names.
  setnames(planetLongitudeTableLong, c('Date', 'lon', 'pID'))
  setcolorder(planetLongitudeTableLong, c('Date', 'pID', 'lon'))
  zodiacSignPositionTableAugment(planetLongitudeTableLong)

  fwrite(
    planetLongitudeTableLong,
    expandPath("./data/daily_planets_positions_long.csv"), append = F
  )
}

