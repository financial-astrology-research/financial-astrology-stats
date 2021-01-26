# Title     : Calculate planet position price effect statistics for watchlist assets.
# Objective : Explore the effect of planet position by: sign polarity, element, triplicity
#             and zodiac sign.
# Created by: pablocc
# Created on: 26/01/2021

library(plyr)

source("./fileSystemUtilities.R")
source("./planetAspectsDataPrepare.R")

#' Augment planet positions data table with zodiacal sign and derivatives: polarity, triplicity, elements and so forth.
#' @param Planet longitude positions long data table.
#' @return Daily planets position table augmented with longitude derivatives.
longitudeDerivativesPositionTableAugment <- function(planetLongitudeTableLong) {
  zsignN <- seq(1, 12)
  zodiacSignName <- c(
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
  # Categorize longitude in zodiac signs: https://www.astro.com/astrowiki/en/Zodiac_Sign
  planetLongitudeTableLong[, znum := ceiling(lon / 30)]
  planetLongitudeTableLong[, zsign := mapvalues(znum, zsignN, zodiacSignName)]

  # Categorize signs in qualities: https://www.astro.com/astrowiki/en/Quality
  elementName <- rep(c('FIR', 'EAR', 'AIR', 'WAT'), 3)
  planetLongitudeTableLong[, element := mapvalues(znum, zsignN, elementName)]

  # Categorize signs in triplicities: https://www.astro.com/astrowiki/en/Triplicity
  triplicityName <- rep(c('CAR', 'FIX', 'MUT'), 4)
  planetLongitudeTableLong[, triplicity := mapvalues(znum, zsignN, triplicityName)]

  # Categorize signs in polarities: https://en.wikipedia.org/wiki/Polarity_(astrology)
  polarityName <- rep(c('POS', 'NEG'), 6)
  planetLongitudeTableLong[, polarity := mapvalues(znum, zsignN, polarityName)]

  # Remove zodiac sign number temporal variable.
  planetLongitudeTableLong[, znum := NULL]
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
  longitudeDerivativesPositionTableAugment(planetLongitudeTableLong)

  fwrite(
    planetLongitudeTableLong,
    expandPath("./data/daily_planets_positions_long.csv"), append = F
  )
}
