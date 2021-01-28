# Title     : Calculate planet position price effect statistics for watchlist assets.
# Objective : Explore the effect of planet position by: sign polarity, element, triplicity
#             and zodiac sign.
# Created by: pablocc
# Created on: 26/01/2021

library(plyr)

source("./fileSystemUtilities.R")
source("./planetAspectsDataPrepare.R")

#' Augment planet positions data table with categorical derivatives: polarity, triplicity, elements and so forth.
#' @param planetLongitudeTableLong Planet longitude positions long data table.
#' @return Daily planets position table augmented with categorical derivatives.
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

#' Augment planets speed data table with categorical derivatives: retrograde, stationary, direct.
#' @param planetSpeedTableLong Planet longitude positions long data table.
#' @return Daily planets speed table augmented with categorical derivatives.
speedDerivativesPositionTableAugment <- function(planetSpeedTableLong) {
  # Determine min speed when planet should be considered stationary based
  # on average boundary inspired by the formula found at
  # https://www.astro.com/astrowiki/en/Stationary_Phase
  planetSpeedBoundary <- planetSpeedTableLong[,
    list(
      Stationary = round(mean(speed) * 0.2, 2)
    ),
    by = "pID"
  ]

  stationaryBoundary <- matrix(
    planetSpeedBoundary$Stationary,
    nrow = 1,
    ncol = length(planetSpeedBoundary$Stationary),
    byrow = TRUE,
    dimnames = list('speed', planetSpeedBoundary$pID)
  )

  planetSpeedTableLong$speedmode <- "DIR"
  planetSpeedTableLong[speed < 0, speedmode := "RET"]
  planetSpeedTableLong[
    speed >= 0 & speed <= stationaryBoundary['speed', pID],
    speedmode := "STA"
  ]
}

#' Augment planet positions data table with position motion speed.
#' @return Daily planets speed table.
dailyPlanetsSpeedTablePrepare <- function() {
  cat("Preparing daily planets speed table.\n")
  planetPositionsTable <- loadPlanetsPositionTable("daily")
  colNames <- colnames(planetPositionsTable)
  speedColNames <- colNames[grep("^..SP", colNames)]
  planetSpeedTableLong <- melt(
    planetPositionsTable,
    id.var = "Date",
    measure.var = speedColNames
  )

  # Moon Nodes are imaginary points so we assume don't have own speed.
  planetSpeedTableLong[variable %in% c('NNSP', 'SNSP'), value := 0]
  # Extract planet ID from variable name.
  planetSpeedTableLong[, variable := substr(variable, 1, 2)]
  setnames(planetSpeedTableLong, c('Date', 'pID', 'speed'))
  speedDerivativesPositionTableAugment(planetSpeedTableLong)
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

  # Extract planet ID from variable name.
  planetLongitudeTableLong[, pID := substr(variable, 1, 2)]
  planetLongitudeTableLong[, variable := NULL]
  # Customize columns names.
  setnames(planetLongitudeTableLong, c('Date', 'lon', 'pID'))
  setcolorder(planetLongitudeTableLong, c('Date', 'pID', 'lon'))
  longitudeDerivativesPositionTableAugment(planetLongitudeTableLong)

  # Merge the planets speed columns.
  planetSpeedTableLong <- dailyPlanetsSpeedTablePrepare()
  planetPositionsTableLong <- merge(
    planetLongitudeTableLong,
    planetSpeedTableLong,
    by = c('Date', 'pID')
  )

  fwrite(
    planetPositionsTableLong,
    expandPath("./data/daily_planets_positions_long.csv"), append = F
  )
}