# Title     : Prepare daily planet position CSV data table.
# Objective : Provide planet position data by: zodiac sign, polarity, element, triplicity,
#             decans, moon mansions, among others.
# Created by: pablocc
# Created on: 26/01/2021

library(data.table)
library(plyr)
library(tidyr)

source("./fileSystemUtilities.R")
source("./planetAspectsDataPrepare.R")

#' Augment planet positions data table with categorical derivatives: polarity, triplicity, elements and so forth.
#' @param planetLongitudeTableLong Planet longitude positions long data table.
#' @return Daily planets position table augmented with categorical derivatives.
longitudeDerivativesPositionTableAugment <- function(planetLongitudeTableLong) {
  zodSignIdx <- seq(1, 12)
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
  planetLongitudeTableLong[Lon == 0, Lon := 0.1]
  # Categorize longitude in zodiac signs: https://www.astro.com/astrowiki/en/Zodiac_Sign
  planetLongitudeTableLong[, ZodSignN := ceiling(Lon / 30)]
  planetLongitudeTableLong[, ZodSignID := mapvalues(ZodSignN, zodSignIdx, zodiacSignName)]

  # Categorize signs in qualities: https://www.astro.com/astrowiki/en/Quality
  elementName <- rep(c('FIR', 'EAR', 'AIR', 'WAT'), 3)
  planetLongitudeTableLong[, ElementID := mapvalues(ZodSignN, zodSignIdx, elementName)]

  # Categorize signs in triplicities: https://www.astro.com/astrowiki/en/TriplicityID
  triplicityName <- rep(c('CAR', 'FIX', 'MUT'), 4)
  planetLongitudeTableLong[, TriplicityID := mapvalues(ZodSignN, zodSignIdx, triplicityName)]

  # Categorize signs in polarities: https://en.wikipedia.org/wiki/Polarity_(astrology)
  polarityName <- rep(c('POS', 'NEG'), 6)
  planetLongitudeTableLong[, PolarityID := mapvalues(ZodSignN, zodSignIdx, polarityName)]

  # Categorize longitude in decans: https://www.astro.com/astrowiki/en/DecanID
  decansLonCut <- seq(0, 360, by = 10)
  zodSignDecanIDGrid <- expand.grid(seq(1, 3), zodiacSignName)
  zodSignDecanIDNames <- paste0(zodSignDecanIDGrid$Var1, zodSignDecanIDGrid$Var2)
  planetLongitudeTableLong[, DecanID := cut(Lon, decansLonCut, zodSignDecanIDNames)]
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
      Stationary = round(mean(Speed) * 0.2, 2)
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

  planetSpeedTableLong$SpeedPhaseID <- "DIR"
  planetSpeedTableLong[Speed < 0, SpeedPhaseID := "RET"]
  planetSpeedTableLong[
    Speed >= 0 & Speed <= stationaryBoundary['speed', pID],
    SpeedPhaseID := "STA"
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

  # Moon Nodes are imaginary points so we assume constant speed.
  planetSpeedTableLong[variable %in% c('NNSP', 'SNSP'), value := 1]
  # Extract planet ID from variable name.
  planetSpeedTableLong[, variable := substr(variable, 1, 2)]
  setnames(planetSpeedTableLong, c('Date', 'pID', 'Speed'))
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
  setnames(planetLongitudeTableLong, c('Date', 'Lon', 'pID'))
  setcolorder(planetLongitudeTableLong, c('Date', 'pID', 'Lon'))
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
