# Title     : Calculate mundane planets aspects from planets positions.
# Objective : Prepare the planets aspects data in tabular form for specific angular aspects and orbs.
# Created by: pablocc
# Created on: 08/01/2021

library(data.table)

source("./aspectSets.R")
source("./planetSets.R")

expandPath <- function(path) {
  normalizePath(path.expand(path))
}

#' Normalize planets longitude distance to 180 degrees limit.
degreesDistanceNormalize <- function(x) {
  x[x > 180] <- abs(x[x > 180] - 360)
  x[x < -180] <- abs(x[x < -180] + 360)
  abs(x)
}

composePlanetColNameCombine <- function(planetsIds, colNameSuffix) {
  planetsComb <- combn(planetsIds, 2, simplify = F)
  as.character(lapply(planetsComb, function(x) paste(x[1], x[2], colNameSuffix, sep = '')))
}

#' Generate all combined planets longitude column names.
#' For example, combining Moon (MO) and Sun (SU) result in MOSULON column name.
#' @param planetsIds Planets IDs vector.
planetsLongitudeColNamesCombine <- function(planetsIds) {
  composePlanetColNameCombine(planetsIds, "LON")
}

#' Generate all combined planets aspect column names.
#' For example, combining Moon (MO) and Sun (SU) result in MOSUASP column name.
#' @param planetsIds Planets IDs vector.
planetsAspectColNamesCombine <- function(planetsIds) {
  composePlanetColNameCombine(planetsIds, "ASP")
}

#' Generate all combined planets orb column names.
#' For example, combining Moon (MO) and Sun (SU) result in MOSUORB column name.
#' @param planetsIds Planets IDs vector.
planetsOrbColNamesCombine <- function(planetsIds) {
  composePlanetColNameCombine(planetsIds, "ORB")
}

#' Load planets position data table with indicated resolution.
#' @param resolution Positions time resolution: "daily" or "hourly", defaults to the later.
#' @return The planets position data table.
loadPlanetsPositionTable <- function(resolution = "hourly") {
  if (resolution == "daily") {
    planetsDataFile <- expandPath(paste("./data/planets_position_daily_1930-2029.tsv", sep = ""))
  }
  else {
    planetsDataFile <- expandPath(paste("./data/planets_position_hourly_1980-2000.tsv", sep = ""))
  }

  planetsPositionsTable <- fread(planetsDataFile, sep = "\t", na.strings = "", verbose = F)
  # Normalize date and set year and weekday columns.
  planetsPositionsTable[, Date := as.Date(Date, format = "%Y-%m-%d")]
  planetsPositionsTable[, Year := as.character(format(Date, "%Y"))]
  planetsPositionsTable[, wday := format(Date, "%w")]
  setkey(planetsPositionsTable, 'Date')

  return(planetsPositionsTable)
}

#' Categorize the longitue distance between two planets as astrological angular aspect.
#' @param x Longitude distance vector.
#' @param orbsMatrix Aspects orbs matrix.
#' @return A vector of the continuous longitude distance mapped to aspect categories.
longitudeDistanceAspectCategorize <- function(x, orbsMatrix) {
  allidx <- rep(FALSE, length(x))
  aspects <- as.numeric(colnames(orbsMatrix))
  for (aspect in aspects) {
    comborb <- orbsMatrix['orbs', as.character(aspect)]
    rstart <- aspect - comborb
    rend <- aspect + comborb
    idx <- x >= rstart & x <= rend
    x[idx] <- aspect
    allidx[idx] <- TRUE
  }

  # Set NA when no aspects mapped.
  x[!allidx] <- NA
  return(x)
}

#' Calculate the aspect orb (distance from exact angle) between two planets angular aspect.
#' @param x Longitude distance vector.
#' @param orbsMatrix Aspects orbs matrix.
#' @return A vector of the continuous longitude distance mapped to aspect categories.
longitudeDistanceAspectOrbCalculate <- function(x, orbsMatrix) {
  allidx <- rep(FALSE, length(x))
  aspects <- as.numeric(colnames(orbsMatrix))
  for (aspect in aspects) {
    comborb <- orbsMatrix['orbs', as.character(aspect)]
    rstart <- aspect - comborb
    rend <- aspect + comborb
    idx <- x >= rstart & x <= rend
    x[idx] <- abs(x[idx] - aspect)
    allidx[idx] <- TRUE
  }

  # Set NA when no aspects mapped.
  x[!allidx] <- NA
  return(x)
}

#' Calculate planets aspects within desired aspect type orb.
#' @param planetsPositions Planets positions data table.
#' @param usePlanets Planets IDs to compute angular aspects for it's longitudes.
#' @param aspectSet Aspects set with aspect / orbs properties to compute.
#' @return Planets data table augmented with aspects and orbs planets combination columns.
planetAspectsCalculate <- function(planetsPositions, usePlanets, aspectSet) {
  # Clone to avoid original table is not modified.
  planetsPositionsClone <- copy(planetsPositions)
  planetsCombLonCols <- planetsLongitudeColNamesCombine(usePlanets)
  planetsCombAspCols <- planetsAspectColNamesCombine(usePlanets)
  planetsCombOrbCols <- planetsOrbColNamesCombine(usePlanets)

  orbsMatrix <- matrix(
    aspectSet$orbs,
    nrow = 1,
    ncol = length(aspectSet$aspects),
    byrow = TRUE,
    dimnames = list('orbs', aspectSet$aspects)
  )

  planetsPositionsClone[,
    c(planetsCombAspCols) :=
      lapply(.SD, longitudeDistanceAspectCategorize, orbs = orbsMatrix), .SDcols = planetsCombLonCols
  ]

  planetsPositionsClone[,
    c(planetsCombOrbCols) :=
      lapply(.SD, longitudeDistanceAspectOrbCalculate, orbs = orbsMatrix), .SDcols = planetsCombLonCols
  ]

  return(planetsPositionsClone)
}

#' Calculate specific planet angles aspects for a given resolution.
#' @param usePlanets The list of planets ID codes to calculate aspects for.
#' @param resolution The row resolution of the aspects: "hourly" or "daily".
#' @param aspectSet Aspects set list, that defines "aspect" and "orbs" properties.
#' @return A data table with combined planet code columns with the angular aspects.
planetAspectsTablePrepare <- function(resolution, usePlanets, aspectSet) {
  planets <- loadPlanetsPositionTable(resolution)
  colNames <- colnames(planets)
  filterColNames <- colNames[grep(paste0(usePlanets, collapse = "|"), colNames)]
  planetsCombLonCols <- planetsLongitudeColNamesCombine(usePlanets)
  selectCols <- c('Date', 'Hour', filterColNames)

  planets <- planets[, selectCols, with = F]
  # Calculate planets combinations longitudinal differences.
  for (currentComb in planetsCombLonCols) {
    col1 <- paste(substr(currentComb, 1, 2), 'LON', sep = '')
    col2 <- paste(substr(currentComb, 3, 4), 'LON', sep = '')
    planets[, c(currentComb) := get(col1) - get(col2)]
  }

  # Normalize to 180 degrees range.
  planets[, c(planetsCombLonCols) := lapply(.SD, degreesDistanceNormalize), .SDcols = planetsCombLonCols]

  # Calculate aspects within specified orb.
  planets <- planetAspectsCalculate(
    planetsPositions = planets,
    usePlanets = usePlanets,
    aspectSet = aspectSet
  )

  #planets[, c(planetsSpCols) := lapply(.SD, function(x) scales::rescale(x, to = c(0, 1))), .SDcols = planetsSpCols]
  #planets[, c(planetsDecCols) := lapply(.SD, function(x) scales::rescale(x, to = c(0, 1))), .SDcols = planetsDecCols]

  return(planets)
}

#' Append aspects orbs column data to planet aspects table.
#' @param planetAspectsLong Planets aspects long table (one planet combination aspect per row).
#' @param planetAspectsWide Planets aspects wide table with all planets/aspects data columns.
#' @param idCols Columns IDs to use for aspects orb table merge.
#' @return Planets aspects long table augmented with aspects orb cols.
aspectsOrbColumnsAppend <- function(planetAspectsLong, planetAspectsWide, idCols = c('Date')) {
  colNames <- colnames(planetAspectsWide)
  orbColNames <- colNames[grep("^....ORB$", colNames)]
  planetAspectsOrbs <- melt(
    planetAspectsWide, id.var = idCols, variable.name = 'origin',
    value.name = 'orb', measure.var = orbColNames
  )

  planetAspectsOrbs[, orb := round(orb, 2)]
  planetAspectsOrbs[, origin := substr(origin, 1, 4)]
  merge(planetAspectsLong, planetAspectsOrbs, by = c(idCols, 'origin'))
}

#' Convert hourly aspects wide table into long format.
#' @param hourlyPlanetAspectsWide Planets hourly position / aspects / orb data table.
#' @return Long format aspects data table.
hourlyAspectsWideToLongTransform <- function(hourlyPlanetAspectsWide) {
  idCols <- c('Date', 'Hour')
  colNames <- colnames(hourlyPlanetAspectsWide)
  aspectColNames <- colNames[grep("^....ASP$", colNames)]
  hourlyPlanetAspectsLong <- melt(
    hourlyPlanetAspectsWide, id.var = idCols,
    variable.name = 'origin', value.name = 'aspect',
    value.factor = T, measure.var = aspectColNames, na.rm = T
  )

  setkey(hourlyPlanetAspectsLong, 'Date', 'Hour')
  hourlyPlanetAspectsLong[, origin := substr(origin, 1, 4)]
  planetAspectsLongDataAugment(hourlyPlanetAspectsLong, hourlyPlanetAspectsWide, idCols)
}

#' Augment planet aspects rows with additional aspects / planets data: orb, speed, declination, etc.
#' @param planetAspectsLong Planets aspects long table (one planet combination aspect per row).
#' @param planetAspectsWide Planets aspects wide table (one planet combination per column).
#' @param mergeCols The time period merge cols used to merge.
#' @return Planets aspects wide table augmented with all astrological relevant data: orbs, speed, declination, etc.
planetAspectsLongDataAugment <- function(planetAspectsLong, planetAspectsWide, mergeCols) {
  aspectsOrbColumnsAppend(planetAspectsLong, planetAspectsWide, mergeCols)
}

#' Daily aggregate hourly resolution planet aspects.
#' @param hourlyPlanetAspectsLong Planets aspects long table (one planet combination aspect per row).
hourlyAspectsDateAggregate <- function(hourlyPlanetAspectsLong) {
  # Keep min orb for the aggrecation.
  dailyPlanetAspectsLong <- hourlyPlanetAspectsLong[,
    min(orb), by = list(Date, origin, aspect)
   ]

  setnames(dailyPlanetAspectsLong, c('Date', 'origin', 'aspect', 'orb'))
}

modernPlanetsPabloAspectsDailyAspectsTable <- function() {
  modernPlanetsSet <- modernPlanets()
  aspectSet <- pabloCerdaAspectSet()
  planetAspectsWideTable <- planetAspectsTablePrepare(
    resolution = "hourly",
    usePlanets = modernPlanetsSet,
    aspectSet = aspectSet
  )

  hourlyPlanetAspectsLong <- hourlyAspectsWideToLongTransform(planetAspectsWideTable)
  hourlyAspectsDateAggregate(hourlyPlanetAspectsLong)
}

planetAspectsWide <- modernPlanetsPabloAspectsDailyAspectsTable()