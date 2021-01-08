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
  planetsComb <- combn(planetsIds, 2, simplify=F)
  as.character(lapply(planetsComb, function(x) paste(x[1], x[2], colNameSuffix, sep='')))
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

#' Calculate planets aspects within desired aspect type orb.
#' @param planetsPositions Planets positions data table.
#' @param usePlanets Planets IDs to compute angular aspects for it's longitudes.
#' @param orbs Aspects planets orb (degrees tolerance to exact angle).
#' @return Planets data table augmented with aspects and orbs planets combination columns.
planetsAspectsCalculate <- function(planetsPositions, usePlanets, orbs) {
  # Clone to avoid original table is not modified.
  planetsPositionsClone <- copy(planetsPositions)
  planetsCombAspCols <- planetsAspectColNamesCombine(usePlanets)
  planetsCombOrbCols <- planetsOrbColNamesCombine(usePlanets)

  planetsPositionsClone[,
     c(planetsCombAspCols) := lapply(.SD, calculateAspects, orbs = orbs), .SDcols = planetsCombAspCols
  ]

  planetsPositionsClone[,
    c(planetsCombOrbCols) := lapply(.SD, calculateAspectOrbs, orbs = orbs), .SDcols = planetsCombOrbCols
  ]

  return(planetsPositionsClone)
}

#' Calculate specific planet angles aspects for a given resolution.
#' @param usePlanets The list of planets ID codes to calculate aspects for.
#' @param resolution The row resolution of the aspects: "hourly" or "daily".
#' @param aspects Aspects angles vector needs to be calculated.
#' @param cusorbs Aspects orbs (tolerance to exact angle formation) vector in same order as the aspects.
#' @return A data table with combined planet code columns with the angular aspects.
planetsAspectsTablePrepare <- function(resolution, usePlanets, aspects, orbs) {
  planets <- loadPlanetsPositionTable(resolution)
  colNames <- colnames(planets)
  selectCols <- colNames[grep(paste0(usePlanets, collapse = "|"), colNames)]
  planetsCombLonCols <- planetsLongitudeColNamesCombine(usePlanets)

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
  orbsmatrix <- matrix(orbs, nrow = 1, ncol = length(aspects), byrow = TRUE, dimnames = list('orbs', aspects))
  planets <- planetsAspectsCalculate(planets, orbsmatrix)

  #planets[, c(planetsSpCols) := lapply(.SD, function(x) scales::rescale(x, to = c(0, 1))), .SDcols = planetsSpCols]
  #planets[, c(planetsDecCols) := lapply(.SD, function(x) scales::rescale(x, to = c(0, 1))), .SDcols = planetsDecCols]

  return(planets)
}

modernPlanetsSet <- modernPlanets()
aspectsSet <- pabloCerdaAspectSet()
dailyPlanets <- planetsAspectsTablePrepare(
  resolution = "hourly",
  usePlanets = modernPlanetsSet,
  aspects = aspectsSet$aspects,
  orbs = aspectsSet$orbs
)
