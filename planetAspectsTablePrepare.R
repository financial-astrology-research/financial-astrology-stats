# Title     : Calculate mundane planets aspects from planets positions.
# Objective : Prepare the planets aspects data in tabular form for specific angular aspects and orbs.
# Created by: pablocc
# Created on: 08/01/2021

#' Load planets position data table with indicated resolution.
#' @param resolution Positions time resolution: "daily" or "hourly", defaults to the later.
#' @return The planets position data table.
loadPlanetsPositionTable <- function(resolution = "hourly") {
  if (resolution == "daily") {
    planetsDataFile <- npath(paste("./data/planets_position_daily_1930-2029.tsv", sep = ""))
  }
  else {
    planetsDataFile <- npath(paste("./data/planets_position_hourly_1980-2029.tsv", sep = ""))
  }

  planetsPositionsTable <- fread(planetsDataFile, sep = "\t", na.strings = "", verbose = F)

  # Normalize date and set year and weekday columns.
  planets[, Date := as.Date(planets$Date, format = "%Y-%m-%d")]
  planets[, Year := as.character(format(Date, "%Y"))]
  planets[, wday := format(Date, "%w")]
  setkey(planets, 'Date')

  return(planetsPositionsTable)
}

#' Calculate specific planet angles aspects for a given resolution.
#' @param selectPlanets The list of planets ID codes to calculate aspects for.
#' @param resolution The row resolution of the aspects: "hourly" or "daily".
#' @param aspects Aspects angles vector needs to be calculated.
#' @param cusorbs Aspects orbs vector in the same order as the aspects to compute.
#' @return A data table with combined planet code columns with the angular aspects.
planetsAspectsTablePrepare <- function(resolution, selectPlanets, cusorbs) {
  planets <- loadPlanetsPositionTable(resolution)
  planets <- planets[, selectPlanets, with = F]
  # Calculate planets combinations longitudinal differences.
  for (curcol in planetsCombLon) {
    col1 <- paste(substr(curcol, 1, 2), 'LON', sep = '')
    col2 <- paste(substr(curcol, 3, 4), 'LON', sep = '')
    planets[, c(curcol) := get(col1) - get(col2)]
  }

  # Normalize to 180 degrees range.
  planets[, c(planetsCombLon) := lapply(.SD, normalizeDistance), .SDcols = planetsCombLon]

  # Calculate aspects within specified orb.
  orbsmatrix <- matrix(cusorbs, nrow = 1, ncol = length(aspects), byrow = TRUE, dimnames = list('orbs', aspects))
  planets <- processPlanetsAspects(planets, orbsmatrix)

  #planets[, c(planetsSpCols) := lapply(.SD, function(x) scales::rescale(x, to = c(0, 1))), .SDcols = planetsSpCols]
  #planets[, c(planetsDecCols) := lapply(.SD, function(x) scales::rescale(x, to = c(0, 1))), .SDcols = planetsDecCols]

  return(planets)
}

dailyPlanets <- planetsAspectsTablePrepare(
  resolution = "hourly",
  selectPlanets = c("MO", "SU")
)
