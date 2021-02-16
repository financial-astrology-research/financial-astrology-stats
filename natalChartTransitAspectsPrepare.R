# Title     : Prepare transits aspects to asset natal chart.
# Objective : Support asset natal chart transits aspects data preparation for statistical analysis.
# Created by: pablocc
# Created on: 16/02/2021

library(data.table)
library(magrittr)
library(stringr)

source("./planetAspectsDataPrepare.R")
source("./fileSystemUtilities.R")

calculatePointsPlanetsAspects <- function(longitudePoints) {
  aspectsSet <- pabloCerdaAspectSet()
  columnNames <- colnames(longitudePoints)
  lonColumnNames <- columnNames[grep('^..LON$', columnNames)]

  # Build column names for distance, aspect and orbs.
  planetsLonDisCols <- str_replace(lonColumnNames, 'LON', 'DIS')
  planetsLonAspCols <- str_replace(lonColumnNames, 'LON', 'ASP')
  planetsLonOrbCols <- str_replace(lonColumnNames, 'LON', 'ORB')

  # Calculate lon / planets distance
  for (curcol in planetsLonDisCols) {
    planetcol <- paste0(substr(curcol, 1, 2), 'LON')
    longitudePoints[, c(curcol) := lon - get(planetcol)]
  }

  # Normalize to 180 degrees range
  longitudePoints[,
    c(planetsLonDisCols) := lapply(.SD, degreesDistanceNormalize), .SDcols = planetsLonDisCols
  ]

  # Aspects orbs matrix.
  orbsMatrix <- matrix(
    aspectsSet$orbs,
    nrow = 1,
    ncol = length(aspectsSet$orbs),
    byrow = TRUE,
    dimnames = list('orbs', aspectsSet$aspects)
  )

  longitudePoints[,
    c(planetsLonAspCols) :=
      lapply(.SD, longitudeDistanceAspectCategorize, orbs = orbsMatrix), .SDcols = planetsLonDisCols
  ]

  longitudePoints[,
    c(planetsLonOrbCols) :=
      lapply(.SD, longitudeDistanceAspectOrbCalculate, orbs = orbsMatrix), .SDcols = planetsLonDisCols
  ]

  # Format wide all the significant point points.planets
  longitudePoints[, variable := substr(variable, 1, 2)]
  natalPointsAspects <- dcast(
    longitudePoints,
    Date + Hour ~ variable,
    value.var = c(planetsLonAspCols, planetsLonOrbCols),
    fill = NA
  )

  # Normalize aspect names with same format as mundane aspects.
  columnNames <- colnames(natalPointsAspects)
  aspectColumnNames <- columnNames[grep("_", columnNames)]
  columnNamesParts <- tstrsplit(aspectColumnNames, "_", fixed = T)
  normalAspectColumnNames <- paste0(columnNamesParts[[2]], columnNamesParts[[1]])
  setnames(natalPointsAspects, c('Date', 'Hour', normalAspectColumnNames))

  return(natalPointsAspects)
}

#' Prepare the natal positions table for a given symbol.
buildNatalLongitudes <- function(symbol) {
  # open the stocks incorporation date planets positions
  natalfile <- paste0(astroDataDestinationPath(), 'assets_natal_charts.tsv')
  natal <- fread(natalfile, sep = "\t", na.strings = "", verbose = F)
  loncols <- colnames(natal)
  loncols <- loncols[grep('^..LON$', loncols)]
  natal.long <- melt(natal, id.var = c('Symbol'), measure.var = loncols)
  natal.symbol <- natal.long[Symbol == symbol,]
  setnames(natal.symbol, c('Symbol', 'variable', 'lon'))

  return(natal.symbol)
}

# Calculate transits to natal position (symbol incorporation chart) aspects.
buildNatalLongitudeAspects <- function(symbol, dailyPlanetsPositions) {
  # build natal positions
  natalPlanetPositions <- buildNatalLongitudes(symbol)
  columnNames <- colnames(dailyPlanetsPositions)
  selectColumnNames <- columnNames[grep('..LON', columnNames)]
  # extract only the planets longitudes
  dailyPlanetsPositions <- dailyPlanetsPositions[, c('Date', 'Hour', selectColumnNames), with = F]
  # Cartesian join of natal and mundane positions.
  natalMundanePositions <- dailyPlanetsPositions[,
    as.list(natalPlanetPositions),
    by = dailyPlanetsPositions
  ]
  # Calculate natal chart positions transits aspects.
  calculatePointsPlanetsAspects(natalMundanePositions)
}

natalAspectsWide <- loadPlanetsPositionTable() %>%
  buildNatalLongitudeAspects('BTC', .)
natalAspectsLong <- hourlyAspectsWideToLongTransform(natalAspectsWide)
dailyNatalAspectsLong <- hourlyAspectsDateAggregate(hourlyPlanetAspectsLong)
