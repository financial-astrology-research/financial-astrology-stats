# Title     : Prepare transits aspects to asset natal chart.
# Objective : Support asset natal chart transits aspects data preparation for statistical analysis.
# Created by: pablocc
# Created on: 16/02/2021

library(data.table)
library(magrittr)
library(stringr)

source("./configUtils.R")
source("./dataLoadUtils.R")
source("./fileSystemUtilities.R")
source("./planetAspectsDataPrepare.R")

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
  normalAspectColumnNames <- paste0(
    substr(columnNamesParts[[1]], 1, 2),
    columnNamesParts[[2]],
    substr(columnNamesParts[[1]], 3, 6)
  )
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

#' Asset natal chart data.
natalChartDateGet <- function(symbol) {
  # open the stocks incorporation date planets positions
  natalfile <- paste0(astroDataDestinationPath(), 'assets_natal_charts.tsv')
  natalCharts <- fread(natalfile, sep = "\t", na.strings = "", verbose = F)
  natalChart <- natalCharts[Symbol == symbol]
  as.Date(natalChart$Date)
}

# Calculate transits to natal position (symbol incorporation chart) aspects.
buildNatalLongitudeAspects <- function(symbolID, dailyPlanetsPositions) {
  bornDate <- natalChartDateGet(symbolID)

  if (length(bornDate) == 0) {
    return(NULL)
  }

  cat("Calculating", symbolID, "natal transits aspects\n", sep = " ")
  # Prepare natal positions data table.
  natalPlanetPositions <- buildNatalLongitudes(symbolID)
  columnNames <- colnames(dailyPlanetsPositions)
  selectColumnNames <- columnNames[grep('..LON', columnNames)]
  # extract only the planets longitudes
  dailyPlanetsPositions <- dailyPlanetsPositions[, c('Date', 'Hour', selectColumnNames), with = F] %>%
    dataTableDateGreaterFilter(bornDate)
  # Cartesian join of natal and mundane positions.
  natalMundanePositions <- setkey(dailyPlanetsPositions[, c(k = 1, .SD)], k)[
    natalPlanetPositions[, c(k = 1, .SD)],
    allow.cartesian = T
  ]
  # Helper dummy index cleanup.
  natalMundanePositions[, k := NULL]

  natalMundanePositions <- dailyPlanetsPositions[,
    as.list(natalPlanetPositions),
    by = dailyPlanetsPositions
  ]

  # Calculate natal chart positions transits aspects.
  calculatePointsPlanetsAspects(natalMundanePositions)
}

assetsNatalChartTransitsPrepare <- function() {
  watchList <- assetsWatchList()
  for (symbolID in watchList$SymbolID) {
    symbolIdParts <- unlist(strsplit(symbolID, "-"))
    isCurrencyPair <- length(symbolIdParts) == 2
    natalSymbolId <- ifelse(isCurrencyPair, symbolIdParts[1], symbolID)
    natalAspectsWide <- loadPlanetsPositionTable() %>%
      buildNatalLongitudeAspects(natalSymbolId, .)

    # Process only when natal aspects ara available for this symbol.
    if (!is.null(natalAspectsWide)) {
      natalAspectsLong <- hourlyAspectsWideToLongTransform(natalAspectsWide)
      dailyNatalAspectsLong <- hourlyAspectsDateAggregate(natalAspectsLong)
      fwrite(
        dailyNatalAspectsLong,
        paste0(astroDataDestinationPath(), natalSymbolId, '_natal_transits.csv')
      )
    }
  }
}

natalChartFixedStarPositionsGet <- function(natalSymbolID) {
  bornDate <- natalChartDateGet(natalSymbolID)
  chineseZodiacFixedStarPositions <- chineseZodiacFixedStarPositionsLoad()
  natalFixedStarPositions <- chineseZodiacFixedStarPositions[Date == bornDate]

  if (nrow(natalFixedStarPositions) == 0) {
    return(NULL);
  }

  natalFixedStarPositions[, -c('Date')] %>% as.vector() %>% sort()
}

#' Prepare transit planets natal chart chinese mansions positions.
natalChartTransitsChineseMansionsPrepare <- function(symbolID) {
  planetPositionsTable <- loadPlanetsPositionTable()
  symbolIdParts <- unlist(strsplit(symbolID, "-"))
  isCurrencyPair <- length(symbolIdParts) == 2
  natalSymbolID <- ifelse(isCurrencyPair, symbolIdParts[1], symbolID)
  bornDate <- natalChartDateGet(natalSymbolID)
  natalFixedStarPositions <- natalChartFixedStarPositionsGet(natalSymbolID)
  colNames <- colnames(planetPositionsTable)
  longitudeColNames <- colNames[grep("^..LON", colNames)]
  planetLongitudeTableLong <- melt(
    planetPositionsTable,
    id.var = "Date",
    measure.var = longitudeColNames
  )

  # Aggregate horly to daily with average longitude.
  planetLongitudeTableLong <- planetLongitudeTableLong[, mean(value), by = c('Date', 'variable')]
  setnames(planetLongitudeTableLong, c('Date', 'pID', 'Lon'))
  planetLongitudeTableLong[, Lon := round(Lon, 2)]
  setkey(planetLongitudeTableLong, Date)

  if (!is.null(natalFixedStarPositions)) {
    zodiacMansionsCut <- c(0, as.numeric(natalFixedStarPositions), 360)
    zodiacMansionsNames <- names(natalFixedStarPositions)
    zodiacMansionsCutNames <- c(
      last(zodiacMansionsNames),
      zodiacMansionsNames
    )

    planetLongitudeTableLong[, CM := cut(Lon, zodiacMansionsCut, zodiacMansionsCutNames)]
    planetLongitudeTableLong[, pID := substr(pID, 1, 2)]

    targetPathFileName <- paste0(astroDataDestinationPath(), natalSymbolID, '_transits_positions.csv')
    fwrite(
      planetLongitudeTableLong[Date >= as.Date(bornDate)],
      targetPathFileName
    )

    cat("Exported transits positions to: ", targetPathFileName, "\n")
  }
}

assetsNatalChartTransitsPrepare()
natalChartTransitsChineseMansionsPrepare("BTC-USD")
