# Title     : Export planets position longitude from 2010 to 2030 as PineScript string variables.
# Objective : Plug planets positions data into Trading View indicator.
# Created by: pablocc
# Created on: 28/05/2021

library(stringr)

source("./fileSystemUtilities.R")
source("./planetAspectsDataPrepare.R")

startDate <- as.Date("2010-01-01")
endDate <- as.Date("2030-12-31")
planetPositionsTable <- loadPlanetsPositionTable("hourly")
# Filter to day start hour observation.
dailyPlanetPositions <- planetPositionsTable[Hour == 0,]
# Filter positions observations from 2010 to 2030.
dailyPlanetPositions <- dailyPlanetPositions[Date >= startDate & Date <= endDate,]
colNames <- colnames(dailyPlanetPositions)
longitudeColNames <- colNames[grep("^..LON", colNames)]
# Max items per data chunk assuming positions need 3 numbers + 1 comma separator
# ever item can consume max of 4 bytes and PineScript strings has a bytes limit.
pineScriptStringLimit <- 4096
itemsPerChunk <- pineScriptStringLimit / 4

vectorChunkSplit <- function(x, nChunks) {
  split(x, cut(seq_along(x), nChunks, labels = FALSE))
}

positionsVariableDump <- function(fileHandler, varName, values) {
  dataString <- str_flatten(values, collapse = ',')
  variableDefinition <- paste0('string ', varName, ' = "', dataString, '"')
  write(variableDefinition, fileHandler, append = T)
  cat(varName, "positions exported to", positionsPathFile, "\n")
}

openPineScriptAstroPositionsFile <- function() {
  positionsPathFile <- paste0(astroDataDestinationPath(), '-pine-script-astro-positions.txt')
  # Ensure the file is empty.
  cat("", positionsPathFile, append = F)
  file(positionsPathFile, "w")
}

closePineScriptAstroPositionsFile <- function(fileHandler) {
  close(fileHandler)
}

fileHandler <- openPineScriptAstroPositionsFile()
for (colName in longitudeColNames) {
  positions <- round(dailyPlanetPositions[[colName]], 0)
  nChunks <- ceiling(length(positions) / itemsPerChunk)
  positionsChunks <- vectorChunkSplit(positions, nChunks)
  for (index in names(positionsChunks)) {
    varName <- paste0(colName, index)
    positionsVariableDump(fileHandler, varName, positionsChunks[[index]])
  }
}

closePineScriptAstroPositionsFile(fileHandler)