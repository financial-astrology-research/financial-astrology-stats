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
speedColNames <- colNames[grep("^..SP", colNames)]
declinationColNames <- colNames[grep("^..DEC", colNames)]
# Max items per data chunk per PineScript code line to avoid their code editor crash.
pineScriptStringLimit <- 2048
itemsPerChunk <- pineScriptStringLimit / 4

vectorChunkSplit <- function(x, nChunks) {
  split(x, cut(seq_along(x), nChunks, labels = FALSE))
}

positionsVariableDump <- function(fileHandler, varName, valuesChunks) {
  positionsPathFile <- paste0(astroDataDestinationPath(), 'pine-script-astro-positions.txt')
  variableDefinition <- paste0(varName, ' = array.from(')
  write(variableDefinition, fileHandler, append = T)
  for (index in names(valuesChunks)) {
    chunkContent <- paste0('\t ', str_flatten(valuesChunks[[index]], collapse = ','), ',')
    write(chunkContent, fileHandler, append = T)
  }
  write('\t )', fileHandler, append = T)
  cat(varName, "positions exported to", positionsPathFile, '\n')
}

openPineScriptAstroPositionsFile <- function() {
  positionsPathFile <- paste0(astroDataDestinationPath(), 'pine-script-astro-positions.txt')
  # Ensure the file is empty.
  cat("", positionsPathFile, append = F)
  file(positionsPathFile, "w")
}

closePineScriptAstroPositionsFile <- function(fileHandler) {
  close(fileHandler)
}

fileHandler <- openPineScriptAstroPositionsFile()
for (colName in c(longitudeColNames, speedColNames, declinationColNames)) {
  positions <- round(dailyPlanetPositions[[colName]], 2)
  nChunks <- ceiling(length(positions) / itemsPerChunk)
  positionsChunks <- vectorChunkSplit(positions, nChunks)
  positionsVariableDump(fileHandler, colName, positionsChunks)
}

closePineScriptAstroPositionsFile(fileHandler)