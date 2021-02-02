# Title     : Prepare daily moon phase and position CSV data table.
# Objective : Provide the daily location of last moon phase (new, full) and event position location.
# Created by: pablocc
# Created on: 02/02/2021

library(data.table)
library(plyr)

source("./fileSystemUtilities.R")
source("./idsExpandUtils.R")
source("./planetAspectsDataPrepare.R")

#' Prepare daily moon phase data table.
dailyMoonPhaseTablePrepare <- function() {
  cat("Preparing daily moon phases and position table.\n")
  planetPositionsTable <- loadPlanetsPositionTable()
  planetPositionsTable <- planetLongitudesDistanceDataAugment(planetPositionsTable, c('MO', 'SU'))
  dailyMoonSunPositionsTable <- planetPositionsTable[,
    list(
      MOLON = mean(MOLON),
      SULON = mean(SULON),
      DISMIN = min(MOSULON),
      DISMAX = max(MOSULON)
    ),
    by = "Date"
  ]

  # Remove factional part.
  dailyMoonSunPositionsTable[, MOLON := round(MOLON)]
  dailyMoonSunPositionsTable[, SULON := round(SULON)]
  dailyMoonSunPositionsTable[, DISMIN := round(DISMIN)]
  dailyMoonSunPositionsTable[, DISMAX := round(DISMAX)]

  # Categorize the moon phases.
  dailyMoonSunPositionsTable[DISMIN <= 1, MoonPhase := 'N']
  dailyMoonSunPositionsTable[DISMAX >= 179, MoonPhase := 'F']

  # Calculate moon phase sign position.
  zodiacSignName <- zodSignIdsDefinition()
  dailyMoonSunPositionsTable[MOLON == 0, MOLON := 0.1]
  # TODO: using daily data could cause that sign of the exact phase event is different than mean lon.
  dailyMoonSunPositionsTable[!is.na(MoonPhase), ZodSignN := ceiling(MOLON / 30)]

  # Fill backward NAs with last moon phase occurrence.
  dailyMoonSunPositionsTable[,
    MoonPhase := MoonPhase[
      nafill(replace(.I, is.na(MoonPhase), NA), "locf")
    ]
  ]

  # Fill backward zod sign with last moon phase occurrence.
  dailyMoonSunPositionsTable[,
    ZodSignN := ZodSignN[
      nafill(replace(.I, is.na(ZodSignN), NA), "locf")
    ]
  ]

  dailyMoonSunPositionsTable[, ZodSignID := mapvalues(ZodSignN, seq(1, 12), zodiacSignName)]

  fwrite(
    dailyMoonSunPositionsTable,
    expandPath("./data/daily_moon_phase_positions.csv"), append = F
  )
}
