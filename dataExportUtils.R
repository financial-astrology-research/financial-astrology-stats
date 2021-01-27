# Title     : Utilities to export data tables to CSV files.
# Objective : Persist data tables into format that can be used by researchers.
# Created by: pablocc
# Created on: 27/01/2021

library(data.table)

source("./fileSystemUtilities.R")

#' Persist stats data table into target path and file destination.
#' @param symbolID Asset symbol ID the stats are calculated for.
#' @param dataTable The data table to persist.
#' @param targetFileName The destination file name without extension.
dataTableStatsExport <- function(dataTable, targetFileName) {
  targetFileName <- paste0(statsDataDestinationPath(), targetFileName, ".csv")
  fwrite(dataTable, targetFileName)
  cat("Stats table exported to:", targetFileName, "\n")
}