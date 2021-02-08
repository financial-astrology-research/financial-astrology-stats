# Title     : Data tables load utilities.
# Objective : Ease the load of data tables.
# Created by: pablocc
# Created on: 08/02/2021

library(data.table)
library(memoise)

#' Load CSV data table.
#' @param pathFileName CSV file name including absolute or relative path.
#' @return A data table.
dataTableRead <- function(pathFileName) {
  dataTable <- fread(pathFileName)
  columnNames <- colnames(dataTable)
  # Set date as primary key when column exists.
  if ("Date" %in% columnNames) {
    setkey(dataTable, Date)
  }

  return(dataTable)
}

#' Memoized file read with memory cache (persist during current session).
memoFileRead <- memoise(dataTableRead)
