# Title     : File sytem utilities.
# Created by: pablocc
# Created on: 10/01/2021

library(magrittr)

expandPath <- function(path) {
  normalizePath(path.expand(path))
}

#' Create destination path directories when not exist.
#' @param destinationPath Absolute or relative destination path.
#' @return The destination path.
destinationPathCreate <- function(destinationPath) {
  if (!dir.exists(destinationPath)) {
    dir.create(destinationPath, recursive = T)
  }

  return(destinationPath)
}

#' Provide visualizations data destination path.
#' @returns The stats data relative destination path.
visualizationsDataDestinationPath <- function() {
  destinationPath <- "./visualizations/"
}

#' Provide stats data destination path.
#' @param symbolID Asset symbol ID.
#' @returns The stats data relative destination path.
statsDataDestinationPath <- function(symbolID) {
  destinationPath <- paste0("./stats/", symbolID, "/")
  destinationPathCreate(destinationPath)
}

#' Provides models predictions destination path.
#' @returns The models predictions relative destination path.
modelsPredictionDestinationPath <- function() {
  destinationPath <- paste0(normalizePath('./machine_learning/predictions'), "/")
}

#' Provides top performing models predictions signals index destination path.
#' @returns The models predictions index relative destination path.
modelsSignalsIndexDestinationPath <- function() {
  destinationPath <- paste0(normalizePath('./machine_learning/signals_index'), "/")
}

#' Provides models performance destination path.
#' @returns The models performance relative destination path.
modelsPerformanceDestinationPath <- function() {
  destinationPath <- paste0(normalizePath('./machine_learning/performance'), "/")
}

#' Provides assets data destination path.
#' @returns The assets data relative destination path.
assetsDataDestinationPath <- function() {
  destinationPath <- paste0(normalizePath('./data/tmp'), "/")
}

#' Provides astro data destination path.
#' @returns The astro data relative destination path.
astroDataDestinationPath <- function() {
  destinationPath <- paste0(normalizePath('./data'), "/")
}

#' Provide mundane events reports destination path.
#' @param symbolID Asset symbol ID.
#' @param reportDate Events report date used to group reports within year/month directory.
#' @returns The stats data relative destination path.
mundaneEventsDestinationPath <- function(symbolID, reportDate) {
  yearMonth <- format(reportDate, "%Y_%b")
  destinationPath <- paste0("./mundane_events/", symbolID, "/", yearMonth, "/")
  destinationPathCreate(destinationPath)
}

#' Provides machine learning predictions destination path.
#' @returns The machine learning predictions relative destination path.
modelsPredictionsDestinationPath <- function() {
  destinationPath <- paste0(normalizePath('./machine_learning/predictions'), "/")
}

#' Provides machine learning performance reports destination path.
#' @returns The machine learning performance reports relative destination path.
modelsPerformanceDestinationPath <- function() {
  destinationPath <- paste0(normalizePath('./machine_learning/performance'), "/")
}

#' Provides latest machine learning performance report path / filename.
#' @returns The report path and filename.
modelsLatestPerformancePathFileNameGet <- function() {
  list.files(modelsPerformanceDestinationPath(), pattern = "*.csv") %>%
    sort(decreasing = T) %>%
    head(n = 1) %>%
    paste0(modelsPerformanceDestinationPath(), .)
}
