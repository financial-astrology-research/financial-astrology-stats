# Title     : File sytem utilities.
# Created by: pablocc
# Created on: 10/01/2021

expandPath <- function(path) {
  normalizePath(path.expand(path))
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
  if (!dir.exists(destinationPath)) {
    dir.create(destinationPath)
  }

  return(destinationPath)
}

#' Provides models predictions destination path.
#' @returns The models predictions data relative destination path.
modelsPredictionDestinationPath <- function() {
  destinationPath <- paste0(normalizePath('./machine_learning/predictions'), "/")
}

#' Provides models performance destination path.
#' @returns The models performance data relative destination path.
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
