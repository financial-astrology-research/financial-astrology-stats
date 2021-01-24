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
#' @returns The stats data relative destination path.
statsDataDestinationPath <- function() {
  destinationPath <- "./stats/"
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
