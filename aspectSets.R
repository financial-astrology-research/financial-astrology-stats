# Title     : Define different aspects sets.
# Objective : Astrologers have constant debate on what angular aspects should be used to forecast
#             so research for different astrologers suggested aspects sets is needed to evaluate validity.
# Created by: pablocc
# Created on: 08/01/2021

#' All possible aspects suggested by modern astrologers set.
#' @return A list of aspects and orbs vectors.
allMajorAndMinorAspectsSet <- function() {
  return(
    list(
      aspects = c(0, 30, 36, 40, 45, 51, 60, 72, 80, 90, 103, 108, 120, 135, 144, 150, 154, 160, 180),
      orbs = c(12, 2, 2, 2, 2, 2, 7, 2, 2, 7, 2, 2, 7, 2, 2, 2, 2, 2, 12)
    )
  )
}

#' Reduced modern astrologers aspects set excluding less common angles that overlap in close orb with common aspects.
#' @return A list of aspects and orbs vectors.
allMajorAndMinorAspectsSet2 <- function() {
  return(
    list(
      aspects = c(0, 30, 45, 52, 60, 72, 90, 103, 120, 135, 144, 150, 180),
      orbs = c(10, 3, 3, 3, 6, 3, 10, 3, 6, 3, 3, 6, 10)
    )
  )
}

#' Classic astrologers aspects that tends to have a good consensus from traditional astrology.
#' @return A list of aspects and orbs vectors.
classicAspectsSet <- function() {
  return(
    list(
      aspects = c(0, 30, 45, 60, 90, 120, 135, 150, 180),
      orbs = c(10, 4, 4, 6, 10, 6, 4, 6, 10)
    )
  )
}

#' Same as classic aspects set with equal aspects orb of 5 degrees.
#' @return A list of aspects and orbs vectors.
classicAspectsSet2 <- function() {
  return(
    list(
      aspects = c(0, 30, 45, 60, 90, 120, 135, 150, 180),
      orbs = c(5, 5, 5, 5, 5, 5, 5, 5, 5)
    )
  )
}

#' Major angular aspects that are generally reported by astrologers to show strongest effect.
#' @return A list of aspects and orbs vectors.
setMajorAspectsSet <- function() {
  return(
    list(
      aspects = c(0, 60, 90, 120, 150, 180),
      orbs = c(5, 5, 5, 5, 5, 5)
    )
  )
}

#' This are the aspects that personally found relevant for financial forecasting
#' the orb effect was mixed for different researched markets and conjunction / opposition
#' seems to start 1 degree sooner than other aspects, although major effect was observed
#' within 2 degrees exact orb and in many experimentes reduced orb to 2 degrees.
#' @return A list of aspects and orbs vectors.
pabloCerdaAspectSet <- function() {
  return(
    list(
      aspects = c(0, 30, 36, 45, 60, 72, 90, 103, 120, 135, 144, 150, 180),
      orbs = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    )
  )
}