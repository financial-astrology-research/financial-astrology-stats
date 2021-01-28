# Title     : IDs abbreviations expand utilities.
# Objective : Centralize all elements IDs that are used in data tables to short column names
#             that are hard to interpret by contributors.
# Created by: pablocc
# Created on: 28/01/2021

library(plyr)

#' Definition of all planets IDs and it's corresponding human readable names.
planetsIdsDefinition <- function() {
  c(
    'Moon' = 'MO',
    'Mercury' = 'ME',
    'Venus' = 'VE',
    'Sun' = 'SU',
    'Mars' = 'MA',
    'Vesta' = 'VS',
    'Juno' = 'JN',
    'Ceres' = 'CE',
    'Pallas' = 'PA',
    'Juno' = 'JU',
    'North Node' = 'NN',
    'South Node' = 'SN',
    'Saturn' = 'SA',
    'Chiron' = 'CH',
    'Uranus' = 'UR',
    'Pholus' = 'PH',
    'Neptune' = 'NE',
    'Pluto' = 'PL'
  )
}

#' Definition of polarities and it's corresponding human readable names.
polarityIdsDefinition <- function() {
  c(
    'Positive' = 'POS',
    'Negative' = 'NEG'
  )
}

#' Definition of triplicity and it's corresponding human readable names.
triplicityIdsDefinition <- function() {
  c(
    'Cardinal' = 'CAR',
    'Fixed' = 'FIX',
    'Mutable' = 'MUT'
  )
}

#' Definition of element and it's corresponding human readable names.
elementIdsDefinition <- function() {
  c(
    'Fire' = 'FIR',
    'Earth' = 'EAR',
    'Air' = 'AIR',
    'Water' = 'WAT'
  )
}

#' Definition of zodiac sign and it's corresponding human readable names.
zodSignIdsDefinition <- function() {
  c(
    'Aries' = 'ARI',
    'Taurus' = 'TAU',
    'Gemini' = 'GEM',
    'Cancer' = 'CAN',
    'Leo' = 'LEO',
    'Virgo' = 'VIR',
    'Libra' = 'LIB',
    'Scorpio' = 'SCO',
    'Sagittarius' = 'SAG',
    'Capricorn' = 'CAP',
    'Aquarius' = 'AQU',
    'Pisces' = 'PIS'
  )
}

#' Map planet IDs vector to planet names.
#' @param ids Planet IDs vector.
#' @return Planet names vector.
planetIdToNameMap <- function(ids) {
  idsDefinition <- planetsIdsDefinition()
  useIds <- idsDefinition[idsDefinition %in% ids]
  mapvalues(ids, from = useIds, to = names(useIds))
}
