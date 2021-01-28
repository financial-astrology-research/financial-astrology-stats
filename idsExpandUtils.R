# Title     : IDs abbreviations expand utilities.
# Objective : Centralize all elements IDs that are used in data tables to short column names
#             that are hard to interpret by contributors.
# Created by: pablocc
# Created on: 28/01/2021

library(plyr)

#' Definition of all planets IDs and it's corresponding human readable names.
planetIdsDefinition <- function() {
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
    'Pluto' = 'PL',
    'Sun Eclipse' = "ES",
    'Moon Eclipse' = 'EM',
  )
}

#' Definition of aspects and it's corresponding human readable names.
aspectIdsDefinition <- function() {
  c(
    'Conjunction' = 'a0',
    'Semi-Sextile' = 'a30',
    'Semi-Quintile' = 'a36',
    'Semi-Square' = 'a45',
    'Septile' = 'a51',
    'Sextile' = 'a60',
    'Quintile' = 'a72',
    'Square' = 'a90',
    'Biseptile' = 'a103',
    'Trine' = 'a120',
    'Sesquisquare' = 'a135',
    'Biquintile' = 'a144',
    'Quincunx' = 'a150',
    'Opposition' = 'a180'
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

#' Definition of speed phases and it's corresponding human readable names.
speedPhasesIdsDefinition <- function() {
  c(
    'Direct' = 'DIR',
    'Stationary' = 'STA',
    'Retrograde' = 'RET'
  )
}

#' Generic IDs to name mapping based on definition vector.
#' @param idsDefinition ID's named vector definition.
#' @param ids ID's vector to map to names.
#' @return Names vector.
idToNameMap <- function(idsDefinition, ids) {
  useIds <- idsDefinition[idsDefinition %in% ids]
  mapvalues(ids, from = useIds, to = names(useIds))
}

#' Map planet IDs vector to names.
#' @param ids Planet IDs vector.
#' @return Planet names vector.
planetIdToNameMap <- function(ids) {
  idToNameMap(planetsIdsDefinition(), ids)
}

#' Map polarity IDs vector to names.
#' @param ids Polarity IDs vector.
#' @return Polarity names vector.
polarityIdToNameMap <- function(ids) {
  idToNameMap(polarityIdsDefinition(), ids)
}

#' Map triplicity IDs vector to names.
#' @param ids Triplicity IDs vector.
#' @return Triplicity names vector.
triplicityIdToNameMap <- function(ids) {
  idToNameMap(triplicityIdsDefinition(), ids)
}

#' Map element IDs vector to names.
#' @param ids Element IDs vector.
#' @return Element names vector.
elementIdToNameMap <- function(ids) {
  idToNameMap(elementIdsDefinition(), ids)
}

#' Map zodiac sign IDs vector to names.
#' @param ids Zodiac sign IDs vector.
#' @return Zodiac sign names vector.
zodSignIdToNameMap <- function(ids) {
  idToNameMap(zodSignIdsDefinition(), ids)
}

#' Map speed phase IDs vector to names.
#' @param ids Speed phase IDs vector.
#' @return Speed phase names vector.
speedPhaseIdToNameMap <- function(ids) {
  idToNameMap(speedPhaseIdsDefinition(), ids)
}

#' Map aspects IDs vector to names.
#' @param ids Aspects IDs vector.
#' @return Aspects names vector.
aspectIdToNameMap <- function(ids) {
  idToNameMap(aspectIdsDefinition(), ids)
}
