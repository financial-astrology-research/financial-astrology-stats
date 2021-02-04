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
    'Jupiter' = 'JU',
    'NNode' = 'NN',
    'SNode' = 'SN',
    'Saturn' = 'SA',
    'Chiron' = 'CH',
    'Uranus' = 'UR',
    'Pholus' = 'PH',
    'Neptune' = 'NE',
    'Pluto' = 'PL',
    'SUEclipse' = "ES",
    'MOEclipse' = 'EM'
  )
}

#' Definition of aspects and it's corresponding human readable names.
aspectIdsDefinition <- function() {
  c(
    'Conjunction' = 'a0',
    'SemiSextile' = 'a30',
    'SemiQuintile' = 'a36',
    'SemiSquare' = 'a45',
    'Septile' = 'a51',
    'Sextile' = 'a60',
    'Quintile' = 'a72',
    'Square' = 'a90',
    'BiSeptile' = 'a103',
    'Trine' = 'a120',
    'SesquiSquare' = 'a135',
    'BiQuintile' = 'a144',
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

#' Definition of decan and it's corresponding human readable names.
decanIdsDefinition <- function() {
  c(
    'AriesD1' = '1ARI',
    'AriesD2' = '2ARI',
    'AriesD3' = '3ARI',
    'TaurusD1' = '1TAU',
    'TaurusD2' = '2TAU',
    'TaurusD3' = '3TAU',
    'GeminiD1' = '1GEM',
    'GeminiD2' = '2GEM',
    'GeminiD3' = '3GEM',
    'CancerD1' = '1CAN',
    'CancerD2' = '2CAN',
    'CancerD3' = '3CAN',
    'LeoD1' = '1LEO',
    'LeoD2' = '2LEO',
    'LeoD1' = '3LEO',
    'VirgoD1' = '1VIR',
    'VirgoD2' = '2VIR',
    'VirgoD3' = '3VIR',
    'LibraD1' = '1LIB',
    'LibraD2' = '2LIB',
    'LibraD3' = '3LIB',
    'ScorpioD1' = '1SCO',
    'ScorpioD2' = '2SCO',
    'ScorpioD3' = '3SCO',
    'SagittariusD1' = '1SAG',
    'SagittariusD2' = '2SAG',
    'SagittariusD3' = '3SAG',
    'CapricornD1' = '1CAP',
    'CapricornD2' = '2CAP',
    'CapricornD3' = '3CAP',
    'AquariusD1' = '1AQU',
    'AquariusD2' = '2AQU',
    'AquariusD3' = '3AQU',
    'PiscesD1' = '1PIS',
    'PiscesD2' = '2PIS',
    'PiscesD3' = '3PIS'
  )
}

#' Definition of speed phases and it's corresponding human readable names.
speedPhaseIdsDefinition <- function() {
  c(
    'Direct' = 'DIR',
    'Stationary' = 'STA',
    'Retrograde' = 'RET'
  )
}

#' Definition of moon phases and it's corresponding human readable names.
moonPhaseIdsDefinition <- function() {
  c(
    'Full' = 'F',
    'New' = 'N'
  )
}

#' Definition of arab moon mansions human readable names.
arabMansionIdsDefinition <- function() {
  c(
    'AM1' = 'Saratan',
    'AM2' = 'Butain',
    'AM3' = 'Turaija',
    'AM4' = 'Dabaran',
    'AM5' = 'Haq\'a',
    'AM6' = 'Han\'a',
    'AM7' = 'Dira',
    'AM8' = 'Natra',
    'AM9' = 'Tarf(a)',
    'AM10' = 'Gabha',
    'AM11' = 'Zubra',
    'AM12' = 'Sarfa',
    'AM13' = 'Auwa',
    'AM14' = 'Simak',
    'AM15' = 'Gafr',
    'AM16' = 'Zubana',
    'AM17' = 'Iklik',
    'AM18' = 'Qualb',
    'AM19' = 'Saula',
    'AM20' = 'Na\'a\'im',
    'AM21' = 'Balda',
    'AM22' = 'Dabih',
    'AM23' = 'Bula',
    'AM24' = 'Su\'ud',
    'AM25' = 'Ahbija',
    'AM26' = 'Muqaddam',
    'AM27' = 'Mu\'ahhar',
    'AM28' = 'Risa',
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
  idToNameMap(planetIdsDefinition(), ids)
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

#' Map decan IDs vector to names.
#' @param ids Decan IDs vector.
#' @return Decan names vector.
decanIdToNameMap <- function(ids) {
  idToNameMap(decanIdsDefinition(), ids)
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

#' Map moon phase IDs vector to names.
#' @param ids Moon phase IDs vector.
#' @return Moon phase names vector.
moonPhaseIdToNameMap <- function(ids) {
  idToNameMap(moonPhaseIdsDefinition(), ids)
}

#' Map arab moon mansion IDs vector to names.
#' @param ids Arab mansion IDs vector.
#' @return Arab mansion names vector.
arabMansionIdToNameMap <- function(ids) {
  idToNameMap(moonPhaseIdsDefinition(), ids)
}
