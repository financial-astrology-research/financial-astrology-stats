# Title     : Define different planets sets based on classic and modern astrology.
# Objective : The debate of astrologers don't finish with what angular aspects and orbs should be considered
#             to forecast cycles effects. Furthermore, as different celestial objects was discovered in our solar system
#             it was needed to evolve classic astrology and fit Uranus, Neptune, Pluto, asteroids and so forth.
#             The challenge is that the list can grow so long if we include arabic parts, midpoints and other
#             potential critical points, like radix positions that could be influential.
#             As performing all the combinations is an intractable problem that requires a lot of computational power
#             even using the most advanced algorithms as Genetic optimization I will try to stick only to planets,
#             sun, moon and asteroids. And the only ficticious points included here are the Moon Nodes.
# Created by: pablocc
# Created on: 08/01/2021

# I designated a planets / celestial objects IDs used to shorten the columns names in data tables:
# MO = Moon
# ME = Mercury
# VE = Venus
# SU = Sun
# MA = Mars
# CE = Ceres
# VS = Vesta
# JU = Jupiter
# SA = Saturn
# CH = Chiron
# UR = Uranus
# NE = Neptune
# PL = Pluto

#' Planets used by classic astrologers prior Uranus discovery in 1781.
#' @return Planets IDs vector.
classicPlanets <- function() {
  return(
    c('MO', 'ME', 'VE', 'SU', 'MA', 'JU', 'SA')
  )
}

modernPlanets <- function() {
  return(
    c('MO', 'ME', 'VE', 'SU', 'MA', 'JU', 'SA', 'UR', 'NE', 'PL')
  )
}

modernPlanetsAndNN <- function() {
  return (
    c('MO', 'ME', 'VE', 'SU', 'MA', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL')
  )
}

modernPlanetsAndCENN <- function() {
  return(
    c('MO', 'ME', 'VE', 'SU', 'MA', 'CE', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL')
  )
}

modernPlanetsAndCE <- function() {
  return(
    c('MO', 'ME', 'VE', 'SU', 'MA', 'CE', 'JU', 'SA', 'UR', 'NE', 'PL')
  )
}

modernPlanetsAndCEVSCHNN <- function() {
  return (
    c('MO', 'ME', 'VE', 'SU', 'MA', 'CE', 'VS', 'JU', 'NN', 'SA', 'UR', 'CH', 'NE', 'PL')
  )
}
