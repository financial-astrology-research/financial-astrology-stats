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

# I designated a planets / celestial objects two chars IDs to shorten the columns names in data tables as follow:
# MO = Moon (0.07 years)
# ME = Mercury (0.24 years)
# VE = Venus (0.61 years)
# SU = Sun (1 year)
# MA = Mars (1.88 years)
# VS = Vesta (3.63 years)
# JN = Juno (4.36 years)
# CE = Ceres (4.61 years)
# PA = Pallas (4.62 years)
# JU = Jupiter (11.86 years)
# SA = Saturn (29.45 years)
# CH = Chiron (50.42 years)
# UR = Uranus (84.02 years)
# PH = Pholus (92.09 years)
# NE = Neptune (164.8 years)
# PL = Pluto (247.94 years)

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
    c('MO', 'ME', 'VE', 'SU', 'MA', 'VS', 'CE', 'JU', 'NN', 'SA', 'UR', 'CH', 'NE', 'PL')
  )
}

allPlanetsAndAsteroids <- function() {
  return(
    c('MO', 'ME', 'VE', 'SU', 'MA', 'VS', 'JN', 'CE', 'PA', 'JU', 'SA', 'CH', 'UR', 'PH', 'NE', 'PL')
  )
}
