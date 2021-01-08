# Title     : Define different planets sets based on classic and modern astrology.
# Objective : The debate of astrologers don't finish with what angular aspects and orbs should be considered
#             to forecast cycles effects, also as different bodies was discovered in our solar system
#             it was considered to evolve classic astrology and fit Uranus, Neptune, Pluto, asteroids and so forth.
#             The challenge is that the list can grow so long if we include arabic parts, midpoints and other
#             potential critical points that apparently could be influenctial.
#             As performing all the combinations is an intractable problem that requires a lot of computational power
#             even using the most advanced algorithms as Genetic optimization I will try to stick only to planets,
#             sun, moon and asteroids. And the only ficticious points included here are the Moon Nodes.
# Created by: pablocc
# Created on: 08/01/2021

planetsMOMEVESUMAJUNNSAURNEPL <- function() {
  planetsBaseCols <<- c('MO', 'ME', 'VE', 'SU', 'MA', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  setPlanetsColsNames(planetsBaseCols)
}

planetsMOMEVESUMACEJUNNSAURNEPL <- function() {
  planetsBaseCols <<- c('MO', 'ME', 'VE', 'SU', 'MA', 'CE', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  setPlanetsColsNames(planetsBaseCols)
}

planetsMOMEVESUMACEVSJUNNSAURCHNEPL <- function() {
  planetsBaseCols <<- c('MO', 'ME', 'VE', 'SU', 'MA', 'CE', 'VS', 'JU', 'NN', 'SA', 'UR', 'CH', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  setPlanetsColsNames(planetsBaseCols)
}

planetsMOMEVESUMAJUSAURNEPL <- function() {
  planetsBaseCols <<- c('MO', 'ME', 'VE', 'SU', 'MA', 'JU', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  setPlanetsColsNames(planetsBaseCols)
}

planetsMOMEVESUMACEJUSAURNEPL <- function() {
  planetsBaseCols <<- c('MO', 'ME', 'VE', 'SU', 'MA', 'CE', 'JU', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  setPlanetsColsNames(planetsBaseCols)
}

planetsMEVESUMACEJUNNSAURNEPL <- function() {
  planetsBaseCols <<- c('ME', 'VE', 'SU', 'MA', 'CE', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  setPlanetsColsNames(planetsBaseCols)
}

planetsMEVESUMACEJUSAURNEPL <- function() {
  planetsBaseCols <<- c('ME', 'VE', 'SU', 'MA', 'CE', 'JU', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  setPlanetsColsNames(planetsBaseCols)
}

planetsMEVESUMAJUSAURNEPL <- function() {
  planetsBaseCols <<- c('ME', 'VE', 'SU', 'MA', 'JU', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  setPlanetsColsNames(planetsBaseCols)
}

planetsSUMOMEVEMACEJUNNSNSAURNEPL <- function() {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'CE', 'JU', 'NN', 'SN', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  setPlanetsColsNames(planetsBaseCols)
}
