# Title     : Swiss ephemeris example
# Objective : Example script to demonstrate the use of swissEphemeris
# Created by: thiloyes
# Created on: 29/01/2021

library(swephR)
source('astro_utils.R')
year <- 1983
month <- 7
day <- 9
hour <- 5
minute <- 30
geo_lat <- 52.5
geo_lon <- 12.5


jd <- swe_julday(year, month, day, 0,SE$GREG_CAL) + (hour + minute/60)/24
for (planet in planets) {
  res <- swe_calc_ut(jd, planet, 0)
  lon <- res$xx[1]
  cat(paste(swe_get_planet_name(planet) , format_longitude(lon)), '\n')
  
}

houses <- swe_houses_ex(jd, 0, geo_lat, geo_lon, 'P')
for (i in 2:13) {
  cat(paste("house ", i-1 ," ", format_longitude(houses$cusps[i]), '\n'))
}