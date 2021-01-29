# Title     : Astrological utility function
# Objective : Ease the calculation of planetary positions, aspects and more
# Created by: thiloyes
# Created on: 29/01/2021

library(swephR)
library(lubridate)

planets = c(SE$MOON, SE$SUN, SE$MERCURY, SE$VENUS, SE$MARS, SE$JUPITER, SE$SATURN, SE$URANUS, SE$NEPTUNE, SE$PLUTO, SE$MEAN_NODE)
signs = c('AR', 'TA', 'GE', 'CN', 'LE', 'VI', 'LI', 'SC', 'SG', 'CP', 'AQ', 'PI')

#' Format a longitude (between 0 and 360) into sign, degree, minute, second
#' @param lon longitude
#' @return formatted longitude
format_longitude <- function(lon) {
  degree <- floor(lon)
  sign = signs[degree %/% 30 + 1]
  degree_sign <- degree %% 30
  minutes <- (lon - degree)*60
  minutes_clean <- floor(minutes)
  seconds <- round((minutes-minutes_clean) * 60)
  paste0(sign, " ", degree_sign, '°', minutes_clean, "'", seconds, "''")
}

#' Normalize degree to 0 and 360
#' @param x degrees
#' @return normalized degrees
degnorm <- function(x) {
  x %% 360
}


#' Normalize degree distance to a value between 0 and 180
#' @param x distance
#' @return normalized distance
degDistNormAbs <- function(x) {
  x[x > 180] <- x[x > 180] - 360
  x[x < -180] <- x[x < -180] + 360
  abs(x)
}

#' Normalize degree distance to a value between -180 and 180
#' @param x distance
#' @return normalized distance
degnorm180 <- function(x) {
  x <- degnorm(x)
  x[x > 180] <- x[x > 180] - 360
  x
}

#' Calculate the distance of sun and moon
#' @param calc_date date
#' @param hour hour of the day
#' @param minute of the day
#' @return distance of moon and sun, if positive moon is waxing
get_sun_moon_dist <- function(calc_date, hour, minute) {
  jd <- swe_julday(year(calc_date), 
                   month(calc_date),
                   mday(calc_date),
                   0,
                   SE$GREG_CAL) + (hour + minute/60)/24
  moon <- swe_calc_ut(jd, SE$MOON, 0)
  sun <- swe_calc_ut(jd, SE$SUN, 0)
  moon_lon <- moon$xx[1]
  sun_lon <- sun$xx[1]
  degnorm180(moon_lon - sun_lon)
}

#' check whether there is a new moon event between the two dates
#' @param calc_date distance
#' @param prev_date previous date
#' @param cut_off_time the time of day that is used
#' @return boolean, True if new moon event lies between the dates
is_new_moon <- function(calc_date, prev_date, cut_off_time) {
  d1 <- get_sun_moon_dist(calc_date, cut_off_time, 0)
  d2 <- get_sun_moon_dist(prev_date, cut_off_time, 0)
  if (d1 > 0 & d2 < 0) {
    return(T)
  }
  return(F)
  
}

#' check whether there is a full moon event between the two dates
#' @param calc_date distance
#' @param prev_date previous date
#' @param cut_off_time the time of day that is used
#' @return boolean, True if full moon event lies between the dates
is_full_moon <- function(calc_date, prev_date, cut_off_time) {
  d1 <- degnorm180(get_sun_moon_dist(calc_date, cut_off_time, 0) - 180)
  d2 <- degnorm180(get_sun_moon_dist(prev_date, cut_off_time, 0) - 180)
  if (d1 > 0 & d2 < 0) {
    return(T)
  }
  return(F)
  
}