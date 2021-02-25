# Title     : Prepare daily fixed stars position table.
# Objective : Support research of planets / stars aspects and planets chinese mansions location.
# Created by: pablocc
# Created on: 25/02/2021

library(lubridate)
library(magrittr)
library(swephR)
options(digits=14)
data(SE)

#' Convert a given date/time to Julian Day.
#' @param dateTime Date time string to convert.
#' @return A julian day.
dateTimeToJulianDayConvert <- function(dateTime) {
  hourDecimal <- hour(dateTime) + (minute(dateTime) / 60)
  jd <- swe_julday(
    year(dateTime),
    month(dateTime),
    mday(dateTime),
    hourDecimal,
    SE$GREG_CAL
  )

  round(jd, 4)
}

#' Calculate planet longitude for a given date/time.
#' @param dateTime Date time string.
#' @param planetID A sweph planet ID, can be inspected at SE global list.
planetLongitudeGet <- function(dateTime, planetID) {
  iflag <- SE$FLG_MOSEPH + SE$FLG_SPEED
  jd <- dateTimeToJulianDayConvert(dateTime)
  position <- swe_calc_ut(jd, planetID, iflag)$xx
  position[1]
}


#' Calculate fixed star longitude for a given date/time.
#' @param dateTime Date time string.
#' @param starName A sweph star name.
starLongitudeGet <- function(dateTime, starName) {
  iflag <- SE$FLG_MOSEPH + SE$FLG_SPEED
  jd <- dateTimeToJulianDayConvert(dateTime)
  result <- swe_fixstar2_ut(starName, jd, iflag)
  print(result)
  position <- result$xx
  position[1]
}


planetLongitudeGet("2021-02-26 20:30:00", SE$SUN)
starLongitudeGet("2021-02-26 20:30:00", "sirius")