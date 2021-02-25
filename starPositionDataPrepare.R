# Title     : Prepare daily fixed stars position table.
# Objective : Support research of planets / stars aspects and planets chinese mansions location.
# Created by: pablocc
# Created on: 25/02/2021

library(data.table)
library(lubridate)
library(magrittr)
library(swephR)

options(digits = 14)
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
#' @examples
#'   planetLongitudeGet("2021-02-26 20:30:00", SE$SUN)
#'   planetLongitudeGet("2021-02-26 20:30:00", SE$MOON)
planetLongitudeGet <- function(dateTime, planetID) {
  iflag <- SE$FLG_MOSEPH + SE$FLG_SPEED
  jd <- dateTimeToJulianDayConvert(dateTime)
  position <- swe_calc_ut(jd, planetID, iflag)$xx
  position[1]
}


#' Calculate fixed star longitude for a given date/time.
#' @param dateTime Date time string.
#' @param starID A sweph star ID (nomenclature), without leading comma.
starLongitudeGet <- function(dateTime, starID) {
  iflag <- SE$FLG_MOSEPH + SE$FLG_SPEED + SE$FLG_EQUATORIAL
  jd <- dateTimeToJulianDayConvert(dateTime)
  result <- swe_fixstar2_ut(paste0(',', starID), jd, iflag)
  position <- result$xx
  position[1]
}

chineseZodiacStarsLatitudeGet <- function(dateTime) {
  zodStarIds <- c(
    'beAri',
    'ta-6Eri',
    '16Tau',
    'epTau',
    'laOri',
    'zeOri',
    'muGem',
    'xiPup',
    'laDra',
    'alPyx',
    'chUMa',
    'gaCom',
    'gaCrv',
    'alVir',
    'laCen',
    'xi-2Lib',
    'piSco',
    'siSco',
    'alHer',
    'ga-2Sgr',
    'muLyr',
    'beCap',
    'epAqr',
    'gaEqu',
    'alAqr',
    'alPeg',
    'psPeg',
    'piAnd'
  )

  longitudes <- lapply(zodStarIds, function(starID) starLongitudeGet(dateTime, starID))
  starsLongitudeTable <- data.table(
    Date = as.Date(dateTime),
    StarID = zodStarIds,
    Longitude = longitudes
  )

  data.table::dcast(
    starsLongitudeTable,
    Date ~ StarID,
    value.var = 'Longitude'
  )
}

chineseZodiacStarsLatitudeGet("2021-02-25 12:00")