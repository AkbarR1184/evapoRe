#' Extraterrestrial Solar Radiation (ESR) Dates
#'
#' Fix dates for best ESR estimates
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom methods as
#' @importFrom raster getZ
#' @param x a character string with the path to the data file.
#' @return data.table
#' @keywords internal

esr_dates <- function(x){
  dummie_day_dif <- as.Date('1980-01-02') - as.Date('1980-01-01')
  dummie_month_dif <- as.Date('1980-02-01') - as.Date('1980-01-01')
  dummie_year_dif1 <- as.Date('1981-01-01') - as.Date('1980-01-01')
  dummie_year_dif2 <- as.Date('1982-01-01') - as.Date('1981-01-01')
  dummie <- getZ(x)
  dummie_dif <- dummie[2] - dummie[1]
  if (dummie_dif == dummie_day_dif){
    dummie_dates <- dummie
  } else if ((dummie_dif > dummie_day_dif) & (dummie_dif <= dummie_month_dif)) {
    dummie_dates <- dummie
    dummie_dates[month(dummie_dates) == 1] <- sub("\\d{2}$", "17", dummie_dates[month(dummie_dates) == 1])
    dummie_dates[month(dummie_dates) == 2] <- sub("\\d{2}$", "16", dummie_dates[month(dummie_dates) == 2])
    dummie_dates[month(dummie_dates) == 3] <- sub("\\d{2}$", "16", dummie_dates[month(dummie_dates) == 3])
    dummie_dates[month(dummie_dates) == 4] <- sub("\\d{2}$", "15", dummie_dates[month(dummie_dates) == 4])
    dummie_dates[month(dummie_dates) == 5] <- sub("\\d{2}$", "15", dummie_dates[month(dummie_dates) == 5])
    dummie_dates[month(dummie_dates) == 6] <- sub("\\d{2}$", "11", dummie_dates[month(dummie_dates) == 6])
    dummie_dates[month(dummie_dates) == 7] <- sub("\\d{2}$", "17", dummie_dates[month(dummie_dates) == 7])
    dummie_dates[month(dummie_dates) == 8] <- sub("\\d{2}$", "16", dummie_dates[month(dummie_dates) == 8])
    dummie_dates[month(dummie_dates) == 9] <- sub("\\d{2}$", "15", dummie_dates[month(dummie_dates) == 9])
    dummie_dates[month(dummie_dates) == 10] <- sub("\\d{2}$", "15", dummie_dates[month(dummie_dates) == 10])
    dummie_dates[month(dummie_dates) == 11] <- sub("\\d{2}$", "14", dummie_dates[month(dummie_dates) == 11])
    dummie_dates[month(dummie_dates) == 12] <- sub("\\d{2}$", "10", dummie_dates[month(dummie_dates) == 12])
  } else if((dummie_dif == dummie_year_dif1) | (dummie_dif == dummie_year_dif1)){
    dummie_dates <- dummie
    dummie_dates <- sub("\\-\\d{2}", '-03', dummie_dates)
    dummie_dates<- sub("\\d{2}$", "16", dummie_dates)
  }
  return(dummie_dates)
}
