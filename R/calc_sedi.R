#' Calculate Standardized Evapotranspiration Deficit Index (SEDI)
#'
#' Computes the standardized anomaly of the ET − PET difference, reflecting water balance stress.
#'
#' @details
#' This function calculates SEDI on a monthly time scale by evaluating the ET deficit (ET - PET)
#' for each grid cell and calendar month. The anomalies are standardized (z-scores) to detect
#' periods of moisture shortage or surplus.
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom lubridate month
#'
#' @param x A `data.table` with columns: "lon", "lat", "date", "aet", "pet"
#'
#' @return A `data.table` with columns: "lon", "lat", "date", "value" (SEDI)
#'
#' @references
#' Zhang, X., et al. (2019). Assessment of an evapotranspiration deficit drought index in relation to impacts on ecosystems.
#' Advances in Atmospheric Sciences, 36, 1273–1287. https://doi.org/10.1007/s00376-019-9061-6
#'
#' @examples
#' \donttest{
#' library(data.table)
#' dt <- data.table(
#'   lon = c(10, 10), lat = c(45, 45),
#'   date = as.Date(c("2001-01-01", "2001-02-01")),
#'   aet = c(30, 25), pet = c(40, 35)
#' )
#' calc_sedi(x = dt)
#' }
#'
#' @export
calc_sedi <- function(x) {
  stopifnot(all(c("lon", "lat", "date", "aet", "pet") %in% names(x)))
  
  x <- copy(x)
  x[, `:=`(
    e_minus_pet = aet - pet,
    month = month(date),
    id = .GRP
  ), by = .(lon, lat)]
  
  x[, mean_diff := mean(e_minus_pet, na.rm = TRUE), by = .(id, month)]
  x[, anomaly := e_minus_pet - mean_diff]
  
  x[!is.na(anomaly),
    value := (anomaly - mean(anomaly, na.rm = TRUE)) / sd(anomaly, na.rm = TRUE),
    by = .(id, month)]
  
  return(x[, .(lon, lat, date, value)])
}
