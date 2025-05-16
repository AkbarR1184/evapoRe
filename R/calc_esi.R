#' Calculate Evaporative Stress Index (ESI)
#'
#' Computes the standardized anomaly of the ET/PET ratio, indicating vegetation water stress.
#'
#' @details
#' This function calculates ESI on a monthly time scale using actual and potential evapotranspiration.
#' The ET/PET ratio is calculated for each grid cell and calendar month, and then standardized
#' (z-score) to highlight deviations from normal moisture conditions.
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom lubridate month
#'
#' @param x A `data.table` with columns: "lon", "lat", "date", "aet", "pet"
#'
#' @return A `data.table` with columns: "lon", "lat", "date", "value" (ESI)
#'
#' @references
#' Anderson, M. C., et al. (2011). Evaluation of drought indices based on thermal remote sensing
#' of evapotranspiration over the continental United States. Journal of Climate, 24(8), 2025–2044.
#' https://doi.org/10.1175/2010JCLI3812.1
#'
#' @examples
#' \donttest{
#' library(data.table)
#' dt <- data.table(
#'   lon = c(10, 10), lat = c(45, 45),
#'   date = as.Date(c("2001-01-01", "2001-02-01")),
#'   aet = c(30, 25), pet = c(40, 35)
#' )
#' calc_esi(x = dt)
#' }
#'
#' @export
calc_esi <- function(x) {
  stopifnot(all(c("lon", "lat", "date", "aet", "pet") %in% names(x)))
  
  x <- copy(x)
  x[, `:=`(
    et_pet = aet / pet,
    month = month(date),
    id = .GRP
  ), by = .(lon, lat)]
  
  x[, mean_ratio := mean(et_pet, na.rm = TRUE), by = .(id, month)]
  x[, anomaly := et_pet - mean_ratio]
  
  x[!is.na(anomaly),
    value := (anomaly - mean(anomaly, na.rm = TRUE)) / sd(anomaly, na.rm = TRUE),
    by = .(id, month)]
  
  return(x[, .(lon, lat, date, value)])
}
