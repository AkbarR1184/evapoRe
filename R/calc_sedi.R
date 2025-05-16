#' Calculate Standardized Evapotranspiration Deficit Index (SEDI)
#'
#' Computes the standardized anomaly of (ET − PET) over a defined time scale.
#'
#' @details
#' SEDI is calculated by aggregating (ET − PET) over a moving window of `scale` months
#' and standardizing the anomaly per calendar month and location. Rolling aggregation uses
#' `frollsum()` from the `data.table` package.
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom lubridate month
#'
#' @param x A `data.table` with columns: "lon", "lat", "date", "aet", "pet"
#' @param scale Integer. Number of months to aggregate over (e.g., 1 = monthly, 6 = SEDI-6)
#'
#' @return A `data.table` with columns: "lon", "lat", "date", "value" (SEDI)
#'
#' @references
#' Zhang, X., et al. (2019). Assessment of an evapotranspiration deficit drought index in relation to impacts on ecosystems.
#' Advances in Atmospheric Sciences, 36, 1273–1287. https://doi.org/10.1007/s00376-019-9061-6
#'
#' @export
calc_sedi <- function(x, scale = 1) {
  stopifnot(all(c("lon", "lat", "date", "aet", "pet") %in% names(x)))
  
  x <- copy(x)
  x[, id := .GRP, by = .(lon, lat)]
  x <- x[order(id, date)]
  x[, et_deficit := aet - pet]
  
  x[, roll_deficit := frollsum(et_deficit, n = scale, align = "right", fill = NA), by = id]
  x[, month := month(date)]
  
  x[!is.na(roll_deficit),
    value := (roll_deficit - mean(roll_deficit, na.rm = TRUE)) / sd(roll_deficit, na.rm = TRUE),
    by = .(id, month)]
  
  return(x[, .(lon, lat, date, value)])
}
