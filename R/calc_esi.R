#' Calculate Evaporative Stress Index (ESI)
#'
#' Computes the standardized anomaly of the ET/PET ratio over a defined time scale.
#'
#' @details
#' ESI is computed by aggregating the ET/PET ratio over a moving window of `scale` months
#' and standardizing it per calendar month and grid cell. Rolling aggregation uses `frollmean()`
#' from the `data.table` package.
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom lubridate month
#'
#' @param x A `data.table` with columns: "lon", "lat", "date", "aet", "pet"
#' @param scale Integer. Number of months to aggregate over (e.g., 1 = monthly, 3 = 3-month ESI)
#'
#' @return A `data.table` with columns: "lon", "lat", "date", "value" (ESI)
#'
#' @references
#' Anderson, M. C., et al. (2011). Evaluation of drought indices based on thermal remote sensing
#' of evapotranspiration over the continental United States. Journal of Climate, 24(8), 2025–2044.
#' https://doi.org/10.1175/2010JCLI3812.1
#'
#' @export
calc_esi <- function(x, scale = 1) {
  stopifnot(all(c("lon", "lat", "date", "aet", "pet") %in% names(x)))
  
  x <- copy(x)
  x[, id := .GRP, by = .(lon, lat)]
  x <- x[order(id, date)]
  x[, et_pet := aet / pet]
  
  x[, roll_ratio := frollmean(et_pet, n = scale, align = "right", fill = NA), by = id]
  x[, month := month(date)]
  
  x[!is.na(roll_ratio),
    value := (roll_ratio - mean(roll_ratio, na.rm = TRUE)) / sd(roll_ratio, na.rm = TRUE),
    by = .(id, month)]
  
  return(x[, .(lon, lat, date, value)])
}
