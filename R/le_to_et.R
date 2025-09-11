#' Convert latent heat flux (LE) to evapotranspiration (ET)
#'
#' The function \code{le_2_et} computes ET from latent heat flux.
#'
#' @details
#' Provide inputs in the following format:
#' * A \code{data.table} with the columns:
#'   "lon", "lat", "date", "le" (MJ m-2 day-1) and, optionally, "tavg" (°C)
#'   when a temperature-dependent latent heat of vaporization is desired.
#'   
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#'
#' @param x  `data.table` with columns "lon", "lat", "date", "le",
#'             and optionally "tavg".
#'
#' @return A `data.table` with columns: `"lon"`, `"lat"`, `"date"`, `"value"`
#'   (ET in mm/day).
#'   
#' @examples
#' \donttest{
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   dt <- data.table::data.table(
#'     lon = c(10.0, 10.5),
#'     lat = c(45.0, 45.5),
#'     date = as.Date(c("2025-01-01", "2025-01-02")),
#'     le = c(12.3, 13.1),   # MJ m-2 day-1
#'     tavg = c(20, 21)      # °C
#'   )
#'   et_dt <- le_2_et(x = dt)
#' }
#' }
#' @export
le_2_et <- function(x) {
  if ("tavg" %in% names(x)) {
    x[, lambda := 2.501 - 0.002361 * tavg]
  } else {
    x[, lambda := 2.45]
  }
  
  x[, value := le / lambda]
  x[value < 0, value := NA_real_]
  
  x[, .(lon, lat, date, value)]
}