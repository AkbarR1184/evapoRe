#' Potential Evapotranspiration (PET) Parameters
#'
#' Calculate parameters related to PET, including `esr_date`, `omega`, `ext_rad`,
#' `delta`,`lat_rad`, and, `dr`
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom methods as
#' @importFrom lubridate days_in_month
#' @param x  a data.table, its columns should be named: "lon", "lat", "date", and "value".
#' @return data.table
#' @keywords internal


pet_params_calc <- function(x) {
  date_sequence <- seq(from = x[, date[1]], to = x[, date[.N]], by = "month")
  day_of_month_map <- c(17, 16, 16, 15, 15, 11, 17, 16, 15, 15, 14, 10)
  esr_date <- as.Date(sprintf(
    "%d-%02d-%02d",
    year(date_sequence),
    month(date_sequence),
    day_of_month_map[month(date_sequence)]
  ))
  lookup_table <- data.table(
    esr_date = esr_date,
    date = date_sequence,
    dr = 1 + 0.033 * cos(2 * pi * yday(esr_date) / 365.25),
    delta = 0.409 * sin(2 * pi * yday(esr_date) / 365.25 - 1.39)
  )
  lat_index <- unique(x[, .(lat)])[, lat_rad := lat * pi / 180]
  pet_params <- CJ(lat_rad = lat_index$lat_rad, date = lookup_table$date, sorted = FALSE)
  pet_params <- merge(pet_params, lat_index, by = "lat_rad")
  pet_params <- merge(pet_params, lookup_table, by = "date")
  pet_params[, arg := pmin(pmax(-tan(lat_rad) * tan(delta), -1), 1)]
  pet_params[, omega := acos(arg)]
  pet_params[, ext_rad := (24.0 * 60.0 / pi) * 0.0820 * dr * (
    omega * sin(lat_rad) * sin(delta) + cos(lat_rad) * cos(delta) * sin(omega)
  )]
  return(pet_params)
}
