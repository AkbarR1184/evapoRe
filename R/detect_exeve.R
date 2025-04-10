#' Detect Extreme Evaporation Events (ExEvE)
#'
#' The function \code{detect_exeve} identifies extreme evaporation events based
#' on standardized evaporation and extreme thresholds.
#' @importFrom stats quantile sd
#' @param x A \code{data.table} containing columns: \code{lon}, \code{lat},
#' \code{date}, and \code{value}, representing daily evaporation values.
#' @param EXTREMES_THRES Numeric. Quantile threshold used to define
#' extreme evaporation events. Default is 0.95.
#' @param LOW_THRES Numeric. Lower quantile threshold. Default is 0.80.
#'
#' @return A \code{data.table} with original columns and added columns:
#' \itemize{
#'   \item \code{std_value} — standardized evaporation
#'   \item \code{pentad_std_q80}, \code{pentad_std_q95} — pentad thresholds
#'   \item \code{value_above_low_thres}, \code{extreme}, \code{evap_event} — flags
#'   \item \code{above_low_thres_id}, \code{extreme_id}, \code{event_id} — event group IDs
#' }
#' @export
#' @examples
#' \donttest{
#' # Example using an RDS file (only run if file exists)
#' evap_path <- file.path(tempdir(), "czechia_evap_gleam.rds")
#' if (file.exists(evap_path)) {
#'   evap <- readRDS(evap_path)
#'   events <- detect_exeve(evap)
#' }
#' }

detect_exeve <- function(x, EXTREMES_THRES = 0.95, LOW_THRES = 0.80) {
  stopifnot(is.data.table(x))
  stopifnot(all(c("lon", "lat", "date", "value") %in% names(x)))
  
  x <- x[order(lon, lat)]
  x[, grid_id := .GRP, by = .(lat, lon)]
  x <- x[, .(grid_id, date, value)]
  
  pentads <- copy(x)
  pentads[, pentad := ceiling(
    (yday(date) - leap_year(year(date)) * (yday(date) > 59)) / 5
  )]
  pentads[, std_value := (value - mean(value)) / sd(value),
          by = .(pentad, grid_id)]
  pentads[, pentad_std_q95 := quantile(std_value, EXTREMES_THRES),
          by = grid_id]
  pentads[, pentad_std_q80 := quantile(std_value, LOW_THRES),
          by = grid_id]
  pentads[, value := NULL]
  
  exeves <- merge(
    x,
    pentads[, .(grid_id, date, std_value, pentad_std_q80, pentad_std_q95)],
    by = c("grid_id", "date"), all.x = TRUE
  )
  
  exeves[, value_above_low_thres := std_value > 0]
  exeves[, extreme := std_value > pentad_std_q95]
  exeves[, evap_event := FALSE]
  
  exeves[, above_low_thres_id := rleid(value_above_low_thres)]
  exeves[, extreme_id := rleid(extreme), by = grid_id]
  
  exeves[extreme == TRUE,
         evap_event := TRUE, by = .(grid_id, above_low_thres_id)]
  
  above_low_thres_ids_with_extreme <- exeves[extreme == TRUE,
                                             unique(above_low_thres_id)]
  exeves[above_low_thres_id %in% above_low_thres_ids_with_extreme,
         evap_event := TRUE]
  
  exeves[, event_id := rleid(evap_event), by = grid_id]
  exeves[evap_event != TRUE, event_id := NA]
  exeves[extreme != TRUE, extreme_id := NA]
  
  return(exeves)
}
