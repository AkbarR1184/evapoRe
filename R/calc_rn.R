#' Calculate Net Radiation (Rn)
#'
#' Computes net radiation (Rn) based on solar radiation, temperature, and elevation.
#'
#' @details
#' For raster inputs, provide individual raster objects or file paths for `tmax`, 
#' `tmin`, `rs`, `elevation`, and optionally `albedo`. For `data.table` input, 
#' provide a single `data.table` with columns: "lon", "lat", "date", "rs", "tmax", 
#' "tmin", "elevation", and optionally "albedo".
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom raster brick calc getZ setZ nlayers
#' @importFrom lubridate leap_year month day
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#'
#' @param tmax Raster* object or file path for maximum temperature (°C)
#' @param tmin Raster* object or file path for minimum temperature (°C)
#' @param rs Raster* object or file path for solar radiation (MJ m-2 day-1)
#' @param elevation Raster* object or file path for elevation (m)
#' @param albedo Numeric, Raster*, or file path for albedo (optional, default = 0.23)
#' @param x A `data.table` with columns: "lon", "lat", "date", "rs", "tmax", 
#'   "tmin", "elevation", and optionally "albedo"
#'
#' @return RasterBrick or data.table of net radiation values (MJ m-2 day-1)
#' @export
#'
#' @examples
#' \donttest{
#' # Example using Raster* input
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   tmax_path <- file.path(tempdir(), "tmax.nc")
#'   tmin_path <- file.path(tempdir(), "tmin.nc")
#'   rs_path   <- file.path(tempdir(), "rs.nc")
#'   elev_path <- file.path(tempdir(), "elevation.nc")
#'
#'   if (file.exists(tmax_path) && file.exists(tmin_path) &&
#'       file.exists(rs_path) && file.exists(elev_path)) {
#'     tmax <- raster::brick(tmax_path)
#'     tmin <- raster::brick(tmin_path)
#'     rs   <- raster::brick(rs_path)
#'     elev <- raster::brick(elev_path)
#'
#'     rn <- calc_rn(tmax = tmax, tmin = tmin, rs = rs, elevation = elev)
#'   }
#' }
#'
#' # Example using data.table input
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   dt <- data.table::data.table(
#'     lon = c(10.0, 10.5),
#'     lat = c(45.0, 45.5),
#'     date = as.Date(c("2001-06-01", "2001-06-01")),
#'     tmax = c(28.3, 27.6),
#'     tmin = c(14.1, 13.5),
#'     rs = c(22.5, 21.9),
#'     elevation = c(400, 420)
#'   )
#'   rn_dt <- calc_rn(x = dt)
#' }
#' }

setGeneric("calc_rn", function(tmax, tmin, rs, elevation, albedo = 0.23, x = NULL) {
  standardGeneric("calc_rn")
})

#' @rdname calc_rn
setMethod("calc_rn", 
          signature(tmax = "missing", tmin = "missing", rs = "missing", 
                    elevation = "missing", albedo = "missing"), 
          function(x) {
            x[, albedo := if ("albedo" %in% names(x)) albedo else 0.23]
            x[, rso := pmax((0.75 - 2e-5 * elevation) * rs, 1e-10)]
            x[, rws := fifelse(rs / rso < 0.3, 0.3, fifelse(rs / rso > 1, 1, rs / rso))]
            x[, rns := (1 - albedo) * rs]
            x[, es_mean := (
              (0.6108 * exp(17.27 * tmax / (tmax + 237.3))) +
                (0.6108 * exp(17.27 * tmin / (tmin + 237.3)))
            ) / 2]
            x[, rnl := 4.903e-9 * ((tmax + 273.16)^4 + (tmin + 273.16)^4) / 2 *
                (0.34 - 0.14 * sqrt(es_mean)) * (1.35 * rws - 0.35)]
            x[, rn := rns - rnl]
            return(x[, .(lon, lat, date, value = rn)])
          })

#' @rdname calc_rn
setMethod("calc_rn", 
          signature(tmax = "Raster", tmin = "Raster", rs = "Raster", 
                    elevation = "Raster", albedo = "ANY"), 
          function(tmax, tmin, rs, elevation, albedo = 0.23, x = NULL) {
            re <- esr(rs)
            rs_dates <- getZ(rs)
            re_dates <- getZ(re)
            rs_table <- data.table(rs_dates, leap = leap_year(rs_dates),
                                   mo = month(rs_dates), dd = day(rs_dates))
            re_table <- data.table(re_dates, leap = leap_year(re_dates),
                                   mo = month(re_dates), dd = day(re_dates))
            merged_dates <- merge(rs_table, re_table, by = c("leap", "mo", "dd"))
            setorder(merged_dates, rs_dates)
            rs_idx <- match(merged_dates$rs_dates, rs_dates)
            re_idx <- match(merged_dates$re_dates, re_dates)
            no_cores <- detectCores() - 1
            if (no_cores < 1 || is.na(no_cores)) no_cores <- 1
            registerDoParallel(cores = no_cores)
            dummie_rn <- foreach(layer_index = 1:nlayers(tmax)) %dopar% {
              idx_rs <- rs_idx[layer_index]
              idx_re <- re_idx[layer_index]
              dummie_tx <- tmax[[idx_rs]]
              dummie_tn <- tmin[[idx_rs]]
              dummie_rs <- rs[[idx_rs]]
              dummie_re <- re[[idx_re]]
              dummie_albedo <- if (inherits(albedo, "Raster")) {
                if (nlayers(albedo) == 1) albedo[[1]] else albedo[[idx_rs]]
              } else if (is.numeric(albedo)) {
                albedo
              } else {
                NA
              }
              rso <- (0.75 - 2e-5 * elevation) * dummie_re
              rso <- calc(rso, function(x) ifelse(x == 0, 1e-10, x))
              rns <- (1 - dummie_albedo) * dummie_rs
              es_mean <- (
                0.6108 * exp(17.27 * dummie_tx / (dummie_tx + 237.3)) +
                  0.6108 * exp(17.27 * dummie_tn / (dummie_tn + 237.3))
              ) / 2
              rws <- dummie_rs / rso
              rws <- calc(rws, function(x) ifelse(x < 0.3, 0.3, ifelse(x > 1, 1, x)))
              rnl <- 4.903e-9 * ((dummie_tx + 273.16)^4 + (dummie_tn + 273.16)^4) / 2 *
                (0.34 - 0.14 * sqrt(es_mean)) * (1.35 * rws - 0.35)
              rns - rnl
            }
            rn_brick <- brick(dummie_rn)
            rn_brick <- setZ(rn_brick, rs_dates[rs_idx])
            return(rn_brick)
          })

#' @rdname calc_rn
setMethod("calc_rn", 
          signature(tmax = "character", tmin = "character", rs = "character", 
                    elevation = "character", albedo = "ANY"), 
          function(tmax, tmin, rs, elevation, albedo = 0.23, x = NULL) {
            dummie_tx <- brick(tmax)
            dummie_tn <- brick(tmin)
            dummie_rs <- brick(rs)
            dummie_elev <- brick(elevation)
            dummie_albedo <- if (is.character(albedo)) brick(albedo) else albedo
            calc_rn(
              tmax = dummie_tx,
              tmin = dummie_tn,
              rs = dummie_rs,
              elevation = dummie_elev,
              albedo = dummie_albedo
            )
          })
