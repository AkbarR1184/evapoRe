#' Calculate Net Radiation (Rn)
#'
#' Computes net radiation (Rn) based on solar radiation, temperature, and elevation.
#' @details
#' For raster inputs, provide individual raster objects or file paths for `tmax`, 
#' `tmin`, `rs`, `elevation`, and optionally `albedo`. For `data.table` input, 
#' provide a single `data.table` with columns: "lon", "lat", "date", "rs", "tmax", 
#' "tmin", "elevation", and optionally "albedo".
#' 
#' @import data.table
#' @importFrom raster brick calc getZ setZ compareRaster nlayers
#' @importFrom lubridate leap_year month day
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @param tmax Raster* object or file path for maximum temperature.
#' @param tmin Raster* object or file path for minimum temperature.
#' @param rs Raster* object or file path for solar radiation.
#' @param elevation Raster* object or file path for elevation.
#' @param albedo Numeric value, Raster* object, or file path for albedo 
#'               (optional, default = 0.23).
#' @param x A `data.table` with required columns.
#' @return Raster* object or `data.table` with net radiation values.
#' @keywords internal
#' 
setGeneric("calc_rn", function(tmax, tmin, rs, elevation, albedo = 0.23, x = NULL) 
  standardGeneric("calc_rn"))
#' @rdname calc_rn
setMethod("calc_rn", 
          signature(tmax = "missing", tmin = "missing", rs = "missing", 
                    elevation = "missing", albedo = "missing"), 
          function(x) {
            if (!inherits(x, "data.table")) 
              stop("For data.table input, x must be a data.table.")
            
            required_cols <- c("lon", "lat", "date", "rs", "tmax", "tmin", 
                               "elevation")
            missing_cols <- setdiff(required_cols, names(x))
            if (length(missing_cols) > 0) 
              stop("Missing columns: ", paste(missing_cols, collapse = ", "))
            
            x[, albedo := if ("albedo" %in% names(x)) albedo else 0.23]
            
            x[, rso := pmax((0.75 - 2e-5 * elevation) * rs, 1e-10)]
            x[, rws := fifelse(rs / rso < 0.3, 0.3,fifelse(rs / rso > 1, 1,rs / rso))]
            x[, rns := (1 - albedo) * rs]
            x[, es_mean := (0.6108 * exp(17.27 * tmax / (tmax + 237.3))) + 
                (0.6108 * exp(17.27 * tmin / (tmin + 237.3))) / 2]
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
            rs_dates_table <- as.data.table(rs_dates) %>%
              .[, leap := leap_year(rs_dates)] %>%
              .[, mo := month(rs_dates)] %>%
              .[, dd := day(rs_dates)]
            re_dates_table <- as.data.table(re_dates) %>%
              .[, leap := leap_year(re_dates)] %>%
              .[, mo := month(re_dates)] %>%
              .[, dd := day(re_dates)]
            dummie_dates_table <- merge(rs_dates_table, re_dates_table, by = c('leap', 'mo', 'dd'))
            setorder(dummie_dates_table, 'rs_dates')
            rs_idx <- match(dummie_dates_table$rs_dates, rs_dates)
            re_idx <- match(dummie_dates_table$re_dates, re_dates)
            dummie_rn <- foreach(index = 1:length(rs_idx)) %dopar% {
              rs_layer <- rs_idx[index]
              re_layer <- re_idx[index]
              dummie_rs <- rs[[rs_layer]]
              dummie_tmax <- tmax[[rs_layer]]
              dummie_tmin <- tmin[[rs_layer]]
              dummie_re <- re[[re_layer]]
              dummie_albedo <-
                if (is.numeric(albedo)) {
                  albedo
                } else if (nlayers(albedo) == 1) {
                  albedo[[1]]
                } else {
                  albedo[[rs_layer]]
                }
              
              rso <- (0.75 - 2e-5 * elevation) * dummie_re
              rso <- calc(rso, fun = function(val) 
                ifelse(val == 0, 1e-10, val))
              rns <- (1 - dummie_albedo) * dummie_rs
              es_mean <- (0.6108 * exp(17.27 * dummie_tmax / (dummie_tmax + 237.3))) + 
                (0.6108 * exp(17.27 * dummie_tmin / (dummie_tmin + 237.3))) / 2
              rws <- dummie_rs / rso
              rws <- calc(
                rws,
                fun = function(val) {
                  ifelse(val < 0.3, 0.3, 
                         ifelse(val > 1, 1, val))
                }
              )
              
              rnl <- 4.903e-9 * ((dummie_tmax + 273.16)^4 + (dummie_tmin + 273.16)^4) / 2 *
                (0.34 - 0.14 * sqrt(es_mean)) * (1.35 * rws - 0.35)
              rns - rnl
            }
            rn_brick <- brick(dummie_rn)
            setZ(rn_brick, rs_dates)
            return(rn_brick)
          })


#' @rdname calc_rn
setMethod("calc_rn", 
          signature(tmax = "character", tmin = "character", rs = "character", 
                    elevation = "character", albedo = "ANY"), 
          function(tmax, tmin, rs, elevation, albedo = 0.23, x = NULL) {
            tmax <- brick(tmax)
            tmin <- brick(tmin)
            rs <- brick(rs)
            elevation <- brick(elevation)
            if (is.character(albedo)) {
              if (!file.exists(albedo)) {
                stop("Albedo file does not exist: ", albedo)
              }
              albedo <- brick(albedo)
            } else if (!is.numeric(albedo)) {
              stop("Argument 'albedo' must be a numeric value or a file path to a raster.")
            }
            
            calc_rn(tmax, tmin, rs, elevation, albedo)
          })
