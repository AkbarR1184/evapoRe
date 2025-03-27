#' PET calculation by Hargreaves and Samani method
#'
#' The function \code{hargreaves_samani} computes Potential Evapotranspiration (PET) by Hargreaves and Samani method.
#'
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", "tavg", "tmin", and "tmax".
#'
#' If `x`, `y`,`z` is a filename, it should point to a *.nc file.

#' @import  data.table
#' @import doParallel
#' @import foreach
#' @import parallel
#' @importFrom methods as
#' @importFrom lubridate day days_in_month leap_year
#' @importFrom raster brick calc getZ init nlayers reclassify setZ
#' @param x Raster* object; data.table (see details); filename (character; see details)
#' @param y Raster* object or filename; maximum temperature data (required for Raster and character inputs)
#' @param z Raster* object or filename; minimum temperature data (required for Raster and character inputs)
#' @return Raster* object; data.table
#' @keywords internal
setGeneric("hargreaves_samani", function(x, y = NULL, z = NULL) {
  standardGeneric("hargreaves_samani")
})
#' @rdname hargreaves_samani
#' @method hargreaves_samani Raster

setMethod("hargreaves_samani", signature(x = "Raster", y = "Raster", z = "Raster"),
          function(x, y, z) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))
               (no_cores <- 1)
            registerDoParallel(cores = no_cores)
            tavg <- x
            tmax <- y
            tmin <- z
            re <- esr(tavg)
            t_dates <- getZ(tavg)
            re_dates <- getZ(re)
            t_dates_table <- as.data.table(t_dates) %>%
              .[, leap := leap_year(t_dates)] %>%
              .[, mo := month(t_dates)] %>%
              .[, dd := day(t_dates)]
            re_dates_table <- as.data.table(re_dates) %>%
              .[, leap := leap_year(re_dates)] %>%
              .[, mo := month(re_dates)] %>%
              .[, dd := day(re_dates)]
            dummie_dates_table <- merge(t_dates_table, re_dates_table, by = c('leap', 'mo', 'dd'))
            setorder(dummie_dates_table, 't_dates')
            t_idx <- match(dummie_dates_table$t_dates, t_dates)
            re_idx <- match(dummie_dates_table$re_dates, re_dates)
            dummie_pet <- foreach(index = 1:length(t_idx)) %dopar% {
              t_layer <- t_idx[index]
              re_layer <- re_idx[index]
              dummie_tavg <- tavg[[t_layer]]
              dummie_tmax <- tmax[[t_layer]]
              dummie_tmin <- tmin[[t_layer]]
              dummie_re <- re[[re_layer]]
              lambda <- 2.501 - 0.002361 * dummie_tavg
              dummie_o <- (0.0023 * dummie_re * sqrt(abs(dummie_tmax - dummie_tmin)) * (dummie_tavg + 17.8)) / lambda
              dummie_o <- calc(dummie_o, fun = function(val) {
                val[val < 0] <- NA
                return(val)
              })
              return(dummie_o)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })
#' @rdname hargreaves_samani
#' @method hargreaves_samani data.table

setMethod("hargreaves_samani", signature(x = "data.table", y = "missing", z = "missing"),
          function(x) {
            esr <- pet_params_calc(x)
            x[, pet_hs := 0.0023 * esr[.SD, on = .(lat, date), ext_rad] *
                sqrt(abs(tmax - tmin)) * (tavg + 17.8) / (2.501 - 0.002361 * tavg)]
            x[, pet_hs := fifelse(pet_hs > 0, pet_hs, NA)]
            x <- x[, .(lon, lat, date, value = pet_hs)]
            return(x)
          })

#' @rdname hargreaves_samani
#' @method hargreaves_samani character

setMethod("hargreaves_samani", signature(x = "character", y = "character", z = "character"),
          function(x, y, z) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            tavg <- brick(x)
            tmax <- brick(y)
            tmin <- brick(z)
            re <- esr(tavg)
            t_dates <- getZ(tavg)
            re_dates <- getZ(re)
            t_dates_table <- as.data.table(t_dates) %>%
              .[, leap := leap_year(t_dates)] %>%
              .[, mo := month(t_dates)] %>%
              .[, dd := day(t_dates)]
            re_dates_table <- as.data.table(re_dates) %>%
              .[, leap := leap_year(re_dates)] %>%
              .[, mo := month(re_dates)] %>%
              .[, dd := day(re_dates)]
            dummie_dates_table <- merge(t_dates_table, re_dates_table, by = c('leap', 'mo', 'dd'))
            setorder(dummie_dates_table, 't_dates')
            t_idx <- match(dummie_dates_table$t_dates, t_dates)
            re_idx <- match(dummie_dates_table$re_dates, re_dates)
            dummie_pet <- foreach(index = 1:length(t_idx)) %dopar% {
              t_layer <- t_idx[index]
              re_layer <- re_idx[index]
              dummie_tavg <- tavg[[t_layer]]
              dummie_tmax <- tmax[[t_layer]]
              dummie_tmin <- tmin[[t_layer]]
              dummie_re <- re[[re_layer]]
              lambda <- 2.501 - 0.002361 * dummie_tavg
              dummie_o <- (0.0023 * dummie_re * sqrt(abs(dummie_tmax - dummie_tmin)) * (dummie_tavg + 17.8)) / lambda
              dummie_o <- calc(dummie_o, fun = function(val) {
                val[val < 0] <- NA
                return(val)
              })
              return(dummie_o)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })