#' PET calculation by Baier and Robertson method
#'
#' The function \code{baier_robertson} computes Potential Evapotranspiration (PET) by Baier and Robertson method.
#'
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", "tmin", and "tmax".
#'
#' If `x`, `y`, is a filename, it should point to a *.nc file.

#' 
#' @import  data.table
#' @import doParallel
#' @import foreach
#' @import parallel
#' @importFrom methods as
#' @importFrom lubridate day days_in_month leap_year
#' @importFrom raster brick calc getZ init nlayers reclassify setZ
#' @param x Raster* object; data.table (see details); filename (character; see details)

#' @param y Raster* object or filename; minimum temperature data (required for Raster and character inputs)
#' @return Raster* object; data.table
#' @keywords internal
setGeneric("baier_robertson", function(x, y = NULL) {
  standardGeneric("baier_robertson")
})
#' @rdname baier_robertson
#' @method baier_robertson Raster

setMethod("baier_robertson", signature(x = "Raster", y = "Raster"),
          function(x, y) {
            no_cores <- max(1, floor(detectCores() * 0.75))
            registerDoParallel(cores = no_cores)
            re <- esr(x)
            t_dates <- getZ(x)
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
              dummie_tmax <- x[[t_layer]]
              dummie_tmin <- y[[t_layer]]
              dummie_re <- re[[re_layer]]
              dummie_o <- (0.157 * dummie_tmax) + (0.158 * (dummie_tmax - dummie_tmin)) + (0.109 * dummie_re) - 5.39
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

#' @rdname baier_robertson
#' @method baier_robertson data.table

setMethod("baier_robertson", signature(x = "data.table", y = "missing"),
          function(x) {
            esr <- pet_params_calc(x)
            x[, pet_br := (0.157 * tmax) + (0.158 * (tmax - tmin)) + 
             (0.109 * esr[.SD, on = .(lat, date), ext_rad * nday]) - 5.39]
              x[, pet_br := fifelse(pet_br > 0, pet_br, 0)]
              x <- x[, .(lon, lat, date, value = pet_br)]
              return(x)
          })

#' @rdname baier_robertson
#' @method baier_robertson character

setMethod("baier_robertson", signature(x = "character", y = "character"),
          function(x, y) {
            no_cores <- max(1, floor(detectCores() * 0.75))
            registerDoParallel(cores = no_cores)
            tmax <- brick(x)
            tmin <- brick(y)
            re <- esr(tmax)
            t_dates <- getZ(tmax)
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
              dummie_tmax <- tmax[[t_layer]]
              dummie_tmin <- tmin[[t_layer]]
              dummie_re <- re[[re_layer]]
              dummie_o <- (0.157 * dummie_tmax) + (0.158 * (dummie_tmax - dummie_tmin)) + (0.109 * dummie_re) - 5.39
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
