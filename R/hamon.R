#' PET calculation by Hamon method
#'
#' The function \code{hamon} computes Potential Evapotranspiration (PET) by Hamon method.
#'
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#'
#' If `x` is a filename, it should point to a *.nc file.

#' 
#' @import  data.table
#' @import doParallel
#' @import foreach
#' @import parallel
#' @importFrom methods as
#' @importFrom lubridate day days_in_month leap_year
#' @importFrom raster brick calc getZ init nlayers reclassify setZ
#' @param x Raster* object; data.table (see details); filename (character; see details)
#' @return Raster* object; data.table
#' @keywords internal

setGeneric("hamon", function(x) standardGeneric("hamon"))

#' @rdname hamon
#' @method hamon Raster

setMethod("hamon", "Raster",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            tavg <- x
            dl <- day_length(x)
            t_dates <- getZ(x)
            dl_dates <- getZ(dl)
            t_dates_table <- as.data.table(t_dates) %>% 
              .[, leap := leap_year(t_dates)] %>% .[, mo := month(t_dates)] %>%
              .[, dd := day(t_dates)]
            dl_dates_table <- as.data.table(dl_dates) %>%
              .[, leap := leap_year(dl_dates)] %>% .[, mo := month(dl_dates)] %>%
              .[, dd := day(dl_dates)]
            dummie_dates_table <- merge(t_dates_table, dl_dates_table,
                                        by = c('leap', 'mo', 'dd'))
            setorder(dummie_dates_table, 't_dates')
            t_idx <- match(dummie_dates_table$t_dates, t_dates)
            dl_idx <- match(dummie_dates_table$dl_dates, dl_dates)
            dummie_pet <- foreach(index = 1:length(t_idx)) %dopar% {
              t_layer <- t_idx[index]
              dl_layer <- dl_idx[index]
              dummie_t <- tavg[[t_layer]]
              dummie_dl <- dl[[dl_layer]]
              es <- 6.108*exp(17.27*dummie_t/(dummie_t + 273.3))
              dummie_h <- 0.165*216.7*(dummie_dl/12)*es/(dummie_t + 273.3)
              dummie_h <- calc(dummie_h, fun = function(val) {
                val[val < 0] <- NA
                return(val)
              })
              return(dummie_h)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })
#' @rdname hamon
#' @method hamon data.table

setMethod("hamon", "data.table",
          function(x) {
            dummie_params <- pet_params_calc(x)
            
            x[, value := 0.1651 * 216.7 * (2 * dummie_params[.SD, on = .(lat, date), omega*nday] / pi) * 
                (6.108 * exp(17.27 * value / (value + 237.3)) / (value + 273.3))]
            x[, value := fifelse(value > 0, value, NA)]
            
            return(x)
          })

#' @rdname hamon
#' @method hamon character

setMethod("hamon", "character",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie_brick <- brick(x)
            tavg <- dummie_brick
            dl <- day_length(dummie_brick)
            t_dates <- getZ(dummie_brick)
            dl_dates <- getZ(dl)
            t_dates_table <- as.data.table(t_dates) %>% 
              .[, leap := leap_year(t_dates)] %>% .[, mo := month(t_dates)] %>%
              .[, dd := day(t_dates)]
            dl_dates_table <- as.data.table(dl_dates) %>%
              .[, leap := leap_year(dl_dates)] %>% .[, mo := month(dl_dates)] %>%
              .[, dd := day(dl_dates)]
            dummie_dates_table <- merge(t_dates_table, dl_dates_table,
                                        by = c('leap', 'mo', 'dd'))
            setorder(dummie_dates_table, 't_dates')
            t_idx <- match(dummie_dates_table$t_dates, t_dates)
            dl_idx <- match(dummie_dates_table$dl_dates, dl_dates)
            dummie_pet <- foreach(index = 1:length(t_idx)) %dopar% {
              t_layer <- t_idx[index]
              dl_layer <- dl_idx[index]
              dummie_t <- tavg[[t_layer]]
              dummie_dl <- dl[[dl_layer]]
              es <- 6.108*exp(17.27*dummie_t/(dummie_t + 273.3))
              dummie_h <- 0.165*216.7*(dummie_dl/12)*es/(dummie_t + 273.3)
              dummie_h <- calc(dummie_h, fun = function(val) {
                val[val < 0] <- NA
                return(val)
              })
              return(dummie_h)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })
