#' PET calculation by Jensen Haise method
#'
#' The function \code{jensen_haise} computes Potential Evapotranspiration (PET) by Jensen Haise method.
#'
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value".
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

setGeneric("jensen_haise", function(x) standardGeneric("jensen_haise"))

#' @rdname jensen_haise
#' @method jensen_haise Raster

setMethod("jensen_haise", "Raster",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))
              (no_cores <- 1)
            registerDoParallel(cores = no_cores)
            tavg <- x
            re <- esr(x)
            t_dates <- getZ(x)
            re_dates <- getZ(re)
            t_dates_table <- as.data.table(t_dates) %>% 
              .[, leap := leap_year(t_dates)] %>% .[, mo := month(t_dates)] %>%
              .[, dd := day(t_dates)]
            re_dates_table <- as.data.table(re_dates) %>%
              .[, leap := leap_year(re_dates)] %>% .[, mo := month(re_dates)] %>%
              .[, dd := day(re_dates)]
            dummie_dates_table <- merge(t_dates_table, re_dates_table,
                                        by = c('leap', 'mo', 'dd'))
            setorder(dummie_dates_table, 't_dates')
            t_idx <- match(dummie_dates_table$t_dates, t_dates)
            re_idx <- match(dummie_dates_table$re_dates, re_dates)
            dummie_pet <- foreach(index = 1:length(t_idx)) %dopar% {
              t_layer <- t_idx[index]
              re_layer <- re_idx[index]
              dummie_t <- tavg[[t_layer]]
              dummie_re <- re[[re_layer]]
              lambda <- 2.501 - 0.002361*dummie_t
              dummie_jh <- dummie_re*dummie_t/(40*lambda)
              dummie_jh <- calc(dummie_jh, fun = function(val) {
                val[val < 0] <- NA
                return(val)
              })
              return(dummie_jh)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })
#' @rdname jensen_haise
#' @method jensen_haise data.table

setMethod("jensen_haise", "data.table",
          function(x) {
            dummie_params <- pet_params_calc(x)
            x[, value := fifelse(
              value > 0, 
              dummie_params[.SD, ext_rad * nday, on = .(lat, date)]
              * (value) / ((2.501 - 0.002361 * value) * 40), 
              NA
            )]
            
            return(x)
          })

#' @rdname jensen_haise
#' @method jensen_haise character

setMethod("jensen_haise", "character",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie_brick <- brick(x)
            tavg <- dummie_brick
            re <- esr(dummie_brick)
            t_dates <- getZ(dummie_brick)
            re_dates <- getZ(re)
            t_dates_table <- as.data.table(t_dates) %>% 
              .[, leap := leap_year(t_dates)] %>% .[, mo := month(t_dates)] %>%
              .[, dd := day(t_dates)]
            re_dates_table <- as.data.table(re_dates) %>%
              .[, leap := leap_year(re_dates)] %>% .[, mo := month(re_dates)] %>%
              .[, dd := day(re_dates)]
            dummie_dates_table <- merge(t_dates_table, re_dates_table,
                                        by = c('leap', 'mo', 'dd'))
            setorder(dummie_dates_table, 't_dates')
            t_idx <- match(dummie_dates_table$t_dates, t_dates)
            re_idx <- match(dummie_dates_table$re_dates, re_dates)
            dummie_pet <- foreach(index = 1:length(t_idx)) %dopar% {
              t_layer <- t_idx[index]
              re_layer <- re_idx[index]
              dummie_t <- tavg[[t_layer]]
              dummie_re <- re[[re_layer]]
              lambda <- 2.501 - 0.002361*dummie_t
              dummie_jh <- dummie_re*dummie_t/(40*lambda)
              dummie_jh <- calc(dummie_jh, fun = function(val) {
                val[val < 0] <- NA
                return(val)
              })
              return(dummie_jh)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })
