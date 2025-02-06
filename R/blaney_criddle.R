#' PET calculation by Blaney Criddle method
#'
#' The function \code{blaney_criddle} computes Potential Evapotranspiration (PET) by blaney_criddle method.
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

setGeneric("blaney_criddle", function(x) standardGeneric("blaney_criddle"))

#' @rdname blaney_criddle
#' @method blaney_criddle Raster

setMethod("blaney_criddle", "Raster",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))
               (no_cores <- 1)
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
              dummie_bc <- 0.85*100*dummie_dl*(0.46*dummie_t + 8.13)/(365*12)
              dummie_bc <- calc(dummie_bc, fun = function(val) {
                val[val < 0] <- NA
                return(val)
              })
              return(dummie_bc)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })
#' @rdname blaney_criddle
#' @method blaney_criddle data.table

setMethod("blaney_criddle", "data.table",
          function(x) {
            dummie_params <- pet_params_calc(x)
            x[, value := 0.825 * (100 * 24 * dummie_params[.SD, on = .(lat, date), omega*nday]) * 
                (0.46 * value + 8.13) / (pi * 365 * 12)]
            x[, value := fifelse(value >= 0, value, NA)]
            
            return(x)
          })

#' @rdname blaney_criddle
#' @method blaney_criddle character

setMethod("blaney_criddle", "character",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))
               (no_cores <- 1)
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
              dummie_bc <- 0.85*100*dummie_dl*(0.46*dummie_t + 8.13)/(365*12)
              dummie_bc <- calc(dummie_bc, fun = function(val) {
                val[val < 0] <- NA
                return(val)
              })
              return(dummie_bc)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })
