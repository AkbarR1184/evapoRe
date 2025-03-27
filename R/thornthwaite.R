#' PET calculation by Thornthwaite method
#'
#' The function \code{thornthwaite} computes Potential Evapotranspiration (PET) by Thornthwaite  method.
#'
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#'
#' If `x` is a filename, it should point to a *.nc file.

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

setGeneric("thornthwaite", function(x) standardGeneric("thornthwaite"))

#' @rdname thornthwaite 
#' @method thornthwaite  Raster

setMethod("thornthwaite", "Raster",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))
               (no_cores <- 1)
            registerDoParallel(cores = no_cores)
            tavg <- x
            hi <- heat_index(x)
            dl <- day_length(x)
            t_dates <- getZ(x)
            hi_dates <- getZ(hi)
            dl_dates <- getZ(dl)
            t_dates_table <- as.data.table(t_dates) %>% 
              .[, leap := leap_year(t_dates)] %>% .[, mo := month(t_dates)] %>%
              .[, dd := day(t_dates)] %>% .[, year_idx := year(t_dates)]
            dl_dates_table <- as.data.table(dl_dates) %>%
              .[, leap := leap_year(dl_dates)] %>% .[, mo := month(dl_dates)] %>%
              .[, dd := day(dl_dates)] %>% .[, year_idx := year(dl_dates)]
            hi_dates_table <- as.data.table(hi_dates) %>%
              .[, year_idx := year(hi_dates)] 
            dummie_dates_table_dl <- merge(t_dates_table, dl_dates_table,
                                           by = c('leap', 'mo', 'dd'))
            dummie_dates_table_hi <- merge(t_dates_table, hi_dates_table,
                                           by = c('year_idx'))
            setorder(dummie_dates_table_dl, 't_dates')
            setorder(dummie_dates_table_hi, 't_dates')
            t_idx <- match(dummie_dates_table_dl$t_dates, t_dates)
            dl_idx <- match(dummie_dates_table_dl$dl_dates, dl_dates)
            hi_idx <- match(dummie_dates_table_hi$hi_dates, hi_dates)
            dummie_pet <- foreach(index = 1:length(t_idx)) %dopar% {
              t_layer <- t_idx[index]
              dl_layer <- dl_idx[index]
              hi_layer <- hi_idx[index]
              dummie_t <- tavg[[t_layer]]
              dummie_dl <- dl[[dl_layer]]
              dummie_hi <- hi[[hi_layer]]
              dummie_k <- 0.49239 + (1.792*dummie_hi*1e-2) - (0.771*(dummie_hi^2)*1e-4)
              dummie_th <- 16*(dummie_dl/360)*(10*dummie_t/dummie_hi)^dummie_k
              dummie_th <- calc(dummie_th, fun = function(val) {
                val[val < 0] <- NA
                return(val)
              })
              return(dummie_th)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })
#' @rdname thornthwaite 
#' @method thornthwaite  data.table

setMethod("thornthwaite", "data.table",
          function(x) {
            dummie_params <- pet_params_calc(x)
            x[, mon_heat := (value/5)^1.514]
            x[, ann_heat := sum(mon_heat, na.rm = TRUE), by = .(lon, lat, year(date))]
            x[, setdiff(names(x), c("lon", "lat","date", "value","ann_heat")) := NULL]
            x[, k := 0.49239 + (1.792 * ann_heat * 1e-2) - (0.771 * (ann_heat ^
                                                                       2) * 1e-4) + (675 * (ann_heat ^ 3) * 1e-9)]
            x[, value := (16 * 24 * dummie_params[.SD, on = .(lat, date), omega ]) / (pi * 360) * (10 * value / ann_heat) ^ k]
            x[, setdiff(names(x), c("lon", "lat","date", "value")) := NULL]
            return(x)
          })

#' @rdname thornthwaite 
#' @method thornthwaite  character

setMethod("thornthwaite", "character",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie_brick <- brick(x)
            tavg <- dummie_brick
            hi <- heat_index(dummie_brick)
            dl <- day_length(dummie_brick)
            t_dates <- getZ(dummie_brick)
            hi_dates <- getZ(hi)
            dl_dates <- getZ(dl)
            t_dates_table <- as.data.table(t_dates) %>% 
              .[, leap := leap_year(t_dates)] %>% .[, mo := month(t_dates)] %>%
              .[, dd := day(t_dates)] %>% .[, year_idx := year(t_dates)]
            dl_dates_table <- as.data.table(dl_dates) %>%
              .[, leap := leap_year(dl_dates)] %>% .[, mo := month(dl_dates)] %>%
              .[, dd := day(dl_dates)] %>% .[, year_idx := year(dl_dates)]
            hi_dates_table <- as.data.table(hi_dates) %>%
              .[, year_idx := year(hi_dates)] 
            dummie_dates_table_dl <- merge(t_dates_table, dl_dates_table,
                                           by = c('leap', 'mo', 'dd'))
            dummie_dates_table_hi <- merge(t_dates_table, hi_dates_table,
                                           by = c('year_idx'))
            setorder(dummie_dates_table_dl, 't_dates')
            setorder(dummie_dates_table_hi, 't_dates')
            t_idx <- match(dummie_dates_table_dl$t_dates, t_dates)
            dl_idx <- match(dummie_dates_table_dl$dl_dates, dl_dates)
            hi_idx <- match(dummie_dates_table_hi$hi_dates, hi_dates)
            dummie_pet <- foreach(index = 1:length(t_idx)) %dopar% {
              t_layer <- t_idx[index]
              dl_layer <- dl_idx[index]
              hi_layer <- hi_idx[index]
              dummie_t <- tavg[[t_layer]]
              dummie_dl <- dl[[dl_layer]]
              dummie_hi <- hi[[hi_layer]]
              dummie_k <- 0.49239 + (1.792*dummie_hi*1e-2) - (0.771*(dummie_hi^2)*1e-4)
              dummie_th <- 16*(dummie_dl/360)*(10*dummie_t/dummie_hi)^dummie_k
              dummie_th <- calc(dummie_th, fun = function(val) {
                val[val < 0] <- NA
                return(val)
              })
              return(dummie_th)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })
