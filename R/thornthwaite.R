#' Calculate Potential Evapotranspiration (PET) using Thornthwaite method
#'
#' The function \code{thornthwaite} computes PET using the Thornthwaite method.
#'
#' @details
#' For Raster input, provide a raster object or file path for average temperature.
#' For `data.table` input, provide a table with columns: "lon", "lat", "date", and "tavg".
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom raster brick calc getZ setZ nlayers
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom lubridate day leap_year month year
#' @param x Raster* object, character file path, or data.table (see details)
#' @return RasterBrick or data.table of PET values (mm/day)
#' @keywords internal

setGeneric("thornthwaite", function(x) standardGeneric("thornthwaite"))

#' @rdname thornthwaite
setMethod("thornthwaite", "Raster",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 || is.na(no_cores)) no_cores <- 1
            registerDoParallel(cores = no_cores)
            tavg <- x
            hi <- heat_index(x)
            dl <- day_length(x)
            t_dates <- getZ(x)
            hi_dates <- getZ(hi)
            dl_dates <- getZ(dl)
            t_dates_table <- data.table(t_dates)[, `:=`(leap = leap_year(t_dates),
                                                        mo = month(t_dates),
                                                        dd = day(t_dates),
                                                        year_idx = year(t_dates))]
            dl_dates_table <- data.table(dl_dates)[, `:=`(leap = leap_year(dl_dates),
                                                          mo = month(dl_dates),
                                                          dd = day(dl_dates),
                                                          year_idx = year(dl_dates))]
            hi_dates_table <- data.table(hi_dates)[, year_idx := year(hi_dates)]
            dummie_dates_table_dl <- merge(t_dates_table, dl_dates_table, by = c("leap", "mo", "dd"))
            dummie_dates_table_hi <- merge(t_dates_table, hi_dates_table, by = "year_idx")
            setorder(dummie_dates_table_dl, t_dates)
            setorder(dummie_dates_table_hi, t_dates)
            t_idx <- match(dummie_dates_table_dl$t_dates, t_dates)
            dl_idx <- match(dummie_dates_table_dl$dl_dates, dl_dates)
            hi_idx <- match(dummie_dates_table_hi$hi_dates, hi_dates)
            dummie_pet <- foreach(index = seq_along(t_idx)) %dopar% {
              dummie_ta <- tavg[[t_idx[index]]]
              dummie_dl <- dl[[dl_idx[index]]]
              dummie_hi <- hi[[hi_idx[index]]]
              dummie_k <- 0.49239 + (1.792 * dummie_hi * 1e-2) - (0.771 * (dummie_hi^2) * 1e-4)
              dummie_th <- 16 * (dummie_dl / 360) * (10 * dummie_ta / dummie_hi)^dummie_k
              dummie_th <- calc(dummie_th, function(x) { x[x < 0] <- NA; x })
              return(dummie_th)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })

#' @rdname thornthwaite
setMethod("thornthwaite", "character",
          function(x){
            thornthwaite(brick(x))
          })

#' @rdname thornthwaite
setMethod("thornthwaite", "data.table",
          function(x) {
            dummie_params <- pet_params_calc(x)
            x[, mon_heat := (tavg / 5)^1.514]
            x[, ann_heat := sum(mon_heat, na.rm = TRUE), by = .(lon, lat, year(date))]
            x[, setdiff(names(x), c("lon", "lat", "date", "tavg", "ann_heat")) := NULL]
            x[, k := 0.49239 + (1.792 * ann_heat * 1e-2) - (0.771 * (ann_heat^2) * 1e-4) + (675 * (ann_heat^3) * 1e-9)]
            x[, pet := (16 * 24 * dummie_params[.SD, on = .(lat, date), omega]) / (pi * 360) * (10 * tavg / ann_heat)^k]
            x[, pet := fifelse(pet < 0, NA_real_, pet)]
            return(x[, .(lon, lat, date, value = pet)])
          })