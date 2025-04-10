#' Calculate Potential Evapotranspiration (PET) using Hamon method
#'
#' The function \code{hamon} computes PET using the Hamon method.
#'
#' @details
#' For Raster input, provide a raster object or file path for average temperature.
#' For `data.table` input, provide a table with columns: "lon", "lat", "date", and "tavg".
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom raster brick calc getZ setZ nlayers
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom lubridate day leap_year month
#'
#' @param x Raster* object, character file path, or data.table (see details)
#'
#' @return RasterBrick or data.table of PET values (mm/day)
#' @keywords internal

setGeneric("hamon", function(x) standardGeneric("hamon"))

#' @rdname hamon
setMethod("hamon", "Raster",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 || is.na(no_cores)) no_cores <- 1
            registerDoParallel(cores = no_cores)
            tavg <- x
            dl <- day_length(x)
            t_dates <- getZ(x)
            dl_dates <- getZ(dl)
            t_dates_table <- data.table(t_dates)[, `:=`(leap = leap_year(t_dates),
                                                        mo = month(t_dates),
                                                        dd = day(t_dates))]
            dl_dates_table <- data.table(dl_dates)[, `:=`(leap = leap_year(dl_dates),
                                                          mo = month(dl_dates),
                                                          dd = day(dl_dates))]
            dummie_dates_table <- merge(t_dates_table, dl_dates_table, by = c("leap", "mo", "dd"))
            setorder(dummie_dates_table, t_dates)
            t_idx <- match(dummie_dates_table$t_dates, t_dates)
            dl_idx <- match(dummie_dates_table$dl_dates, dl_dates)
            dummie_pet <- foreach(index = seq_along(t_idx)) %dopar% {
              dummie_ta <- tavg[[t_idx[index]]]
              dummie_dl <- dl[[dl_idx[index]]]
              es <- 6.108 * exp(17.27 * dummie_ta / (dummie_ta + 237.3))
              dummie_hm <- 0.165 * 216.7 * (dummie_dl / 12) * es / (dummie_ta + 273.3)
              dummie_hm <- calc(dummie_hm, function(x) { x[x < 0] <- NA; x })
              return(dummie_hm)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })

#' @rdname hamon
setMethod("hamon", "character",
          function(x){
            hamon(brick(x))
          })

#' @rdname hamon
setMethod("hamon", "data.table",
          function(x) {
            dummie_params <- pet_params_calc(x)
            x[, pet := 0.165 * 216.7 * (2 * dummie_params[.SD, on = .(lat, date), omega] / pi) *
                (6.108 * exp(17.27 * tavg / (tavg + 237.3)) / (tavg + 273.3))]
            x[, pet := fifelse(pet < 0, NA_real_, pet)]
            return(x[, .(lon, lat, date, value = pet)])
          })
