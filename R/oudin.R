#' Calculate Potential Evapotranspiration (PET) using Oudin method
#'
#' The function \code{oudin} computes PET using the Oudin method.
#'
#' @details
#' For Raster inputs, provide a raster object or file path for average temperature.
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

setGeneric("oudin", function(x) standardGeneric("oudin"))

#' @rdname oudin
setMethod("oudin", "Raster",
          function(x){
            no_cores <- detectCores() - 1
            if (no_cores < 1 || is.na(no_cores)) no_cores <- 1
            registerDoParallel(cores = no_cores)
            tavg <- x
            re <- esr(x)
            t_dates <- getZ(x)
            re_dates <- getZ(re)
            t_dates_table <- data.table(t_dates)[, `:=`(leap = leap_year(t_dates),
                                                        mo = month(t_dates),
                                                        dd = day(t_dates))]
            re_dates_table <- data.table(re_dates)[, `:=`(leap = leap_year(re_dates),
                                                          mo = month(re_dates),
                                                          dd = day(re_dates))]
            dummie_dates_table <- merge(t_dates_table, re_dates_table, by = c("leap", "mo", "dd"))
            setorder(dummie_dates_table, t_dates)
            t_idx <- match(dummie_dates_table$t_dates, t_dates)
            re_idx <- match(dummie_dates_table$re_dates, re_dates)
            dummie_pet <- foreach(index = seq_along(t_idx)) %dopar% {
              dummie_ta <- tavg[[t_idx[index]]]
              dummie_re <- re[[re_idx[index]]]
              lambda <- 2.501 - 0.002361 * dummie_ta
              dummie_o <- dummie_re * (dummie_ta + 5) / (100 * lambda)
              dummie_o <- calc(dummie_o, function(x) { x[x < 0] <- NA; x })
              return(dummie_o)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })

#' @rdname oudin
setMethod("oudin", "data.table",
          function(x) {
            dummie_params <- pet_params_calc(x)
            x[, lambda := 2.501 - 0.002361 * tavg]
            x[, pet := fifelse(tavg > -5,
                               dummie_params[.SD, ext_rad, on = .(lat, date)] *
                                 (tavg + 5) / (lambda * 100),NA)]
            x[, pet := fifelse(pet < 0, NA_real_, pet)]
            return(x[, .(lon, lat, date, value = pet)])
          })

#' @rdname oudin
setMethod("oudin", "character",
          function(x){
            oudin(brick(x))
          })
