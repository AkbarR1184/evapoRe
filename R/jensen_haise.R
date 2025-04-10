#' Calculate Potential Evapotranspiration (PET) using Jensen-Haise method
#'
#' The function \code{jensen_haise} computes PET using the Jensen-Haise method.
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

setGeneric("jensen_haise", function(x) standardGeneric("jensen_haise"))

#' @rdname jensen_haise
setMethod("jensen_haise", "Raster",
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
              dummie_jh <- dummie_re * dummie_ta / (40 * lambda)
              dummie_jh <- calc(dummie_jh, function(x) { x[x < 0] <- NA; x })
              return(dummie_jh)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })

#' @rdname jensen_haise
setMethod("jensen_haise", "character",
          function(x){
            jensen_haise(brick(x))
          })

#' @rdname jensen_haise
setMethod("jensen_haise", "data.table",
          function(x) {
            dummie_params <- pet_params_calc(x)
            x[, lambda := 2.501 - 0.002361 * tavg]
            x[, pet := fifelse(
              tavg >= 0,
              dummie_params[.SD, ext_rad, on = .(lat, date)] 
              * tavg / (lambda * 40),NA)]
            x[, pet := fifelse(pet < 0, NA_real_, pet)]
            return(x[, .(lon, lat, date, value = pet)])
          })
