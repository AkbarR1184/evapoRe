#' Calculate Potential Evapotranspiration (PET) using Baier-Robertson method
#'
#' The function \code{baier_robertson} computes PET using the Baier and Robertson method.
#'
#' @details
#' For Raster inputs, provide raster objects or file paths for maximum (`tmax`)
#'  and minimum (`tmin`) temperature.For `data.table` input, provide a table with 
#'  columns: "lon", "lat", "date", "tmax", and "tmin".
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom raster brick calc getZ setZ nlayers
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom lubridate day leap_year month
#' @param tmax Raster* object or file path; maximum temperature (°C)
#' @param tmin Raster* object or file path; minimum temperature (°C)
#' @param x A `data.table` with columns: "lon", "lat", "date", "tmax", "tmin".
#' @return RasterBrick or data.table of PET values (mm/day)
#' @keywords internal

setGeneric("baier_robertson", function(tmax, tmin, x = NULL) {
  standardGeneric("baier_robertson")
})

#' @rdname baier_robertson
setMethod("baier_robertson",
          signature(tmax = "Raster", tmin = "Raster"),
          function(tmax, tmin, x = NULL) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 || is.na(no_cores)) no_cores <- 1
            registerDoParallel(cores = no_cores)
            re <- esr(tmax)
            t_dates <- getZ(tmax)
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
              dummie_tx <- tmax[[t_idx[index]]]
              dummie_tn <- tmin[[t_idx[index]]]
              dummie_re <- re[[re_idx[index]]]
              dummie_br <- (0.157 * dummie_tx) + (0.158 * (dummie_tx - dummie_tn)) + (0.109 * dummie_re) - 5.39
              dummie_br <- calc(dummie_br, function(x) { x[x < 0] <- NA; x })
              return(dummie_br)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })

#' @rdname baier_robertson
setMethod("baier_robertson",
          signature(tmax = "character", tmin = "character"),
          function(tmax, tmin, x = NULL) {
            baier_robertson(tmax = brick(tmax),
                            tmin = brick(tmin))
          })

#' @rdname baier_robertson
setMethod("baier_robertson",
          signature(tmax = "missing", tmin = "missing"),
          function(x) {
            dummie_params <- pet_params_calc(x)
            x[, pet := (0.157 * tmax) + (0.158 * (tmax - tmin)) +
                (0.109 * dummie_params[.SD, ext_rad, on = .(lat, date)]) - 5.39]
            x[, pet := fifelse(pet < 0, 0, pet)]
            return(x[, .(lon, lat, date, value = pet)])
          })
