#' Calculate Potential Evapotranspiration (PET) using Hargreaves-Samani method
#'
#' The function \code{hargreaves_samani} computes PET using the Hargreaves and Samani method.
#'
#' @details
#' For Raster inputs, provide raster objects or file paths for average (`tavg`),
#' maximum (`tmax`), and minimum (`tmin`) temperature. For `data.table` input,
#' provide a table with columns: "lon", "lat", "date", "tavg", "tmin", and "tmax".
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom raster brick calc getZ setZ nlayers
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom lubridate day leap_year month
#'
#' @param tavg Raster* object or file path; average temperature (°C)
#' @param tmax Raster* object or file path; maximum temperature (°C)
#' @param tmin Raster* object or file path; minimum temperature (°C)
#' @param x A `data.table` with columns: "lon", "lat", "date", "tavg", "tmin", "tmax".
#'
#' @return RasterBrick or data.table of PET values (mm/day)
#' @keywords internal

setGeneric("hargreaves_samani", function(tavg, tmax, tmin, x = NULL) {
  standardGeneric("hargreaves_samani")
})

#' @rdname hargreaves_samani
setMethod("hargreaves_samani",
          signature(tavg = "Raster", tmax = "Raster", tmin = "Raster"),
          function(tavg, tmax, tmin, x = NULL) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 || is.na(no_cores)) no_cores <- 1
            registerDoParallel(cores = no_cores)
            re <- esr(tavg)
            t_dates <- getZ(tavg)
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
              dummie_tx <- tmax[[t_idx[index]]]
              dummie_tn <- tmin[[t_idx[index]]]
              dummie_re <- re[[re_idx[index]]]
              lambda <- 2.501 - 0.002361 * dummie_ta
              dummie_hs <- (0.0023 * dummie_re * sqrt(abs(dummie_tx - dummie_tn)) * (dummie_ta + 17.8)) / lambda
              dummie_hs <- calc(dummie_hs, function(x) { x[x < 0] <- NA; x })
              return(dummie_hs)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, t_dates)
            return(dummie_pet)
          })

#' @rdname hargreaves_samani
setMethod("hargreaves_samani",
          signature(tavg = "character", tmax = "character", tmin = "character"),
          function(tavg, tmax, tmin, x = NULL) {
            hargreaves_samani(tavg = brick(tavg),
                              tmax = brick(tmax),
                              tmin = brick(tmin))
          })

#' @rdname hargreaves_samani
setMethod("hargreaves_samani",
          signature(tavg = "missing", tmax = "missing", tmin = "missing"),
          function(x) {
            dummie_params <- pet_params_calc(x)
            x[, lambda := 2.501 - 0.002361 * tavg]
            x[, pet := 0.0023 * dummie_params[.SD, on = .(lat, date), ext_rad] *
                sqrt(abs(tmax - tmin)) * (tavg + 17.8) / lambda]
            x[, pet := fifelse(pet < 0, NA_real_, pet)]
            return(x[, .(lon, lat, date, value = pet)])
          })
