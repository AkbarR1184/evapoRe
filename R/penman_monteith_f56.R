#' Calculate Potential Evapotranspiration (PET) using FAO Penman-Monteith
#'
#' The function \code{penman_monteith_f56} computes PET by FAO Penman-Monteith method.
#'
#' @details
#' For Raster inputs, provide raster objects or file paths for `tavg`, `tmin`, 
#' `tmax`, `rn`, `u`, `tdew`, and either `elevation` or `pres`. For data.table 
#' input, provide a single `data.table` with columns: "lon", "lat", "date", 
#' "tavg", "tmin", "tmax", "rn", "u", "tdew", and either "elevation" or "pres".
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom raster brick calc getZ setZ nlayers
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#'
#' @param tavg Raster* object or file path; average temperature (°C)
#' @param tmin Raster* object or file path; minimum temperature (°C)
#' @param tmax Raster* object or file path; maximum temperature (°C)
#' @param rn Raster* object or file path; net radiation (MJ m-2 day-1)
#' @param u Raster* object or file path; wind speed at 2 meters (m/s)
#' @param tdew Raster* object or file path; dew point temperature (°C)
#' @param elevation Raster* object or file path; elevation (m). Optional if `pres` is provided.
#' @param pres Optional. Raster* object or file path; atmospheric pressure (kPa)
#' @param x A `data.table` with columns: "lon", "lat", "date", 
#'   "tavg", "tmin", "tmax", "rn", "u", "tdew", and either "elevation" or "pres".
#'
#' @return RasterBrick or data.table of PET values (mm/day)
#' @keywords internal

setGeneric("penman_monteith_f56", function(tavg, tmin, tmax, rn, u, tdew, 
                                       elevation = NULL, pres = NULL, x = NULL) {
  standardGeneric("penman_monteith_f56")
})

#' @rdname penman_monteith_f56
setMethod("penman_monteith_f56", 
          signature(tavg = "missing", tmin = "missing", tmax = "missing", 
                    rn = "missing", u = "missing", tdew = "missing", 
                    elevation = "missing", pres = "missing"), 
          function(x) {
            x[, lambda := 2.501 - 0.002361 * tavg]
            if (!"pres" %in% names(x)) {
              x[, pres := 101.3 * ((293 - 0.0065 * elevation) / 293)^5.26]
            }
            x[, gamma := (1.013e-3 * pres) / (0.622 * lambda)]
            x[, delta := 2503.058 * exp(17.27 * tavg / (tavg + 237.3)) / 
                ((tavg + 237.3)^2)]
            x[, es := 0.6108 * (
              exp(17.27 * tmax / (tmax + 237.3)) + 
                exp(17.27 * tmin / (tmin + 237.3))
            ) / 2]
            x[, ea := 0.6108 * exp(17.27 * tdew / (tdew + 237.3))]
            x[, pet := (0.408 * delta * rn + gamma * (900 / (tavg + 273)) * 
                          u * (es - ea)) / 
                (delta + gamma * (1 + 0.34 * u))]
            x[, pet := fifelse(pet < 0, NA_real_, pet)]
            return(x[, .(lon, lat, date, value=pet)])
          })

#' @rdname penman_monteith_f56
setMethod("penman_monteith_f56", 
          signature(tavg = "Raster", tmin = "Raster", tmax = "Raster",
                    rn = "Raster", u = "Raster", tdew = "Raster"), 
          function(tavg, tmin, tmax, rn, u, tdew, elevation = NULL, pres = NULL, x = NULL) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 || is.na(no_cores)) no_cores <- 1
            registerDoParallel(cores = no_cores)
            dummie_pet <- foreach(layer_index = 1:nlayers(tavg)) %dopar% {
              dummie_ta <- tavg[[layer_index]]
              dummie_tx <- tmax[[layer_index]]
              dummie_tn <- tmin[[layer_index]]
              dummie_tdew <- tdew[[layer_index]]
              dummie_rn <- rn[[layer_index]]
              dummie_u <- u[[layer_index]]
              ea <- 0.6108 * exp(17.27 * dummie_tdew / (dummie_tdew + 237.3))
              lambda <- 2.501 - 0.002361 * dummie_ta
              dummie_pres <- if (is.null(pres)) {
                101.3 * ((293 - 0.0065 * elevation) / 293)^5.26
              } else {
                pres[[layer_index]]
              }
              gamma <- (1.013e-3 * dummie_pres) / (0.622 * lambda)
              delta <- 2503.058 * exp(17.27 * dummie_ta / (dummie_ta + 237.3)) / 
                ((dummie_ta + 237.3)^2)
              es <- 0.6108 * ((exp(17.27 * dummie_tx / (dummie_tx + 237.3)) + 
                                 exp(17.27 * dummie_tn / (dummie_tn + 237.3))) / 2)
              dummie_pm <- (0.408 * delta * dummie_rn + gamma * 
                              (900 / (dummie_ta + 273)) * dummie_u * (es - ea)) /
                (delta + gamma * (1 + 0.34 * dummie_u))
              
              dummie_pm <- calc(dummie_pm, function(x) { x[x < 0] <- NA; x })
              return(dummie_pm)
            }
            
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, getZ(tavg))
            return(dummie_pet)
          })

#' @rdname penman_monteith_f56
setMethod("penman_monteith_f56", 
          signature(tavg = "character", tmin = "character", tmax = "character",
                    rn = "character", u = "character", tdew = "character"), 
          function(tavg, tmin, tmax, rn, u, tdew, elevation = NULL, pres = NULL, x = NULL) {
            dummie_ta <- brick(tavg)
            dummie_tn <- brick(tmin)
            dummie_tx <- brick(tmax)
            dummie_rn <- brick(rn)
            dummie_u <- brick(u)
            dummie_tdew <- brick(tdew)
            dummie_elev <- if (is.character(elevation)) brick(elevation) else elevation
            dummie_pres <- if (is.character(pres)) brick(pres) else pres
            penman_monteith_f56(
              tavg = dummie_ta,
              tmin = dummie_tn,
              tmax = dummie_tx,
              rn = dummie_rn,
              u = dummie_u,
              tdew = dummie_tdew,
              elevation = dummie_elev,
              pres = dummie_pres
            )
          })