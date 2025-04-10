#' Calculate Potential Evapotranspiration (PET) using Priestley-Taylor Method
#'
#' The function \code{priestley_taylor} computes PET by Priestley-Taylor method.
#' @details
#' For Raster inputs, provide raster objects or file paths for `tavg`, `rn`, 
#' and either `elevation` or `pres`. For `data.table` input, provide a single 
#' `data.table` with columns: "lon", "lat", "date", "tavg", "rn", and either 
#' "elevation" or "pres".
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom raster brick calc getZ setZ nlayers
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @param tavg Raster* object or file path; average temperature (°C)
#' @param rn Raster* object or file path; net radiation (MJ m-2 day-1)
#' @param elevation Raster* object or file path; elevation (m). Optional if `pres` is provided.
#' @param pres Optional. Raster* object or file path; atmospheric pressure (kPa)
#' @param x A `data.table` with columns: "lon", "lat", "date", "tavg", "rn", and either "elevation" or "pres".
#' @return RasterBrick or data.table of PET values (mm/day)
#' @keywords internal

setGeneric("priestley_taylor", function(tavg, rn, elevation = NULL, pres = NULL, x = NULL) {
  standardGeneric("priestley_taylor")
})

#' @rdname priestley_taylor
setMethod("priestley_taylor", 
          signature(tavg = "missing", rn = "missing", elevation = "missing", pres = "missing"), 
          function(x) {
            x[, lambda := 2.501 - 0.002361 * tavg]
            if (!"pres" %in% names(x)) {
              x[, pres := 101.3 * ((293 - 0.0065 * elevation) / 293)^5.26]
            }
            x[, gamma := (1.013e-3 * pres) / (0.622 * lambda)]
            x[, delta := 2503.058 * exp(17.27 * tavg / (tavg + 237.3)) / 
                ((tavg + 237.3)^2)]
            x[, pet := (1.26 * rn * delta) / (lambda * (delta + gamma))]
            x[, pet := fifelse(pet < 0, NA_real_, pet)]
            return(x[, .(lon, lat, date, value=pet)])
          })

#' @rdname priestley_taylor
setMethod("priestley_taylor", 
          signature(tavg = "Raster", rn = "Raster"), 
          function(tavg, rn, elevation = NULL, pres = NULL, x = NULL) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 || is.na(no_cores)) no_cores <- 1
            registerDoParallel(cores = no_cores)
            dummie_pet <- foreach(layer_index = 1:nlayers(tavg)) %dopar% {
              dummie_ta <- tavg[[layer_index]]
              dummie_rn <- rn[[layer_index]]
              lambda <- 2.501 - 0.002361 * dummie_ta
              dummie_pres <- if (is.null(pres)) {
                101.3 * ((293 - 0.0065 * elevation) / 293)^5.26
              } else {
                pres[[layer_index]]
              }
              gamma <- (1.013e-3 * dummie_pres) / (0.622 * lambda)
              delta <- 2503.058 * exp(17.27 * dummie_ta / (dummie_ta + 237.3)) / 
                ((dummie_ta + 237.3)^2)
              dummie_pt <- (1.26 * dummie_rn * delta) / (lambda * (delta + gamma))
              dummie_pt <- calc(dummie_pt, function(x) { x[x < 0] <- NA; x })
              return(dummie_pt)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, getZ(tavg))
            return(dummie_pet)
          })

#' @rdname priestley_taylor
setMethod("priestley_taylor", 
          signature(tavg = "character", rn = "character"), 
          function(tavg, rn, elevation = NULL, pres = NULL, x = NULL) {
            dummie_ta <- brick(tavg)
            dummie_rn <- brick(rn)
            dummie_elev <- if (is.character(elevation)) brick(elevation) else elevation
            dummie_pres <- if (is.character(pres)) brick(pres) else pres
            priestley_taylor(
              tavg = dummie_ta,
              rn = dummie_rn,
              elevation = dummie_elev,
              pres = dummie_pres
            )
          })
