#' Calculate Potential Evapotranspiration (PET) using the Turc Method
#'
#' The function \code{turc} Computes PET using the Turc formula.

#'
#' @details
#' For Raster inputs, provide raster objects or file paths for `tavg`, `rs`, 
#' and `rh`. For data.table input, provide a single `data.table` with columns: "lon", "lat", "date", 
#' "tavg", "rs", and "rh".
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom raster brick calc getZ setZ nlayers
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#'
#' @param tavg Raster* object or file path; average temperature (°C)
#' @param rs Raster* object or file path; shortwave radiation (MJ m-2 day-1)
#' @param rh Raster* object or file path; relative humidity (percent)
#' @param x A `data.table` with columns: "lon", "lat", "date", "tavg", "rs", and "rh"
#' 
#' @return RasterBrick or data.table of PET values (mm/day)
#' @keywords internal

setGeneric("turc", function(tavg, rs, rh, x = NULL) {
  standardGeneric("turc")
})

#' @rdname turc
setMethod("turc",
          signature(tavg = "missing", rs = "missing", rh = "missing"),
          function(x) {
            x[, c := ifelse(rh >= 50, 1, 1 + ((50 - rh) / 70))]
            x[, pet := 0.31 * c * (rs + 2.094) * (tavg / (tavg + 15))]
            x[, pet := fifelse(pet < 0, NA_real_, pet)]
            return(x[, .(lon, lat, date, value=pet)])
          })

#' @rdname turc
setMethod("turc",
          signature(tavg = "Raster", rs = "Raster", rh = "Raster"),
          function(tavg, rs, rh, x = NULL) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 || is.na(no_cores)) no_cores <- 1
            registerDoParallel(cores = no_cores)
            dummie_pet <- foreach(layer_index = 1:nlayers(tavg)) %dopar% {
              dummie_ta <- tavg[[layer_index]]
              dummie_rs <- rs[[layer_index]]
              dummie_rh <- rh[[layer_index]]
              dummie_c <- calc(dummie_rh, fun = function(x) {
                ifelse(x >= 50, 1, 1 + ((50 - x) / 70))
              })
              dummie_o <- 0.31 * dummie_c * (dummie_rs + 2.094) * (dummie_ta / (dummie_ta + 15))
              dummie_o <- calc(dummie_o, function(x) { x[x < 0] <- NA; x })
              return(dummie_o)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, getZ(tavg))
            return(dummie_pet)
          })

#' @rdname turc
setMethod("turc",
          signature(tavg = "character", rs = "character", rh = "character"),
          function(tavg, rs, rh, x = NULL) {
            dummie_ta <- brick(tavg)
            dummie_rs <- brick(rs)
            dummie_rh <- brick(rh)
            turc(
              tavg = dummie_ta,
              rs = dummie_rs,
              rh = dummie_rh
            )
          })