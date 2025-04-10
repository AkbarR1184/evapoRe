#' Calculate Potential Evapotranspiration (PET) using Abtew Method
#'
#' The function \code{abtew} computes PET by Abtew method

#'
#' @details
#' For Raster inputs, provide raster objects or file paths for `tavg` and `rs`.
#' For `data.table` input, provide a single `data.table` with columns: 
#' "lon", "lat", "date", "tavg", and "rs".
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom raster brick calc getZ setZ nlayers
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#'
#' @param tavg Raster* object or file path; average temperature (°C)
#' @param rs Raster* object or file path; shortwave radiation (MJ m-2 day-1)
#' @param x A `data.table` with columns: "lon", "lat", "date", "tavg", "rs"
#'
#' @return RasterBrick or data.table of PET values (mm/day)
#' @keywords internal

setGeneric("abtew", function(tavg, rs, x = NULL) {
  standardGeneric("abtew")
})

#' @rdname abtew
setMethod("abtew",
          signature(tavg = "missing", rs = "missing"),
          function(x) {
            x[, lambda := 2.501 - 0.002361 * tavg]
            x[, pet := (0.53 * rs) / lambda]
            x[, pet := fifelse(pet < 0, NA_real_, pet)]
            return(x[, .(lon, lat, date, value=pet)])
          })

#' @rdname abtew
setMethod("abtew",
          signature(tavg = "Raster", rs = "Raster"),
          function(tavg, rs, x = NULL) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 || is.na(no_cores)) no_cores <- 1
            registerDoParallel(cores = no_cores)
            dummie_pet <- foreach(layer_index = 1:nlayers(tavg)) %dopar% {
              dummie_ta <- tavg[[layer_index]]
              dummie_rs <- rs[[layer_index]]
              lambda <- 2.501 - 0.002361 * dummie_ta
              dummie_o <- (0.53 * dummie_rs) / lambda
              dummie_o <- calc(dummie_o, function(x) { x[x < 0] <- NA; x })
              return(dummie_o)
            }
            dummie_pet <- brick(dummie_pet)
            dummie_pet <- setZ(dummie_pet, getZ(tavg))
            return(dummie_pet)
          })

#' @rdname abtew
setMethod("abtew",
          signature(tavg = "character", rs = "character"),
          function(tavg, rs, x = NULL) {
            dummie_ta <- brick(tavg)
            dummie_rs <- brick(rs)
            abtew(
              tavg = dummie_ta,
              rs = dummie_rs
            )
          })
