#' PET calculation by McGuinness Bordne method
#'
#' Function to calculate pet getZ setZ
#'
#' @importFrom raster calc 
#' 
#' @param tavg a RasterBrick object having average temperature 
#' @return a RasterBrick object
#' @keywords internal
#' @examples
#' \dontrun{
#' #Function \code{\link{download_terraclimate}} is used to download temperature data
#' tavg_brick <- raster::brick("terraclimate_tavg_land_19580101_20221231_025_monthly.nc")
#' pet_mb <- pet_calc(method = "mb", tavg = tavg_brick)}

pet_mb <- function(tavg){
  LATENT_HEAT_CONSTANT_1 <- 2.501
  LATENT_HEAT_CONSTANT_2 <- 0.002361 
  pet_mb <- pet_aux(tavg)*(tavg+5)/(68*(LATENT_HEAT_CONSTANT_1 - LATENT_HEAT_CONSTANT_2 *tavg))
  pet_mb <- calc(pet_mb, fun = function(y){ifelse(y < 0, 0, y)})
  names(pet_mb) <- paste0("X", raster::getZ(tavg))
  pet_mb <- raster::setZ(pet_mb, z = raster::getZ(tavg), name = "Date/time")
  return(pet_mb)
}

