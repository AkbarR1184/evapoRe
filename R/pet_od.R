#' PET calculation by Oudin method
#'
#' Function to calculate pet 
#'
#' @importFrom raster calc getZ setZ
#' 
#' @param tavg a RasterBrick object having average temperature 
#' @return a RasterBrick object
#' @keywords internal
#' @examples 
#' #Function \code{\link{download_terraclimate}} is used to download TerraClimate temperature data (tavg) 
#' tavg_brick <- raster::brick("terraclimate_tavg_land_19580101_20221231_025_monthly.nc")
#' pet_od <- pet_calc(method = "od", tavg = tavg_brick)

pet_od <- function(tavg){
  OUDIN_1 <- 5
  OUDIN_2 <- 100
  LATENT_HEAT_CONSTANT_1 <- 2.501
  LATENT_HEAT_CONSTANT_2 <- 0.002361 
  pet_od <- pet_aux(tavg)*(tavg+ OUDIN_1)/((LATENT_HEAT_CONSTANT_1 - LATENT_HEAT_CONSTANT_2 *tavg)*OUDIN_2)
  pet_od <- calc(pet_od, fun = function(y){ifelse(y < 0, 0, y)})
  names(pet_od) <- paste0("X", raster::getZ(tavg))
  pet_od <- raster::setZ(pet_od, z = raster::getZ(tavg), name = "Date/time")
  return(pet_od)
}


