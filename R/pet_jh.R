#' PET calculation by Jensen Haise method
#'
#' Function to calculate pet 
#'
#' @importFrom raster calc 
#' 
#' @param tavg a RasterBrick object having average temperature 
#' @return a RasterBrick object
#' @keywords internal
#' @examples 
#' download_terraclimate(folder_path = ".","land", variable = "tavg") 
#' tavg_brick <- raster::brick("terraclimate_tavg_land_19580101_20221231_025_monthly.nc")
#' pet_jh <- pet_calc(method = "jh", tavg = tavg_brick)

pet_jh <- function(tavg){
  LATENT_HEAT_CONSTANT_1 <- 2.501
  LATENT_HEAT_CONSTANT_2 <- 0.002361 
  pet_jh <- pet_aux(tavg)*tavg/(40*(LATENT_HEAT_CONSTANT_1 - LATENT_HEAT_CONSTANT_2 *tavg))
  pet_jh <- calc(pet_jh, fun = function(y){ifelse(y < 0, 0, y)})
  return(pet_jh)
}
