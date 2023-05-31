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
#' \dontrun{
#' x <- tavg.nc  or character string with path to data file 
#' x_brick <- raster::brick(x) 
#' pet <- pet_jh(tavg = x_brick)
#' }

pet_jh <- function(tavg){
  LATENT_HEAT_CONSTANT_1 <- 2.501
  LATENT_HEAT_CONSTANT_2 <- 0.002361 
  pet_jh <- pet_aux(tavg)*tavg/(40*(LATENT_HEAT_CONSTANT_1 - LATENT_HEAT_CONSTANT_2 *tavg))
  pet_jh <- calc(pet_jh, fun = function(y){ifelse(y < 0, 0, y)})
  return(pet_jh)
}
