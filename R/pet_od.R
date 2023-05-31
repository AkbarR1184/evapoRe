#' PET calculation by Oudin method
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
#' pet <- pet_od(tavg = x_brick)
#' }

pet_od <- function(tavg){
  OUDIN_1 <- 5
  OUDIN_2 <- 100
  LATENT_HEAT_CONSTANT_1 <- 2.501
  LATENT_HEAT_CONSTANT_2 <- 0.002361 
  pet_od <- pet_aux(tavg)*(tavg+ OUDIN_1)/((LATENT_HEAT_CONSTANT_1 - LATENT_HEAT_CONSTANT_2 *tavg)*OUDIN_2)
  pet_od <- calc(pet_od, fun = function(y){ifelse(y < 0, 0, y)})
  return(pet_od)
}
