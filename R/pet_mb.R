#' PET calculation by McGuinness Bordne method
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
#' x <- tavg.nc  or charater string with path to data file 
#' x_brick <- raster::brick(x) 
#' pet <- pet_mb(tavg = x_brick)
#' }

pet_mb <- function(tavg){
  LATENT_HEAT_CONSTANT_1 <- 2.501
  LATENT_HEAT_CONSTANT_2 <- 0.002361 
  pet_mb <- pet_aux(tavg)*(tavg+5)/(68*(LATENT_HEAT_CONSTANT_1 - LATENT_HEAT_CONSTANT_2 *tavg))
  pet_mb <- calc(pet_mb, fun = function(y){ifelse(y < 0, 0, y)})
  return(pet_mb)
}

