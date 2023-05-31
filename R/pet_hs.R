#' PET calculation by Hargreaves-Samani method
#'
#' Function to calculate pet 
#'
#' @importFrom raster calc 
#' 
#' @param tavg a RasterBrick object having average temperature 
#' @param tmax a RasterBrick object having maximum temperature
#' @param tmin a RasterBrick object having minimum temperature 
#' @return a RasterBrick object
#' @keywords internal
#' @examples 
#' \dontrun{
#' x <- tavg.nc  # or character string with path to data file 
#' y <- tmax.nc # or character string with path to data file 
#' z <- tmin.nc # or character string with path to data file 
#' x_brick <- raster::brick(x) 
#' y_brick <- raster::brick(y) 
#' z_brick <- raster::brick(z) 
#' pet <- pet_hs(tavg = x_brick, tmax = y_brick, tmin = z_brick)
#' }

pet_hs <- function(tavg,tmax,tmin){
  HARGREAVES_1 <- 17.8   
  HARGREAVES_2 <- 0.0023
  LATENT_HEAT_CONSTANT_1 <- 2.501
  LATENT_HEAT_CONSTANT_2 <- 0.002361 
  pet_hs <- HARGREAVES_2 * pet_aux(tavg) * sqrt(abs(tmax - tmin))*(tavg+HARGREAVES_1)/(LATENT_HEAT_CONSTANT_1 - LATENT_HEAT_CONSTANT_2 *tavg)
  pet_hs <- raster::calc(pet_hs, fun = function(y){ifelse(y < 0, 0, y)})
  return(pet_hs)
}
