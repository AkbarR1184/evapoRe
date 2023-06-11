#' PET calculation by Baier and Robertson method
#'
#' Function to calculate pet 
#'
#' @importFrom raster calc getZ setZ 
#' 
#' @param tmax a RasterBrick object having maximum temperature  
#' @param tmin a RasterBrick object having minimum temperature  
#' @return a RasterBrick object
#' @keywords internal
#' @examples
#' \donttest{
#' #Function \code{\link{download_terraclimate}} is used to download TerraClimate temperature data (tmax,tmin)
#' tmax_brick <- raster::brick("terraclimate_tmax_land_19580101_20221231_025_monthly.nc")
#' tmin_brick <- raster::brick("terraclimate_tmin_land_19580101_20221231_025_monthly.nc")
#' pet_br <- pet_calc(method = "br", tmax = tmax_brick, tmin = tmin_brick)
#' }

pet_br <- function(tmax,tmin){
  BR_1 <- 0.109
  BR_2 <- 0.157
  BR_3 <- 0.158
  BR_4 <- 5.39
  pet_br <- BR_1*pet_aux(tmax) + BR_2*tmax + BR_3*(tmax - tmin) - BR_4
  pet_br <- calc(pet_br, fun = function(y){ifelse(y < 0, 0, y)})
  names(pet_br) <- paste0("X", raster::getZ(tmax))
  pet_br <- raster::setZ(pet_br, z = raster::getZ(tmax), name = "Date/time")
  return(pet_br)
}

