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
#' #Downloading temperature data
#' download_terraclimate(folder_path = ".","land", variable = "tavg")   
#' download_terraclimate(folder_path = ".","land", variable = "tmin") 
#' download_terraclimate(folder_path = ".","land", variable = "tmax")
#'  
#' #Loading the nc files     
#' tavg_nc <- "terraclimate_tavg_land_19580101_20221231_025_monthly.nc"
#' tmax_nc <- "terraclimate_tmax_land_19580101_20221231_025_monthly.nc"
#' tmin_nc <- "terraclimate_tmin_land_19580101_20221231_025_monthly.nc"
#' 
#' # Converting nc files to RasterBrick 
#' tavg_brick <- raster::brick(tavg_nc)
#' tmax_brick <- raster::brick(tmax_nc)
#' tmin_brick <- raster::brick(tmin_nc)
#' 
#' #Calculating PET 
#' pet_hs <- pet_calc(method_name = "hs",tavg_brick, tmax_brick, tmin_brick)

pet_hs <- function(tavg,tmax,tmin){
  HARGREAVES_1 <- 17.8   
  HARGREAVES_2 <- 0.0023
  LATENT_HEAT_CONSTANT_1 <- 2.501
  LATENT_HEAT_CONSTANT_2 <- 0.002361 
  pet_hs <- HARGREAVES_2 * pet_aux(tavg) * sqrt(abs(tmax - tmin))*(tavg+HARGREAVES_1)/(LATENT_HEAT_CONSTANT_1 - LATENT_HEAT_CONSTANT_2 *tavg)
  pet_hs <- raster::calc(pet_hs, fun = function(y){ifelse(y < 0, 0, y)})
  return(pet_hs)
}
