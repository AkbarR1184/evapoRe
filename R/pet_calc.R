#' PET methods calculation 
#'
#' Function to calculate various pet methods 
#' 
#' @param method_name a character string indicating the method name to calculate
#' pet. Available options are:
#' \itemize{
#' \item{"hs" for calculating pet by Hargreves Samani method,}
#' \item{"od" for calculating pet by Oudin methode,}
#' \item{"mb" for calculating pet by McGuinness and Bordne methode,}
#' \item{"jh" for calculating pet by Jensen Haise methode,}
#' \item{"br",for calculating pet by Baier and Robertson.}
#' }
#' @param tavg a RasterBrick object having average temperature 
#' @param tmax a RasterBrick object having maximum temperature 
#' @param tmin a RasterBrick object having minimum temperature
#' @return a RasterBrick object
#' @export
#' @examples 
#' #Calculate PET by Hargreaves-Samani method 
#' #Downloading temperature data (To download the raster brick in a 
#' #specific folder, specify the desired folder path instead of using the 
#' #default current working directory )
#' 
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
#' 
#' # Calculate PET by Oudin method 
#' #Downloading temperature data (To download the raster brick in a 
#' #specific folder, specify the desired folder path instead of using the 
#' #default current working directory )
#' download_terraclimate(folder_path = ".","land", variable = "tavg")
#' 
#' #Loading the nc files     
#' tavg_nc <- "terraclimate_tavg_land_19580101_20221231_025_monthly.nc"
#' 
#' # Converting nc files to RasterBrick 
#' tavg_brick <- raster::brick(tavg_nc)
#' 
#' #Calculating PET
#' pet_od <- pet_calc(method_name = "od", tavg = tavg_brick)

pet_calc <- function(method_name,tavg,tmax,tmin){
  pet_check(method_name)
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  pet_mon <- lapply(method_name, function(dataset) switch(dataset,
                                               "jh" = pet_jh(tavg),
                                               "mb" = pet_mb(tavg),
                                               "od" = pet_od(tavg),
                                               "hs" = pet_hs(tavg,tmax,tmin),
                                               "br" = pet_br(tmax,tmin),
                                               
  ))
  return(pet_mon[[1]])
}