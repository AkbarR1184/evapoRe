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
#' @examples \dontrun{
#' calculate pet by Hargreaves Samani method 
#' x <- tavg.nc  # or character string with path to data file 
#' y <- tmax.nc # or character string with path to data file 
#' z <- tmin.nc # or character string with path to data file 
#' x_brick <- raster::brick(x) 
#' y_brick <- raster::brick(y) 
#' z_brick <- raster::brick(z)
#' pet <- pet_calc(method_name = "hs",x_brick,y_brick,z_brick)
#' 
#' calculate pet by Oudin method 
#' x <- tavg.nc  # or character string with path to data file
#' pet <- pet_calc(method_name = "od", tavg = x_brick)
#' }
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