#' Calculate PET by various offline methods 
#'
#' The function \code{pet_calc} calculate selected PET method  
#' 
#' @param method a character string indicating the method name to calculate PET. Available options are:
#' \itemize{
#' \item{"hs" for Hargreaves Samani method (tavg, tmax and tmin are required),}
#' \item{"od" for Oudin method (tavg required),}
#' \item{"mb" for McGuinness and Bordne method (tavg required),}
#' \item{"jh" for Jensen Haise method (tavg required),}
#' \item{"br" for Baier and Robertson (tmax and tmin are required).}
#' }
#' @param tavg a RasterBrick object having average temperature 
#' @param tmax a RasterBrick object having maximum temperature 
#' @param tmin a RasterBrick object having minimum temperature
#' @return a RasterBrick object
#' @export
#' @examples 
#' #Calculate PET by Hargreaves-Samani method 
#' download_terraclimate(folder_path = ".","land", variable = "tavg")   
#' download_terraclimate(folder_path = ".","land", variable = "tmin") 
#' download_terraclimate(folder_path = ".","land", variable = "tmax")
#' tavg_brick <- raster::brick("terraclimate_tavg_land_19580101_20221231_025_monthly.nc")
#' tmax_brick <- raster::brick("terraclimate_tmax_land_19580101_20221231_025_monthly.nc")
#' tmin_brick <- raster::brick("terraclimate_tmin_land_19580101_20221231_025_monthly.nc")
#' pet_hs <- pet_calc(method = "hs",tavg = tavg_brick, tmax = tmax_brick, tmin = tmin_brick)

pet_calc <- function(method,tavg,tmax,tmin){
  pet_mon <- switch(method,
                    "jh" = pet_jh(tavg),
                    "mb" = pet_mb(tavg),
                    "od" = pet_od(tavg),
                    "hs" = pet_hs(tavg, tmax, tmin),
                    "br" = pet_br(tmax, tmin),
                    stop("Error: method is not available. Select from br, hs, jh, mb, od")
  )
  return(pet_mon)
}


