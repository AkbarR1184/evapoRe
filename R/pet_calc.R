#' Calculate PET by various  methods 
#'
#' The function \code{pet_calc} calculate selected PET method  
#' 
#' @details Different PET methods require different meteorological inputs, such 
#' as tmax (maximum temperature), tmin (minimum temperature), and tavg (average temperature). 
#' The required inputs corresponding to each PET method are listed as follows:
#' \itemize{
#' \item{"hs" (Hargreaves-Samani): tavg, tmin, tmax,}
#' \item{"od" (Oudin): tavg,}
#' \item{"mb" (McGuinness and Bordne method): tavg,}
#' \item{"jh" (Jensen-Haise): tavg,}
#' \item{"br" (Baier and Robertson): tmin, tmax.}
#' }
#' @param method a character string indicating the method name to calculate PET. Available options are:
#' \itemize{
#' \item{"hs" for Hargreaves Samani method,}
#' \item{"od" for Oudin method,}
#' \item{"mb" for McGuinness and Bordne method,}
#' \item{"jh" for Jensen Haise method,}
#' \item{"br" for Baier and Robertson.}
#' }
#' @param tavg a RasterBrick object having average temperature 
#' @param tmax a RasterBrick object having maximum temperature 
#' @param tmin a RasterBrick object having minimum temperature
#' @return a RasterBrick object
#' @export
#' @examples 
<<<<<<< HEAD
#' \donttest{
#' download_terraclimate(folder_path = ".","land", variable = "t")

=======
#' #Calculate PET by Hargreaves-Samani method 
#' function \code{download_terraclimate} is used to download TerraClimate temperature data (tmax,tmin and tavg)
>>>>>>> 8922f7f25bd293f59fced6d375f76cb6aff969d7
#' tavg_brick <- raster::brick("terraclimate_tavg_land_19580101_20221231_025_monthly.nc")
#'
#' tmax_brick <- raster::brick("terraclimate_tmax_land_19580101_20221231_025_monthly.nc")
#'
#' tmin_brick <- raster::brick("terraclimate_tmin_land_19580101_20221231_025_monthly.nc")
<<<<<<< HEAD
#'
#' Calculate PET by Hargreaves and Samani method
#'
#' pet_hs <- pet_calc(method = "hs", tavg = tavg_brick, tmax = tmax_brick, tmin = tmin_brick)}
=======
#' pet_hs <- pet_calc(method = "hs",tavg = tavg_brick, tmax = tmax_brick, tmin = tmin_brick)
>>>>>>> 8922f7f25bd293f59fced6d375f76cb6aff969d7

pet_calc <- function(method = "od", tavg = NULL, tmax = NULL, tmin = NULL){
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


