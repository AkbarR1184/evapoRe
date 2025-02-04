#' Potential Evapotranspiration
#'
#' The function \code{pet} estimates PET by different methods
#' 
#' @param x a datatable with lon, lat, date, tavg, tmax, and tmin data, 
#' a Raster object containing tavg, or a file path to a .nc file containing tavg data.
#' @param y a Raster object or file path to a .nc file containing tmax data (optional, required for certain methods).
#' @param z a Raster object or file path to a .nc file containing tmin data (optional, required for certain methods).
#' @param method a character string indicating the method to be used. Available options are:
#' \itemize{
#' \item{"bc" for Blaney and Criddle (1950),}
#' \item{"br" for Baier and Robertson  (1965),}
#' \item{"ha" for Hamon (1961),}
#' \item{"hs" for Hargreaves and Samani (1985),}
#' \item{"jh" for Jensen and Haise (1963),}
#' \item{"mb" for McGuinness and Bordne (1972),}
#' \item{"od" for Oudin (2005). Default,}
#' \item{"th" for Thornthwaite (1948).}
#' }
#' @return A Raster object containing potential evapotranspiration in [mm/day], 
#' and a data.table object with potential evapotranspiration in [mm/month].
#' @export
#' @examples 
#' \donttest{
#' #Calculate PET by Oudin 
#' tavg <- raster::brick("terraclimate_tavg_land_19580101_20221231_025_monthly.nc")
#' pet_oudin <- pet(tavg, method = "od")
#' pet_oudin <- muldpm(pet_oudin)
#' }

pet <- function(x, y = NULL, z = NULL, method = "od") {
  pet_mon <- switch(method,
                    "bc" = blaney_criddle(x),
                    "br" = baier_robertson(x,y),
                    "ha" = hamon(x),
                    "hs" = hargreaves_samani(x, y, z),
                    "jh" = jensen_haise(x),
                    "mb" = mcguinness_bordne(x),
                    "od" = oudin(x),
                    "th" = thornthwaite(x)
  )
  
  return(pet_mon)
}


