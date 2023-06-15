#' Potential Evapotranspiration
#'
#' The function \code{pet} estimates PET by different methods
#' 
#' @param x a RasterBrick object with average temperature data.
#' @param method a character string indicating the method to be used. Available options are:
#' \itemize{
#' \item{"bc" for Blaney and Criddle (1950),}
#' \item{"ha" for Hamon (1961),}
#' \item{"jh" for Jensen and Haise (1963),}
#' \item{"mb" for McGuinness and Bordne (1972),}
#' \item{"od" for Oudin (2005). Default,
#' \item{"th" for Thornthwaite (1948).}}
#' }
#' @return a RasterBrick object with potential evapotranspiration in [mm/day].
#' @export
#' @examples 
#' \dontrun{
#' #Calculate PET by Hargreaves-Samani method 
#' tavg <- raster::brick("terraclimate_tavg_land_19580101_20221231_025_monthly.nc")
#' pet_oudin <- pet(tavg, method = "od")
#' pet_oudin <- muldpm(pet_oudin)
#' }

pet <- function(x, method = "od"){
  pet_mon <- switch(method,
                    "bc" = blaney_criddle(x),
                    "ha" = hamon(x),
                    "jh" = jensen_haise(x),
                    "mb" = mcguinness_bordne(x),
                    "od" = oudin(x),
                    "th" = thornthwaite(x)
  )
  return(pet_mon)
}


