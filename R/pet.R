#' Potential Evapotranspiration
#'
#' The function \code{pet} estimates PET using various methods.
#'
#' @param x A data.table containing columns \code{lon}, \code{lat}, \code{date},
#' and the required meteorological variables for the selected method. For raster
#' or NetCDF input, provide named arguments like \code{tavg}, \code{tmin},
#' \code{tmax}, etc., depending on the method.
#'
#' @param method A character string indicating the method to be used. Available
#' options are:
#' \itemize{
#'   \item{"abtew" for Abtew (1996),}
#'   \item{"baier_robertson" for Baier and Robertson (1965),}
#'   \item{"blaney_criddle" for Blaney and Criddle (1950),}
#'   \item{"hamon" for Hamon (1961),}
#'   \item{"hargreaves_samani" for Hargreaves and Samani (1985),}
#'   \item{"jensen_haise" for Jensen and Haise (1963),}
#'   \item{"mcguinness_bordne" for McGuinness and Bordne (1972),}
#'   \item{"oudin" for Oudin (2005). Default,}
#'   \item{"penman_monteith_f56" for FAO Penman-Monteith (FAO-56),}
#'   \item{"priestly_taylor" for Priestly and Taylor (1972),}
#'   \item{"thornthwaite" for Thornthwaite (1948),}
#'   \item{"turc" for Turc (1961).}
#' }
#'
#' @param ... Additional arguments passed to the selected method (e.g., 
#' raster objects or file paths such as \code{tavg}, \code{rs}, \code{rh}, etc.)
#'
#' @details
#' Required input variables depend on the selected method. To check the list of
#' required inputs, use:
#'
#' \code{pet_method_requirements()} — to list all methods and their required inputs  
#' \code{pet_method_requirements("turc")} — to list inputs required for a specific method
#'
#' @return A Raster object in mm/day or a data.table with columns \code{lon},
#' \code{lat}, \code{date}, and \code{value} (PET in mm/day), depending on input type.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Default method (Oudin)
#' pet(x = my_data)
#'
#' # FAO-56 method with raster inputs
#' pet(method = "penman_monteith_f56",
#'     tavg = "tavg.nc", tmin = "tmin.nc", tmax = "tmax.nc",
#'     rn = "rn.nc", u = "u.nc", tdew = "tdew.nc", elevation = "elev.nc")
#'
#' # View required variables
#' pet_method_requirements("turc")
#' }

pet <- function(method = "oudin", ...) {
  switch(method,
         "abtew" = abtew(...),
         "baier_robertson" = baier_robertson(...),
         "blaney_criddle" = blaney_criddle(...),
         "hamon" = hamon(...),
         "hargreaves_samani" = hargreaves_samani(...),
         "jensen_haise" = jensen_haise(...),
         "mcguinness_bordne" = mcguinness_bordne(...),
         "oudin" = oudin(...),
         "penman_monteith_f56" = penman_monteith_f56(...),
         "priestly_taylor" = priestly_taylor(...),
         "thornthwaite" = thornthwaite(...),
         "turc" = turc(...),
         stop("Invalid method.")
  )
}
