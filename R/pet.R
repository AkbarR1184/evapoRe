#' Potential Evapotranspiration
#'
#' The function \code{pet} estimates PET using various methods.
#'
#' @param method Character string indicating the PET estimation method. Available options include:
#' \itemize{
#'   \item \code{"abtew"} — Abtew (1996)
#'   \item \code{"baier_robertson"} — Baier and Robertson (1965)
#'   \item \code{"blaney_criddle"} — Blaney and Criddle (1950)
#'   \item \code{"hamon"} — Hamon (1961)
#'   \item \code{"hargreaves_samani"} — Hargreaves and Samani (1985)
#'   \item \code{"jensen_haise"} — Jensen and Haise (1963)
#'   \item \code{"mcguinness_bordne"} — McGuinness and Bordne (1972)
#'   \item \code{"oudin"} — Oudin (2005). \strong{Default}
#'   \item \code{"penman_monteith_f56"} — FAO Penman-Monteith (FAO-56)
#'   \item \code{"priestly_taylor"} — Priestly and Taylor (1972)
#'   \item \code{"thornthwaite"} — Thornthwaite (1948)
#'   \item \code{"turc"} — Turc (1961)
#' }
#'
#' @param ... Inputs passed to the selected method. These can be:
#' \itemize{
#'   \item A single Raster* object, file path, or data.table — for methods requiring one variable (e.g., \code{oudin});
#'   \item Named arguments (e.g., \code{tavg = ...}, \code{tmax = ...}) — for multi-variable methods;
#'   \item A data.table with all required variables — passed as \code{x = your_data}.
#' }
#'
#' @details
#' For single-input methods (e.g., Oudin), you can pass the input directly.  
#' For multi-input methods (e.g., Penman-Monteith), use named arguments or a data.table.  
#' Use \code{pet_method_requirements()} to check required variables.
#'
#' @return A `Raster*` object in mm/day (if raster-based inputs) or a `data.table`
#' with columns \code{lon}, \code{lat}, \code{date}, and \code{value} (PET in mm/day).
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Oudin method with NetCDF path
#' pet_od <- pet(method="oudin", "tavg.nc")
#'
#' # Oudin method with raster
#' tavg <- raster::brick("tavg.nc")
#' pet_od <- pet("oudin", tavg)
#'
#' # Oudin method with data.table
#' pet_od <- pet(method="oudin", x = your_data)
#'
#' # Hargreaves-Samani method with multiple inputs
#' tavg <- raster::brick("tavg.nc")
#' tmax <- raster::brick("tmax.nc")
#' tmin <- raster::brick("tmin.nc")
#' pet_hs <- pet(method="hargreaves_samani", tavg = tavg, tmin = tmin, tmax = tmax)
#' }

pet <- function(method = "oudin", ...) {
  switch(
    method,
    "abtew" = abtew(...),
    "baier_robertson" = baier_robertson(...),
    "blaney_criddle" = blaney_criddle(...),
    "hamon" = hamon(...),
    "hargreaves_samani" = hargreaves_samani(...),
    "jensen_haise" = jensen_haise(...),
    "mcguinness_bordne" = mcguinness_bordne(...),
    "oudin" = oudin(...),
    "pm_fao56" = penman_monteith_f56(...),
    "priestly_taylor" = priestly_taylor(...),
    "thornthwaite" = thornthwaite(...),
    "turc" = turc(...),
    stop(
      "Invalid method. Valid options are: 'abtew', 'baier_robertson', 'blaney_criddle', 
      'hamon', 'hargreaves_samani', 'jensen_haise', 'mcguinness_bordne', 'oudin', 'pm_fao56',
      'priestly_taylor', 'thornthwaite', 'turc'."
    )
  )
}
