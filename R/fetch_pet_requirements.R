#' Required Input Variables for PET Methods
#'
#' This function returns the required input variables for a given PET method,
#' or all methods if none is specified.
#'
#' @param x Optional. A character string naming a PET method. If NULL (default),
#' the function returns a named list of all available methods and their required
#' input variables.
#'
#' @return A character vector of required input variables (if \code{x} is
#' provided), or a named list of all PET methods and their required inputs
#' (if \code{x} is NULL).
#'
#' @export
#'
#' @examples
#' \donttest{
#' # PET method requirements
#' pet_method_requirements()
#' }

pet_method_requirements <- function(x = NULL) {
  method_inputs <- list(
    abtew = c("tavg", "rs"),
    baier_robertson = c("tmin", "tmax"),
    blaney_criddle = c("tavg"),
    hamon = c("tavg"),
    hargreaves_samani = c("tmin", "tmax", "tavg"),
    jensen_haise = c("tavg"),
    mcguinness_bordne = c("tavg"),
    oudin = c("tavg"),
    penman_monteith_f56 = c(
      "tavg", "tmin", "tmax", "rn", "u", "tdew", "elevation or pres"
    ),
    priestly_taylor = c("tavg", "rn"),
    thornthwaite = c("tavg"),
    turc = c("tavg", "rs", "rh")
  )
  
  if (is.null(x)) {
    return(method_inputs)
  }
  
  if (!x %in% names(method_inputs)) {
    stop("Invalid method name. Use pet_method_requirements() to see options.")
  }
  
  return(method_inputs[[x]])
}
