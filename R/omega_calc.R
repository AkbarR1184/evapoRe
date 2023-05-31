#' Calculate omega
#'
#' Auxiliary function to calculate omega
#'
#' @importFrom raster  reclassify
#' @param x a RasterBrick with delta values
#' @param y a RasterBrick with latitude values
#' @return a RasterBrick object
#' @keywords internal

calculate_omega <- function(x,y) {
  arg <- -tan(x) * tan(y)
  arg <- raster::reclassify(arg, c(-Inf, -1.0, -1.0, 1.0, Inf,1.0 ))
  omega  <- acos(arg)
  return(omega)
}
