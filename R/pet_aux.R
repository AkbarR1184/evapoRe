#' Common math for pet calculations
#'
#' Auxiliary function to calculate pet
#'
#' @importFrom raster brick calc setValues yFromCell ncell getZ
#' @importFrom lubridate yday month
#' @param x a RasterBrick object
#' @return a RasterBrick object
#' @keywords internal

pet_aux <- function(x){
  GSC <- 0.0820
  mon <- c(1:12)
  n_day <-c(17,47,75,105,135,162,198,228,258,288,318,344)
  mon_day <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  lookup <- data.frame(mon,n_day,mon_day)
  match_row <- match(month(getZ(x))[1:12], lookup$mon)
  dist_ratio <- (1.0 + 0.0330 * cos(2*pi *lookup$n_day[match_row] / 365.25))*lookup$mon_day[match_row]
  delta <- 0.409 * sin( 2*pi * yday(getZ(x)) / 365.25 - 1.39)
  delta <- delta[1:12]
  latitude <- raster::setValues(x[[1:12]], yFromCell(x[[1:12]], 1:ncell(x[[1:12]]))*pi/180)
  omega <- calculate_omega(latitude,delta)
  extra_rad <- ((24 * 60) / pi) * GSC  * dist_ratio * (omega * sin(latitude) * sin(delta) + cos(latitude) * cos(delta) * sin(omega))
  return(extra_rad)
}
