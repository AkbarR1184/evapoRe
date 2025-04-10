#' Calculate Extraterrestrial Solar Radiation (ESR)
#'
#' Computes extraterrestrial solar radiation (MJ m-2 day-1) based on latitude and day of the year.
#'
#' @details
#' For raster inputs, provide a `Raster*` object or file path (`character`)
#' The function estimates extraterrestrial radiation.
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @import doParallel
#' @import foreach
#' @import parallel
#' @importFrom raster calc getZ init nlayers reclassify setZ brick
#' @importFrom lubridate day leap_year month yday
#' @importFrom methods setGeneric setMethod
#'
#' @param x A `Raster*` object or a file path (`character`) to a raster file.
#'
#' @return A `RasterBrick` of daily extraterrestrial solar radiation (MJ m-2 day-1).
#'
#' @keywords internal
setGeneric("esr", function(x) standardGeneric("esr"))

#' @rdname esr
setMethod("esr", "Raster", function(x) {
  no_cores <- detectCores() - 1
  if (no_cores < 1 || is.na(no_cores)) no_cores <- 1
  registerDoParallel(cores = no_cores)
  old_dates <- getZ(x)
  new_dates <- esr_dates(x)
  dummie_raster <- setZ(x, new_dates)
  phi <- init(dummie_raster, "y") * pi / 180
  dummie_dates <- yday(getZ(dummie_raster))
  Js <- data.table(date = new_dates, Jday = dummie_dates)
  Js[, `:=`(leap = leap_year(date), mo = month(date), dd = day(date))]
  dummie_idx <- Js[!duplicated(Js, by = c("leap", "mo", "dd"))]
  Js <- dummie_idx$Jday
  esr_dates <- old_dates[match(dummie_idx$date, new_dates)]
  dummie_esr <- foreach(index = 1:length(Js)) %dopar% {
    J <- Js[index]
    delta <- 0.409 * sin(((2 * pi * J) / 365.25) - 1.39)
    dummie <- -tan(phi) * tan(delta)
    dummie <- reclassify(dummie, c(-Inf, -1.0, -1.0, 1.0, Inf, 1.0))
    omega <- acos(dummie)
    dist_ratio <- 1 + 0.033 * cos((2 * pi * J) / 365.25)
    esr <- (24 * 60 / pi) * 0.0820 * dist_ratio *
      ((omega * sin(phi) * sin(delta)) + (cos(phi) * cos(delta) * sin(omega)))
    return(esr)
  }
  dummie_esr <- brick(dummie_esr)
  dummie_esr <- setZ(dummie_esr, esr_dates)
  return(dummie_esr)
})

#' @rdname esr
setMethod("esr", "character", function(x) {
  dummie_brick <- brick(x)
  esr(dummie_brick)
})
