#' Extraterrestrial Solar Radiation (ESR)
#' 
#'#' The function \code{esr} Calculate ESR for a given data formats.
#'
#' @details
#' If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value"
#' If `x` is a filename, it should point to a *.nc file.
#' @import  data.table
#' @import doParallel
#' @import foreach
#' @import parallel
#' @importFrom methods as setGeneric setMethod
#' @importFrom lubridate day leap_year days_in_month
#' @importFrom raster calc getZ init nlayers reclassify setZ
#' @param x Raster* object; data.table (see details); filename (character; see details)
#' @return Raster* object; data.table
#' @keywords internal

setGeneric("esr", function(x) standardGeneric("esr"))

#' @rdname esr
#' @method esr Raster

setMethod("esr", "Raster",
          function(x) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))
              (no_cores <- 1)
            registerDoParallel(cores = no_cores)
            old_dates <- getZ(x)
            new_dates <- esr_dates(x)
            dummie_raster <- setZ(x, new_dates)
            phi <- init(dummie_raster, 'y')*pi/180
            dummie_dates <- yday(getZ(dummie_raster))
            Js <- data.frame('date' = new_dates, 'Jday' = dummie_dates) %>% as.data.table()
            Js <- Js[, leap := leap_year(date)][, mo := month(date)][, dd := day(date)]
            dummie_idx <- Js[!duplicated(Js , by = c('leap', 'mo', 'dd')),]
            Js <- dummie_idx$Jday
            esr_dates <- old_dates[match(dummie_idx$date, new_dates)]
            dummie_esr <- foreach(index = 1:length(Js)) %dopar% {
              J <- Js[index]
              delta <- 0.409*sin(((2*pi*J)/(365.25)) - 1.39)
              dummie <- -tan(phi)*tan(delta)
              dummie <- reclassify(dummie, c(-Inf, -1.0, -1.0, 1.0, Inf, 1.0))
              omega <- acos(dummie)
              dist_ratio <- 1 + 0.033*cos((2*pi*J)/365.25)
              esr <- (24*60/pi)*0.0820*dist_ratio*((omega*sin(phi)*sin(delta)) + 
                                                     (cos(phi)*cos(delta)*sin(omega)))
              return(esr)
            }
            dummie_esr <- brick(dummie_esr)
            dummie_esr <- setZ(dummie_esr, esr_dates)
            return(dummie_esr)
          })

#' @rdname esr
#' @method esr data.table

setMethod("esr", "data.table",
          function(x){
            dummie_esr <- pet_params_calc(x)
            dummie_esr <- dummie_esr[,c("lat","date", "ext_rad","nday")]
            return(dummie_esr)
          })

#' @rdname esr
#' @method esr character

setMethod("esr", "character",
          function(x) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 | is.na(no_cores))
              (no_cores <- 1)
            registerDoParallel(cores = no_cores)
            dummie_brick <- brick(x)
            old_dates <- getZ(dummie_brick)
            new_dates <- esr_dates(dummie_brick)
            dummie_raster <- setZ(dummie_brick, new_dates)
            phi <- init(dummie_raster, 'y')*pi/180
            dummie_dates <- yday(getZ(dummie_raster))
            Js <- data.frame('date' = new_dates, 'Jday' = dummie_dates) %>% as.data.table()
            Js <- Js[, leap := leap_year(date)][, mo := month(date)][, dd := day(date)]
            dummie_idx <- Js[!duplicated(Js , by = c('leap', 'mo', 'dd')),]
            Js <- dummie_idx$Jday
            esr_dates <- old_dates[match(dummie_idx$date, new_dates)]
            dummie_esr <- foreach(index = 1:length(Js)) %dopar% {
              J <- Js[index]
              delta <- 0.409*sin(((2*pi*J)/(365.25)) - 1.39)
              dummie <- -tan(phi)*tan(delta)
              dummie <- reclassify(dummie, c(-Inf, -1.0, -1.0, 1.0, Inf, 1.0))
              omega <- acos(dummie)
              dist_ratio <- 1 + 0.033*cos((2*pi*J)/365.25)
              esr <- (24*60/pi)*0.0820*dist_ratio*((omega*sin(phi)*sin(delta)) + 
                                                     (cos(phi)*cos(delta)*sin(omega)))
              return(esr)
            }
            dummie_esr <- brick(dummie_esr)
            dummie_esr <- setZ(dummie_esr, esr_dates)
            return(dummie_esr)
          })

