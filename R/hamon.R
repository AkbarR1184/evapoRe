#' PET calculation by Hamon method
#'
#' Function to calculate pet
#' 
#' @import  data.table
#' @import doParallel
#' @import foreach
#' @import parallel
#' @importFrom methods as
#' @importFrom lubridate day days_in_month leap_year
#' @importFrom raster calc getZ init nlayers reclassify setZ
#' @param x a RasterBrick object having average temperature 
#' @return a RasterBrick object
#' @keywords internal

hamon <- function(x){
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  tavg <- x
  dl <- day_length(x)
  t_dates <- getZ(x)
  dl_dates <- getZ(dl)
  t_dates_table <- as.data.table(t_dates) %>% 
    .[, leap := leap_year(t_dates)] %>% .[, mo := month(t_dates)] %>%
    .[, dd := day(t_dates)]
  dl_dates_table <- as.data.table(dl_dates) %>%
    .[, leap := leap_year(dl_dates)] %>% .[, mo := month(dl_dates)] %>%
    .[, dd := day(dl_dates)]
  dummie_dates_table <- merge(t_dates_table, dl_dates_table,
                              by = c('leap', 'mo', 'dd'))
  setorder(dummie_dates_table, 't_dates')
  t_idx <- match(dummie_dates_table$t_dates, t_dates)
  dl_idx <- match(dummie_dates_table$dl_dates, dl_dates)
  dummie_pet <- foreach(index = 1:length(t_idx)) %dopar% {
    t_layer <- t_idx[index]
    dl_layer <- dl_idx[index]
    dummie_t <- tavg[[t_layer]]
    dummie_dl <- dl[[dl_layer]]
    es <- 6.108*exp(17.27*dummie_t/(dummie_t + 273.3))
    dummie_h <- 0.165*216.7*(dummie_dl/12)*es/(dummie_t + 273.3)
    dummie_h <- calc(dummie_h, fun = function(val) {
      val[val < 0] <- NA
      return(val)
    })
    return(dummie_h)
  }
  dummie_pet <- brick(dummie_pet)
  dummie_pet <- setZ(dummie_pet, t_dates)
  return(dummie_pet)
}
