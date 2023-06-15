#' PET calculation by Thornthwaite method
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


thornthwaite <- function(x){
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  tavg <- x
  hi <- heat_index(x)
  dl <- day_length(x)
  t_dates <- getZ(x)
  hi_dates <- getZ(hi)
  dl_dates <- getZ(dl)
  t_dates_table <- as.data.table(t_dates) %>% 
    .[, leap := leap_year(t_dates)] %>% .[, mo := month(t_dates)] %>%
    .[, dd := day(t_dates)] %>% .[, year_idx := year(t_dates)]
  dl_dates_table <- as.data.table(dl_dates) %>%
    .[, leap := leap_year(dl_dates)] %>% .[, mo := month(dl_dates)] %>%
    .[, dd := day(dl_dates)] %>% .[, year_idx := year(dl_dates)]
  hi_dates_table <- as.data.table(hi_dates) %>%
    .[, year_idx := year(hi_dates)] 
  dummie_dates_table_dl <- merge(t_dates_table, dl_dates_table,
                              by = c('leap', 'mo', 'dd'))
  dummie_dates_table_hi <- merge(t_dates_table, hi_dates_table,
                                 by = c('year_idx'))
  setorder(dummie_dates_table_dl, 't_dates')
  setorder(dummie_dates_table_hi, 't_dates')
  t_idx <- match(dummie_dates_table_dl$t_dates, t_dates)
  dl_idx <- match(dummie_dates_table_dl$dl_dates, dl_dates)
  hi_idx <- match(dummie_dates_table_hi$hi_dates, hi_dates)
  dummie_pet <- foreach(index = 1:length(t_idx)) %dopar% {
    t_layer <- t_idx[index]
    dl_layer <- dl_idx[index]
    hi_layer <- hi_idx[index]
    dummie_t <- tavg[[t_layer]]
    dummie_dl <- dl[[dl_layer]]
    dummie_hi <- hi[[hi_layer]]
    dummie_k <- 0.49239 + (1.792*dummie_hi*1e-2) - (0.771*(dummie_hi^2)*1e-4)
    dummie_th <- 16*(dummie_dl/360)*(10*dummie_t/dummie_hi)^dummie_k
    dummie_th <- calc(dummie_th, fun = function(val) {
      val[val < 0] <- NA
      return(val)
    })
    return(dummie_th)
  }
  dummie_pet <- brick(dummie_pet)
  dummie_pet <- setZ(dummie_pet, t_dates)
  return(dummie_pet)
}
