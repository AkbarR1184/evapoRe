#' PET calculation by Oudin method
#'
#' Function to calculate pet
#' 
#' @import  data.table
#' @import doParallel
#' @import foreach
#' @import parallel
#' @importFrom methods as
#' @importFrom lubridate day leap_year
#' @importFrom raster calc getZ init nlayers reclassify setZ
#' @param tavg a RasterBrick object having average temperature 
#' @return a RasterBrick object
#' @keywords internal
#' @examples
#' \dontrun{
#' tavg_brick <- raster::brick("terraclimate_tavg_land_19580101_20221231_025_monthly.nc")
#' pet_od <- pet(method = "od", tavg = tavg_brick)}

oudin <- function(x){
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  tavg <- x
  re <- esr(x)
  t_dates <- getZ(x)
  re_dates <- getZ(re)
  t_dates_table <- as.data.table(t_dates) %>% 
    .[, leap := leap_year(t_dates)] %>% .[, mo := month(t_dates)] %>%
    .[, dd := day(t_dates)]
  re_dates_table <- as.data.table(re_dates) %>%
    .[, leap := leap_year(re_dates)] %>% .[, mo := month(re_dates)] %>%
    .[, dd := day(re_dates)]
  dummie_dates_table <- merge(t_dates_table, re_dates_table,
                              by = c('leap', 'mo', 'dd'))
  setorder(dummie_dates_table, 't_dates')
  t_idx <- match(dummie_dates_table$t_dates, t_dates)
  re_idx <- match(dummie_dates_table$re_dates, re_dates)
  dummie_pet <- foreach(index = 1:length(t_idx)) %dopar% {
    t_layer <- t_idx[index]
    re_layer <- re_idx[index]
    dummie_t <- tavg[[t_layer]]
    dummie_re <- re[[re_layer]]
    lambda <- 2.501 - 0.002361*dummie_t
    dummie_od <- dummie_re*(dummie_t + 5)/(100*lambda)
    return(dummie_od)
  }
  dummie_pet <- brick(dummie_pet)
  dummie_pet <- setZ(dummie_pet, t_dates)
  return(dummie_pet)
}
