#' Annual Heat Index
#'
#' Calculate Annual Heat Index
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom methods as
#' @importFrom lubridate day leap_year ymd
#' @importFrom raster getZ setZ stackApply
#' @importFrom utils tail
#' @param x a character string with the path to the data file.
#' @return data.table
#' @keywords internal

heat_index <- function(x){
  dummie_dates <- getZ(x)
  start_date_mo <- dummie_dates[1] %>% ymd()
  end_date_mo <- tail(dummie_dates, 1) %>% ymd()
  dates_mo <- seq(start_date_mo, end_date_mo, 'month')
  start_year <- min(year(dummie_dates))
  start_year <- paste0(start_year, '-01-01') %>% as.Date()
  end_year <- max(year(dummie_dates))
  end_year <- paste0(end_year, '-01-01') %>% as.Date()
  new_dates <- seq(start_year, end_year, 'years')
  dummie_dates <- as.data.table(dummie_dates) %>% setnames('date')
  dummie_dates <- dummie_dates[, layer_idx := .I
                               ][, year_idx := year(date) - min(year(date)) + 1
                                 ][, mo := month(date)
                                   ][, mo_idx := month(date) + 12*(year_idx - 1)]
  is_ym <- !Reduce(`|`, duplicated(dummie_dates, by = c('year_idx', 'mo_idx')))
  if (!is_ym) {
    dummie_mo <- stackApply(x, indices = dummie_dates$mo_idx, fun = mean)
    dummie_mo <- calc(dummie_mo, fun = function(val) {
      (val/5)^1.514
    })
    dummie_mo_dates <- as.data.table(dates_mo) %>% setnames('date')
    dummie_mo_dates <- dummie_mo_dates[, layer_idx := .I
                                       ][, year_idx := year(date) - min(year(date)) + 1
                                         ][, mo := month(date)
                                           ][, mo_idx := month(date) + 12*(year_idx - 1)]
    dummie_t <- stackApply(dummie_mo, indices = dummie_mo_dates$year_idx, fun = sum)
  } else {
    if (length(unique(dummie_dates$year_idx)) == nrow(dummie_dates)) {
      dummie_t <- x
    } else {
      dummie_mo <- calc(x, fun = function(val) {
        (val/5)^1.514
      })
      dummie_t <- stackApply(dummie_mo, indices = dummie_dates$year_idx, fun = sum)
    }
  }
  dummie_t <- setZ(dummie_t, new_dates)
  return(dummie_t)
}
