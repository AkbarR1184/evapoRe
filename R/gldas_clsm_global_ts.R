#' Monthly Evapotranspiration data
#'
#' Global GLDAS monthly Evapotranspiration data in mm. More details of the raw data can be found \href{https://ldas.gsfc.nasa.gov/gldas}{here}.
#'
#' @format A data.table with 120 obs. of 4 variables:
#' \describe{
#'   \item{date}{IDate format \%Y-\%m-\%d}
#'   \item{value}{monthly average values}
#'   \item{name}{full name of the data set}
#'   \item{type}{source type of the data set}
#' }
#' @source National Aeronautics and Space Administration (NASA)
"gldas_clsm_global_ts"