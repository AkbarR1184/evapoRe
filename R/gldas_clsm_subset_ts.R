#' Monthly Evapotranspiration data
#'
#' A subset of GLDAS monthly Evapotranspiration data in mm over -10-40E, 30-45N. More details of the raw data can be found \href{https://ldas.gsfc.nasa.gov/gldas}{here}.
#'
#' @format A data.table with 120 obs. of 2 variables:
#' \describe{
#'   \item{date}{IDate format \%Y-\%m-\%d}
#'   \item{value}{monthly average values}
#' }
#' @source National Aeronautics and Space Administration (NASA)
"gldas_clsm_subset_ts"