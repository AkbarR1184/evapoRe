#' Monthly Total Evapotranspiration from FLUXNET
#'
#' Monthly total evapotranspiration (ET) in millimeters, derived from FLUXNET tower observations worldwide. The data are spatially referenced and aggregated to monthly resolution for global coverage. ET values were calculated from FLUXNET latent heat flux and temperature data provided at \href{https://zenodo.org/records/13853409}{Zenodo}.
#'
#' @format A data.table with 14,592 rows and 4 columns:
#' \describe{
#'   \item{lon}{Longitude in decimal degrees}
#'   \item{lat}{Latitude in decimal degrees}
#'   \item{date}{Date in IDate format (\%Y-\%m-\%d)}
#'   \item{value}{Monthly total evapotranspiration (mm)}
#' }
#' @source \href{https://zenodo.org/records/13853409}{FLUXNET via Zenodo}
"et_fluxnet_global_monthly_total"