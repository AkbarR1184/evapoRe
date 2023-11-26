#' Monthly Potential Evapotranspiration data
#'
#' A subset of calculated monthly Potential Evapotranspiration data in mm over Spain. More details of the used method can be found \samp{https://www.sciencedirect.com/science/article/pii/S0022169404004056}.
#'
#' @format A data.table with 120 obs. of 2 variables:
#' \describe{
#'   \item{date}{IDate format \%Y-\%m-\%d}
#'   \item{value}{monthly average values}
#' }
#' @source Data was calculated using the Oudin method based on raw temperature data. More details of the raw data
#'  can be found \samp{https://journals.ametsoc.org/view/journals/bams/103/3/BAMS-D-21-0145.1.xml}.
"pet_oudin_esp_ts"