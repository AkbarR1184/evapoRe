#' Monthly Potential Evapotranspiration data
#'
#' A subset of calculated monthly Potential Evapotranspiration data in mm over Czechia. More details of the used method can be found \href{https://www.sciencedirect.com/science/article/pii/S0022169404004056}{here}.
#'
#' @format A data.table with 120 obs. of 4 variables:
#' \describe{
#'   \item{date}{IDate format \%Y-\%m-\%d}
#'   \item{value}{monthly average values}
#'   \item{name}{full name of the data set}
#'   \item{type}{source type of the data set}
#' }
#' @source Data was calculated using the Oudin method based on raw temperature data. More details of the raw data
#'  can be found \href{https://www.climatologylab.org/terraclimate.html}{here}.
"pet_oudin_cz_ts"