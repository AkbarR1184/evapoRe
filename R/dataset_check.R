#' Data set name checker
#'
#' Function to check if the data set is available
#'
#' @importFrom methods is
#' @param data_name a character string.
#' @return No return value, called to download the data set.
#' @keywords internal

dataset_check <- function(data_name){
  available_datasets <- c("all", "era5", "era5_land", "fldas", "gldas-clsm",
                          "gldas-noah", "gldas-vic", "gleam", "jra55", "merra2",
                          "terraclimate")
  
  if (!all(data_name %in% available_datasets)){
    stop("Error: Dataset not available.\n
    Select from ", paste(available_datasets, collapse = ", "))
  }
}