#' Temperature Data Downloader
#'
#' Function for downloading Temperature data from different datasets.
#'
#' @importFrom utils download.file
#' @param dataset a character string indicating the dataset to download. Suitable options are:
#' \itemize{
#' \item{"terraclimate" for TerraClimate dataset,}
#' \item{"em_earth" for EM-Earth dataset.}
#' }
#' @param path a character string with the path where the data will be downloaded.
#' @param domain a character string with the desired domain data set. Suitable options are:
#' \itemize{
#' \item{"raw" for default available spatial coverage,}
#' \item{"global" for data sets with global (land and ocean) coverage,}
#' \item{"land" for data sets with land only coverage,}
#' \item{"ocean" for data sets with ocean only coverage.}
#' }
#' @param time_res a character string with the desired time resolution. Suitable options are:
#' \itemize{
#' \item{"monthly",}
#' \item{"yearly".}
#' }
#' @param variables a character string indicating the variable to download for each dataset. Suitable options are:
#' For TerraClimate dataset:
#' \itemize{
#' \item{"tavg" for average temperature,}
#' \item{"tmin" for minimum temperature,}
#' \item{"tmax" for maximum temperature.}
#' }
#' For EM-Earth dataset:
#' \itemize{
#' \item{"tdew" for dew point temperature,}
#' \item{"tavg" for average temperature,}
#' \item{"trange" for temperature range.}
#' }
#' Use "all" to download all available variables for the dataset.
#' @return No return value, called to download the data set.
#' @keywords internal
#' @export
download_t_data <- function(dataset, path = ".", domain = "raw", time_res = "monthly", variables = "all") {
  if (domain == "raw" | domain == "land") {
    domain <- "land"
  } else {
    warning(paste0('The ', domain, ' domain is not available'))
  }
  
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  
  switch(dataset,
         "terraclimate" = {
           zenodo_base <- "https://zenodo.org/record/7990413/files/"
           zenodo_end <- "?download=1"
           
           switch(variables,
                  "all" = {
                    variables <- c("tavg", "tmin", "tmax")
                  },
                  "tavg" = {},
                  "tmin" = {},
                  "tmax" = {},
                  stop("Invalid variable option provided for TerraClimate dataset.")
           )
           
           file_ext <- paste0("_", domain, "_19580101_20221231_025_", time_res, ".nc")
           file_name <- paste0("terraclimate_", variables, file_ext)
         },
         "em_earth" = {
           em_earth_base <- "https://em-earth.eu/dataset/files/"
           em_earth_end <- "?download=1"
           
           switch(variables,
                  "all" = {
                    variables <- c("tdew", "tavg", "trange")
                  },
                  "tdew" = {},
                  "tavg" = {},
                  "trange" = {},
                  stop("Invalid variable option provided for EM-Earth dataset.")
           )
           
           file_ext <- paste0("_", domain, "_19580101_20221231_025_", time_res, ".nc")
           file_name <- paste0("em-earth_", variables, file_ext)
         },
         stop("Invalid dataset option provided.")
  )
  
  file_url <- paste0(ifelse(dataset == "terraclimate", zenodo_base, em_earth_base), file_name, ifelse(dataset == "terraclimate", zenodo_end, em_earth_end))
  file_destination <- paste(path, file_name, sep = "/")
  
  try(download.file(file_url, file_destination, mode = "wb"), silent = TRUE)
}
