#' Temperature Data Downloader
#'
#' Downloading Temperature data from different datasets
#'
#' @importFrom utils download.file
#' @param data_name a character string indicating the dataset to download. Suitable options are:
#' \itemize{
#' \item{"terraclimate" for TerraClimate dataset,}
#' \item{"cru" for CRU dataset,}
#' \item{"mswx" for MSWX dataset.}
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
#' @param variable a character string indicating the variable to download. Suitable options are:
#' For TerraClimate dataset:
#' \itemize{
#' \item{"t2m" for average temperature,}
#' \item{"tmin" for minimum temperature,}
#' \item{"tmax" for maximum temperature.}
#' }
#' Use "all" to download all available variables for the dataset.
#' @return No return value, called to download the required data sets.
#' @export
#' @examples
#' \donttest{
#' download_t_data("cru", tempdir())
#' }
download_t_data <- function(data_name, path = "", domain = "raw", time_res = "monthly", variable = "all") {
  if (domain == "raw" | domain == "land") {
    domain <- "land"
  } else if (domain == "global") {
    # Code for the global domain
  } else {
    warning(paste0('The ', domain, ' domain is not available'))
  }
  
  old_options <- options()
  on.exit(options(old_options))
  options(timeout = 6000)
  
  switch(data_name,
         "terraclimate" = {
           terraclimate_base <- "https://zenodo.org/records/10122830/files/"
           terraclimate_end <- "?download=1"
           
           switch(variable,
                  "all" = {
                    variable <- c("t2m", "tmin", "tmax")
                  },
                  "t2m" = {},
                  "tmin" = {},
                  "tmax" = {},
                  stop("Invalid variable option provided for TerraClimate dataset.")
           )
           
           file_ext <- paste0("_degC_", domain, "_195801_202212_025_", time_res, ".nc")
           file_name <- paste0("terraclimate_", variable, file_ext)
         },
         "cru" = {
           cru_base <- "https://zenodo.org/records/10122830/files/"
           cru_end <- "?download=1"
           
           switch(variable,
                  "all" = {
                    variable <- c("t2m", "tmin", "tmax")
                  },
                  "t2m" = {},
                  "tmin" = {},
                  "tmax" = {},
                  stop("Invalid variable option provided for CRU dataset.")
           )
           
           file_ext <- paste0("_degC_", domain, "_190101_202212_025_", time_res, ".nc")
           file_name <- paste0("cru_", variable, file_ext)
         },
         "mswx" = {
           mswx_base <- "https://zenodo.org/records/10122830/files/"  
           mswx_end <- "?download=1"  
           
           switch(variable,
                  "all" = {
                    variable <- c("t2m", "tmin", "tmax")
                  },
                  "t2m" = {},
                  "tmin" = {},
                  "tmax" = {},
                  stop("Invalid variable option provided for MSWX dataset.")
           )
           
           file_ext <- paste0("_degC_", domain, "_197901_202308_025_", time_res, ".nc") 
           file_name <- paste0("mswx_", variable, file_ext)
         },
         stop("Invalid dataset option provided.")
  )
  
  file_url <- paste0(ifelse(data_name == "terraclimate", terraclimate_base,
                            ifelse(data_name == "cru", cru_base, mswx_base)), file_name,
                     ifelse(data_name == "terraclimate", terraclimate_end,
                            ifelse(data_name == "cru", cru_end, mswx_end)))
  file_destination <- paste(path, file_name, sep = "/")
  
  try(download.file(file_url, file_destination, mode = "wb"), silent = F)
}
