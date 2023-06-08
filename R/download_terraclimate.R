#' TerraClimate data downloader
#'
#' Function for downloading TerraClimate data.
#'
#' @importFrom utils download.file
#' @param folder_path a character string with the path where the data will be downloaded.
#' @param domain a character string with the desired domain data set. Suitable options are:
#' \itemize{
#' \item{"raw" for default available spatial coverage,}
#' \item{"global" for data sets with global (land and ocean) coverage,}
#' \item{"land" for data sets with land only coverage,}
#' \item{"ocean", for data sets with ocean only coverage.}
#' }
#' @param time_res a character string with the desired time resolution. Suitable options are:
#' \itemize{
#' \item{"monthly",}
#' \item{"yearly".}
#' }
#' @param variable a character string indicating the variable to download. Suitable options are:
#' \itemize{
#' \item{"e" for evapotranspiration,}
#' \item{"tmin" for minimum temperature,}
#' \item{"tmax" for maximum temperature,}
#' \item{"tavg" for average temperature,}
#' \item{"t" for all temperature variables (tmin, tmax, tavg).}
#' }
#' @return No return value, called to download the data set.
#' @keywords internal
#' @export
#' 
download_terraclimate <- function(folder_path = ".", domain = "raw", time_res = "monthly", variable = "e") {
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  
  if (domain == "raw" | domain == "land") {
    domain <- "land"
  } else {
    warning(paste0('The ', domain, ' domain is not available'))
  }
  
  zenodo_base <- "https://zenodo.org/record/7990413/files/"
  zenodo_end <- "?download=1"
  
  if (variable == "t") {
    variables <- c("tmin", "tmax", "tavg")
    file_ext <- paste0("_", domain, "_19580101_20221231_025_", time_res, ".nc")
    file_name <- paste0("terraclimate_", variables, file_ext)
  } else {
    switch(variable,
           "e" = {
             file_name <- paste0("terraclimate_e_mm_", domain, "_195801_202112_025_", time_res, ".nc")
           },
           "tmin" = {
             file_name <- paste0("terraclimate_tmin_", domain, "_19580101_20221231_025_", time_res, ".nc")
           },
           "tmax" = {
             file_name <- paste0("terraclimate_tmax_", domain, "_19580101_20221231_025_", time_res, ".nc")
           },
           "tavg" = {
             file_name <- paste0("terraclimate_tavg_", domain, "_19580101_20221231_025_", time_res, ".nc")
           },
           stop("Invalid variable option provided.")
    )
  }
  
  file_url <- paste0(zenodo_base, file_name, zenodo_end)
  file_destination <- paste(folder_path, file_name, sep = "/")
  
  try(download.file(file_url, file_destination, mode = "wb"), silent = TRUE)
}
