#' MERRA-2 data downloader
#'
#' Downloading MERRA-2 evapotranspiration data
#'
#' @importFrom utils download.file
#' @param path a character string with the path where the data will be downloaded.
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
#' @return No return value, called to download the data set.
#' @keywords internal

download_merra2 <- function(path = "", domain = "raw", time_res = "monthly"){
  old_options <- options()
  on.exit(options(old_options))
  options(timeout = 6000)
  if (domain == "raw"){domain <- "land"}
  zenodo_base <- "https://zenodo.org/records/10123598/files/"
  zenodo_end <- "?download=1"
  file_name <- paste0("merra2_e_mm_", domain, "_198001_202301_025_", time_res, ".nc")
  file_url <- paste0(zenodo_base, file_name, zenodo_end)
  file_destination <- paste(path, file_name, sep = "/")
  try(download.file(file_url, file_destination, mode = "wb"), silent = TRUE)
}