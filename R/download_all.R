#' All data downloader
#'
#' Function for downloading all data.
#'
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
#' @return No return value, called to download the data set.
#' @keywords internal

download_all <- function(folder_path = ".", domain = "raw", time_res = "monthly"){
  download_bess(folder_path, domain, time_res)
  download_camele(folder_path, domain, time_res)
  download_era5_land(folder_path, domain, time_res)
  download_era5(folder_path, domain, time_res)
  download_fldas(folder_path, domain, time_res)
  download_gldas_clsm(folder_path, domain, time_res)
  download_gldas_noah(folder_path, domain, time_res)
  download_gldas_vic(folder_path, domain, time_res)
  download_gleam(folder_path, domain, time_res)
  download_jra55(folder_path, domain, time_res)
  download_merra2(folder_path, domain, time_res)
  download_terraclimate(folder_path, domain, time_res)
  download_zheng(folder_path, domain, time_res)
}