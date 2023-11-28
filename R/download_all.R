#' All data downloader
#'
#' Downloading all datasets
#'
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

download_all <- function(path = "", domain = "raw", time_res = "monthly"){
  download_bess(path, domain, time_res)
  download_camele(path, domain, time_res)
  download_era5_land(path, domain, time_res)
  download_era5(path, domain, time_res)
  download_fldas(path, domain, time_res)
  download_gldas_clsm(path, domain, time_res)
  download_gldas_noah(path, domain, time_res)
  download_gldas_vic(path, domain, time_res)
  download_gleam(path, domain, time_res)
  download_jra55(path, domain, time_res)
  download_merra2(path, domain, time_res)
  download_terraclimate(path, domain, time_res)
  download_zheng(path, domain, time_res)
}