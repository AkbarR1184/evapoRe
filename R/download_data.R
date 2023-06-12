#' Download various evapotranspiration data products
#'
#' The function \code{download_data} downloads the selected data product.
#'
#' @importFrom methods is
#' @param data_name a character string with the name(s) of the desired data set. Suitable options are:
#' \itemize{
#' \item{"all" for all of the below listed data sets (default),}
#' \item{"era5" for ERA5,}
#' \item{"era5_land" for ERA5-Land,}
#' \item{"fldas" for FLDAS,} 
#' \item{"gldas-clsm" for GLDAS CLSM,}
#' \item{"gldas-noah" for GLDAS NOAH,}
#' \item{"gldas-vic" for GLDAS VIC,}
#' \item{"gleam" for GLEAM V3,}
#' \item{"jra_55" for JRA-55,}
#' \item{"merra2" for MERRA-2,}
#' \item{"terraclimate" for TerraClimate,}
#' }
#' @param path a character string with the path where the database will be downloaded.
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
#' @return No return value, called to download the required data sets.
#' @export
#' @examples
#' \donttest{
#' download_data("gldas-vic", tempdir())
#' }

download_data <- function(data_name = "all", path = ".", domain = "raw", time_res = "monthly"){
  if (!Reduce("&", is.element(name, c("all", "era5", "era5_land", "fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "gleam", "jra_55", "merra2","terraclimate")))){
    stop("Error: Data set not available. Select from era5, era5_land, fldas, gldas-clsm, gldas-noah, gldas-vic, gleam, jra_55, merra2, terraclimate")
  }
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  lapply(data_name, function(dataset) switch(dataset,
                                        "all"  = download_all(path, domain, time_res),
                                        "era5" = download_era5(path, domain, time_res),
                                        "era5_land" = download_era5_land(path, domain, time_res),
                                        "fldas" = download_fldas(path, domain, time_res),
                                        "gldas-clsm" = download_gldas_clsm(path, domain, time_res),
                                        "gldas-noah" = download_gldas_noah(path, domain, time_res),
                                        "gldas-vic" = download_gldas_vic(path, domain, time_res),
                                        "gleam" = download_gleam(path, domain, time_res),
                                        "jra55" = download_jra55(path, domain, time_res),
                                        "merra2" = download_merra2(path, domain, time_res),
                                        "terraclimate" = download_terraclimate(path, domain, time_res)
  ))
  return(invisible())
}