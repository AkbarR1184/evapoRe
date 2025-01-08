#' Download various evapotranspiration data products
#'
#' The function \code{download_data} downloads the selected data product.
#'
#' @importFrom methods is
#' @param data_name a character string with the name(s) of the desired data set. Suitable options are:
#' \itemize{
#' \item{"all" for all of the below listed data sets (default),}
#' \item{"bess" for BESS,}
#' \item{"camele" for CAMELE,}
#' \item{"era5" for ERA5,}
#' \item{"era5-land" for ERA5-Land,}
#' \item{"fldas" for FLDAS,} 
#' \item{"gldas-clsm-v2-0" for GLDAS CLSM version 2.0,}
#' \item{"gldas-clsm-v2-1" for GLDAS CLSM version 2.1,}
#' \item{"gldas-noah-v2-0" for GLDAS NOAH version 2.0,}
#' \item{"gldas-noah-v2-1" for GLDAS NOAH version 2.1,} 
#' \item{"gldas-vic-v2-0" for GLDAS VIC version 2.0,}
#' \item{"gldas-vic-v2-1" for GLDAS VIC version 2.1,}
#' \item{"gleam-v3-7a" for GLEAM version 3.7a,}
#' \item{"gleam-v4-1a" for GLEAM version 4.1a,}
#' \item{"jra-55" for JRA-55,}
#' \item{"merra-2" for MERRA-2,}
#' \item{"terraclimate" for TerraClimate,}
#' \item{"etmonitor" for ETMonitor,}
#' \item{"etsynthesis for SynthesizedET}
#' \item{"sith for Simple Terrestrial Hydrosphere model, version 2}
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
#' \item{"daily",}
#' \item{"monthly",}
#' \item{"yearly".}
#' }
#' @return No return value, called to download the required data sets.
#' @export
#' @examples
#' \donttest{
#' download_data("gldas-vic", tempdir())
#' }

download_data <- function(data_name = "all", path = "", domain = "raw", time_res = "monthly", variable="e"){
  if (!Reduce("&", is.element(data_name, c("all","bess","camele", "era5", "era5-land", "fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "gleam", "jra-55", "merra-2","terraclimate", "zheng")))){
    stop("Error: Data set not available. Select from era5, era5-land, fldas, gldas-clsm, gldas-noah, gldas-vic, gleam, jra-55, merra-2, terraclimate", "zheng")
  }
  old_options <- options()
  on.exit(options(old_options))
  options(timeout = 6000)
  lapply(data_name, function(dataset) switch(dataset,
                                        "all"  = download_all(path, domain, time_res),
                                        "bess" = download_bess(path, domain, time_res),
                                        "camele" = download_camele(path, domain, time_res),
                                        "era5" = download_era5(path, domain, time_res),
                                        "era5-land" = download_era5_land(path, domain, time_res),
                                        "fldas" = download_fldas(path, domain, time_res),
                                        "gldas-clsm" = download_gldas_clsm(path, domain, time_res),
                                        "gldas-noah" = download_gldas_noah(path, domain, time_res),
                                        "gldas-vic" = download_gldas_vic(path, domain, time_res),
                                        "gleam" = download_gleam(path, domain, time_res),
                                        "jra-55" = download_jra55(path, domain, time_res),
                                        "merra-2" = download_merra2(path, domain, time_res),
                                        "terraclimate" = download_terraclimate(path, domain, time_res),
                                        "zheng" = download_zheng(path, domain, time_res)
  ))
  return(invisible())
}
