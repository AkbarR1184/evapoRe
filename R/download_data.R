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
#' \item{"etsynthesis" for SynthesizedET,}
#' \item{"sith" for Simple Terrestrial Hydrosphere model, version 2.}
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
#' @param variable a character string specifying the variable to download (default = "e").
#' @return No return value, called to download the required data sets.
#' @export
#' @examples
#' \donttest{
#' download_data("gldas-vic-v2-1", tempdir())
#' }

download_data <- function(data_name = "all",
                          path = "",
                          domain = "raw",
                          time_res = "monthly",
                          variable = "e") {
  
  valid_datasets <- c(
    "all", 
    "bess", 
    "camele", 
    "era5", 
    "era5-land", 
    "fldas", 
    "gldas-clsm-v2-0", 
    "gldas-clsm-v2-1", 
    "gldas-noah-v2-0", 
    "gldas-noah-v2-1", 
    "gldas-vic-v2-0", 
    "gldas-vic-v2-1", 
    "gleam-v3-7a", 
    "gleam-v4-1a", 
    "jra-55", 
    "merra-2", 
    "terraclimate", 
    "etmonitor", 
    "etsynthesis", 
    "sith"
  )
  
  if (!all(data_name %in% valid_datasets)) {
    stop(
      "Error: Some requested data set(s) not available.\n",
      "Valid options are: ", paste(valid_datasets, collapse = ", ")
    )
  }
  
  old_options <- options()
  on.exit(options(old_options))
  options(timeout = 6000)
  
  lapply(data_name, function(dataset) {
    switch(
      dataset,
      "all"                = download_all(path, domain, time_res, variable),
      "bess"               = download_bess(path, domain, time_res, variable),
      "camele"             = download_camele(path, domain, time_res, variable),
      "era5"               = download_era5(path, domain, time_res, variable),
      "era5-land"          = download_era5_land(path, domain, time_res, variable),
      "fldas"              = download_fldas(path, domain, time_res, variable),
      "gldas-clsm-v2-0"    = download_gldas_clsm(path, domain, time_res, variable, version = "v2-0"),
      "gldas-clsm-v2-1"    = download_gldas_clsm(path, domain, time_res, variable, version = "v2-1"),
      "gldas-noah-v2-0"    = download_gldas_noah(path, domain, time_res, variable, version = "v2-0"),
      "gldas-noah-v2-1"    = download_gldas_noah(path, domain, time_res, variable, version = "v2-1"),
      "gldas-vic-v2-0"    = download_gldas_vic(path, domain, time_res, variable, version = "v2-0"),
      "gldas-vic-v2-1"    = download_gldas_vic(path, domain, time_res, variable, version = "v2-1"),
      "gleam-v3-7a"        = download_gleam(path, domain, time_res, variable, version = "v3-7a"),
      "gleam-v4-1a"        = download_gleam(path, domain, time_res, variable, version = "v4-1a"),
      "jra-55"             = download_jra55(path, domain, time_res, variable),
      "merra-2"            = download_merra2(path, domain, time_res, variable),
      "terraclimate"       = download_terraclimate(path, domain, time_res, variable),
      "etmonitor"          = download_etmonitor(path, domain, time_res, variable),
      "etsynthesis"        = download_etsynthesis(path, domain, time_res, variable),
      "sith"               = download_sith(path, domain, time_res, variable)
    )
  })
  
  return(invisible())
}
