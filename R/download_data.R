#' Download various evapotranspiration data products
#'
#' The function \code{download_data} downloads the selected data product.
#'
#' @importFrom methods is
#' @param project_folder a character string with the path where evapoRe will be hosted. Inside it the required subfolders will be created.
#' @param name a character string with the name(s) of the desired data set. Suitable options are:
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
#' @return No return value, called to download the required data sets.
#' @export
#' @examples
#' \donttest{
#' download_data("gldas-vic", tempdir())
#' }

download_data <- function(name = "all", project_folder = "."){
  if (!Reduce("&", is.element(name, c("all", "era5", "era5_land", "fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "gleam", "jra_55", "merra2","terraclimate")))){
    stop("Error: Data set not available. Select from era5, era5_land, fldas, gldas-clsm, gldas-noah, gldas-vic, gleam, jra_55, merra2, terraclimate")
  }
  create_folders(project_folder)
  destination <- paste0(project_folder,"/data/database/")
  old_options <- options()
  options(timeout = 6000)
  on.exit(options(old_options))
  lapply(name, function(dataset) switch(dataset,
                                        "all"  = download_all(destination),
                                        "era5" = download_era5(destination),
                                        "era5_land" = download_era5_land(destination),
                                        "fldas" = download_fldas(destination),
                                        "gldas-clsm" = download_gldas_clsm(destination),
                                        "gldas-noah" = download_gldas_noah(destination),
                                        "gldas-vic" = download_gldas_vic(destination),
                                        "gleam" = download_gleam(destination),
                                        "jra55" = download_jra55(destination),
                                        "merra2" = download_merra2(destination),
                                        "terraclimate" = download_terraclimate(destination)
  ))
  return(invisible())
}