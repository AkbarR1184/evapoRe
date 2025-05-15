#' Download environmental data products
#'
#' The function \code{download_env_data} downloads radiation, temperature, dew point, humidity,
#' albedo, surface pressure, wind speed, and related variables from CRU, ERA5, ERA5-Land, MSWX,
#' FluxCom, and TerraClimate.
#'
#' @importFrom utils download.file
#'
#' @param data_name a character string with the name(s) of the desired data set. Suitable options are:
#' \itemize{
#' \item{"all" for all supported datasets (default),}
#' \item{"cru" for CRU,}
#' \item{"era5" for ERA5,}
#' \item{"era5-land" for ERA5-Land,}
#' \item{"mswx-past" for MSWX (historical),}
#' \item{"fluxcom" for FluxCom,}
#' \item{"terraclimate" for TerraClimate.}
#' }
#'
#' @param path a character string specifying the local folder path where data will be saved.
#'
#' @param domain a character string indicating the spatial domain. Suitable options are:
#' \itemize{
#' \item{"raw" for default spatial coverage,}
#' \item{"land" for land-only coverage (default).}
#' }
#'
#' @param time_res a character string indicating the desired time resolution. Currently, only:
#' \itemize{
#' \item{"monthly"} is supported.
#' }
#'
#' @param variable a character string specifying the variable to download (default = "all").
#' See \code{@details} for the list of available variables.
#'
#' @details
#' Available variables (depending on dataset): t2m, tmin, tmax, ssrd, nr, tdew, r, fal, sp, u10.
#' These correspond to 2-meter air temperature, minimum/maximum temperature, shortwave radiation,
#' net radiation, dew point temperature, relative humidity, surface albedo, atmospheric pressure,
#' and wind speed at 10 meters.
#'
#' @return No return value. The function downloads the requested files to the specified folder.
#' @export
#'
#' @examples
#' \donttest{
#' download_env_data(path = tempdir(), data_name = "cru", variable = "t2m")
#' download_env_data(path = tempdir(), data_name = "all", variable = "all")
#' download_env_data(path = tempdir(), data_name = "mswx-past", variable = "r")
#' }

download_env_data <- function(path = "",
                              data_name = "all",
                              variable = "all",
                              domain = "raw",
                              time_res = "monthly") {
  opts_old <- options()
  on.exit(options(opts_old))
  options(timeout = 6000)
  
  if (domain %in% c("raw", "land")) {
    domain <- "land"
  } else {
    stop("Unsupported domain. Use 'raw' or 'land'.")
  }
  
  dataset_vars <- list(
    "cru" = c("t2m", "tmin", "tmax"),
    "era5-land" = c("fal", "tdew"),
    "era5" = c("ssrd"),
    "mswx-past" = c("r", "sp", "ssrd", "t2m", "tmin", "tmax", "u10"),
    "fluxcom" = c("nr"),
    "terraclimate" = c("t2m", "tmin", "tmax")
  )
  
  valid_datasets <- names(dataset_vars)
  
  if (data_name == "all") {
    datasets <- valid_datasets
  } else if (data_name %in% valid_datasets) {
    datasets <- data_name
  } else {
    stop("Invalid data_name. Choose from: ",
         paste(valid_datasets, collapse = ", "), ", or 'all'.")
  }
  
  if (variable != "all") {
    available_in <- names(Filter(function(x) variable %in% x, dataset_vars))
    if (length(available_in) == 0) {
      stop("Variable '", variable, "' not available in any dataset.")
    }
    if (data_name != "all" && !(data_name %in% available_in)) {
      stop("Variable '", variable, "' not in dataset '", data_name, "'.\n",
           "Available datasets: ", paste(available_in, collapse = ", "), ".")
    }
    datasets <- if (data_name == "all") available_in else data_name
  }
  
  zenodo_base <- "https://zenodo.org/records/15422652/files/"
  zenodo_end <- "?download=1"
  
  for (ds in datasets) {
    vars <- if (variable == "all") dataset_vars[[ds]] else variable
    
    for (var in vars) {
      file_name <- switch(ds,
                          "cru" = paste0("cru_", var, "_degC_land_190101_202212_025_", time_res, ".nc"),
                          
                          "era5-land" = switch(var,
                                               "fal" = "era5-land_fal_01_land_195001_202312_025_monthly.nc",
                                               "tdew" = "era5-land_tdew_degC_land_195001_202312_025_monthly.nc",
                                               stop("Unsupported variable for era5-land.")
                          ),
                          
                          "era5" = switch(var,
                                          "ssrd" = "era5_ssrd_mjm-2_land_195901_202112_025_monthly.nc",
                                          stop("Unsupported variable for era5.")
                          ),
                          
                          "fluxcom" = switch(var,
                                             "nr" = "fluxcom_nr_mjm-2d-1_land_200101_201312_025_monthly.nc",
                                             stop("Unsupported variable for fluxcom.")
                          ),
                          
                          "terraclimate" = paste0("terraclimate_", var, "_degC_land_195801_202212_025_", time_res, ".nc"),
                          
                          "mswx-past" = switch(var,
                                               "r" = "mswx-past_r_pct_land_197901_202308_025_monthly.nc",
                                               "sp" = "mswx-past_sp_kpa_land_197901_202412_025_monthly.nc",
                                               "ssrd" = "mswx-past_ssrd_mjm-2_land_197902_202308_025.nc",
                                               "t2m" = "mswx-past_t2m_degC_land_197901_202308_025_monthly.nc",
                                               "tmin" = "mswx-past_tmin_degC_land_197901_202308_025_monthly.nc",
                                               "tmax" = "mswx-past_tmax_degC_land_197901_202308_025_monthly.nc",
                                               "u10" = "mswx-past_u10_ms-1_land_197901_202412_025_monthly.nc",
                                               stop("Unsupported variable for mswx-past.")
                          ),
                          
                          stop("No filename pattern defined for dataset.")
      )
      
      file_url <- paste0(zenodo_base, file_name, zenodo_end)
      file_dest <- file.path(path, file_name)
      
      message("Downloading ", file_name, " ...")
      try(download.file(file_url, file_dest, mode = "wb"), silent = FALSE)
    }
  }
  
  invisible()
}
