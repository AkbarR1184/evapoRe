#' Download environmental data products
#'
#' Downloads radiation, temperature, dew point, humidity, albedo, surface pressure,
#' wind speed, and related data from CRU, ERA5-Land, MSWX, FluxCom, and TerraClimate.
#'
#' @importFrom utils download.file
#' @param path Character; local path where data will be saved.
#' @param data_name Character; dataset name or "all". Options:
#' \itemize{
#' \item{"cru",}
#' \item{"era5-land",}
#' \item{"mswx-past",}
#' \item{"fluxcom",}
#' \item{"terraclimate",}
#' \item{"all".}
#' }
#' @param variable Character; variable name or "all". Variables:
#' \itemize{
#' \item{"t2m": 2-meter air temperature (°C),}
#' \item{"tmin": minimum daily temperature (°C),}
#' \item{"tmax": maximum daily temperature (°C),}
#' \item{"ssrd": surface shortwave downward radiation (MJ/m²),}
#' \item{"nr": net radiation (MJ/m²),}
#' \item{"tdew": dew point temperature (°C),}
#' \item{"r": relative humidity (%),}
#' \item{"fal": surface albedo (fraction),}
#' \item{"sp": surface pressure (kPa),}
#' \item{"u10": 10-meter wind speed (m/s).}
#' }
#' Note: Available variables depend on the selected dataset.
#' @param domain Character; spatial domain. Options: "raw", "land".
#' @param time_res Character; time resolution. Currently only "monthly" supported.
#' @return Invisible; downloads selected data to the specified path.
#' @export
#' @examples
#' \donttest{
#' download_env_data(path = tempdir(), data_name = "mswx-past", variable = "all")
#' download_env_data(path = tempdir(), data_name = "cru", variable = "t2m")
#' download_env_data(path = tempdir(), data_name = "all", variable = "all")
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
  
  zenodo_base <- "https://zenodo.org/records/15351154/files/"
  zenodo_end <- "?download=1"
  
  for (ds in datasets) {
    vars <- if (variable == "all") dataset_vars[[ds]] else variable
    
    for (var in vars) {
      file_name <- switch(ds,
                          "cru" = paste0("cru_", var, "_degC_land_190101_202212_025_", time_res, ".nc"),
                          "era5-land" = paste0("era5-land_", var, "_land_195001_2023_12_025_", time_res, ".nc"),
                          "mswx-past" = paste0("mswx-past_", var, "_land_197901_202412_025_", time_res, ".nc"),
                          "fluxcom" = paste0("fluxcom_", var, "_land_200101_201312_025_", time_res, ".nc"),
                          "terraclimate" = paste0("terraclimate_", var, "_degC_land_195801_202212_025_",
                                                  time_res, ".nc"),
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
