#' Complement make_ts generated csv file
#'
#' Function to generate name and type columns
#'
#' @import data.table
#' @param dummie_name a character string
#' @return character string
#' @keywords internal

aux_ts <- function(dummie_name){
  if (dummie_name == "era5-land"){
    dummie <- c("ERA5-Land", "Reanalysis")
  } else if (dummie_name == "era5"){
    dummie <- c("ERA5", "Reanalysis")
  } else if (dummie_name == "gleam"){
    dummie <- c("GLEAM V3", "Reanalysis")
  } else if (dummie_name == "gldas-clsm"){
    dummie <- c("GLDAS CLSM v2.0", "Model forcing")
  } else if (dummie_name == "gldas-noah"){
    dummie <- c("GLDAS NOAH v2.0", "Model forcing")
  } else if (dummie_name == "gldas-vic"){
    dummie <- c("GLDAS VIC v2.0", "Model forcing")
  } else if (dummie_name == "terraclimate"){
    dummie <- c("TerraClimate", "Model forcing")
  } else if (dummie_name == "jra55"){
    dummie <- c("JRA-55", "Reanalysis")
  } else if (dummie_name == "merra2"){
    dummie <- c("MERRA-2", "Reanalysis")
  } else if (dummie_name == "fldas"){
    dummie <- c("FLDAS", "Model forcing")
  } else {
    dummie <- c(NA, NA)
  }
  return(dummie)
}