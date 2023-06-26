## ----start, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  eval = TRUE,
  fig.width = 7,
  warning = FALSE,
  message = FALSE
)
library(evapoRe)
library(kableExtra)
data('gldas_clsm_global_ts')
data('gldas_clsm_subset_ts')
data('gldas_clsm_cz_ts')
data('pet_oudin_global_ts')
data('pet_oudin_subset_ts')
data('pet_oudin_cz_ts')

## ----satellite, echo=FALSE, results = 'asis'----------------------------------
tibble::tribble(
  ~"Data Set", ~"Spatial Resolution", ~Global, ~Land, ~Ocean, ~"Temporal Resolution", ~"Record Length", ~"Get Data", ~Reference, 
"GLEAM V3.0", "0.25°", "", "x", "", "Monthly", "1980/01-2021/12", "[Download](https://www.gleam.eu/)", "@martens_gleam_2017"
) |>
  kbl(align = 'lcccccccr') |>
  kable_styling("striped") |>
  add_header_above(c(" " = 1, " " = 1, "Spatial Coverage" = 3, " " = 1, " " = 1, " " = 1, " " = 1)) |>
  unclass() |> cat()

## ----reanalysis, echo=FALSE, echo=FALSE, results = 'asis'---------------------
tibble::tribble(
  ~"Data Set", ~"Spatial Resolution", ~Global, ~Land, ~Ocean, ~"Temporal Resolution", ~"Record Length", ~"Get Data", ~Reference,
"ERA5-Land", "0.1°", "", "x", "", "Monthly", "1960/01-2022/12", "[Download](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=form)", "@munoz-sabater_era5-land_2021",
"ERA5", "0.25°", "x", "x", "x", "Monthly", "1959/01-2021/12", "[Download](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=overview)", "@hersbach_era5_2020",
"JRA-55", "1.25°", "", "x", "", "Monthly", "1958/01-2021/12", "[Download](https://rda.ucar.edu/datasets/ds628.1/dataaccess/)", "@kobayashi_jra-55_2015",
"MERRA-2", "0.5° x 0.625°", "", "x", "", "Monthly", "1980/01-2023/01", "[Download](https://disc.gsfc.nasa.gov/datasets?page=1&project=MERRA-2)", "@gelaro_modern-era_2017"
) |>
  kbl(align = 'lcccccccr') |>
  kable_styling("striped") |>
  add_header_above(c(" " = 1, " " = 1, "Spatial Coverage" = 3, " " = 1, " " = 1, " " = 1, " " = 1)) |>
  unclass() |> cat()

## ----models, echo=FALSE, results = 'asis'-------------------------------------
tibble::tribble(
  ~"Data Set", ~"Spatial Resolution", ~Global, ~Land, ~Ocean, ~"Temporal Resolution", ~"Record Length", ~"Get Data", ~Reference,
"FLDAS", "0.1°", "", "x", "", "Monthly", "1982/01-2022/12", "[Download](https://ldas.gsfc.nasa.gov/fldas/fldas-data-download)", "@mcnally_land_2017",
"GLDAS CLSM V2.1", "1°", "", "x", "", "Monthly", "2000/01-2022/11", "[Download](https://ldas.gsfc.nasa.gov/gldas/gldas-get-data)", "@rodell_global_2004",
"GLDAS NOAH V2.1", "0.25°", "", "x", "", "Monthly", "2000/01-2022/11", "[Download](https://ldas.gsfc.nasa.gov/gldas/gldas-get-data)", "@rodell_global_2004 and @beaudoing_gldas_2020",
"GLDAS VIC V2.1", "1°", "", "x", "", "Monthly", "2000/01-2022/11", "[Download](https://ldas.gsfc.nasa.gov/gldas/gldas-get-data)", "@rodell_global_2004",
"TerraClimate", "4$km$", "", "x", "", "Monthly", "1958/01-2021/12", "[Download](https://www.climatologylab.org/terraclimate.html)", "@abatzoglou_terraclimate_2018"
) |>
  kbl(align = 'lcccccccr') |>
  kable_styling("striped") |>
  add_header_above(c(" " = 1, " " = 1, "Spatial Coverage" = 3, " " = 1, " " = 1, " " = 1, " " = 1)) |>
  unclass() |> cat()

## ----evapoRe_installation, eval = FALSE---------------------------------------
#  devtools::install_github("AkbarR1184/evapoRe") #latest dev version
#  install.packages('evapoRe')                    #latest CRAN release
#  library(evapoRe)

## ----download, eval = FALSE---------------------------------------------------
#  download_data(data_name = 'gldas-clsm')
#  gldas_clsm_global <- raster::brick('gldas-clsm_e_mm_land_200001_202211_025_monthly.nc')
#  show_info(gldas_clsm_global)

## ----gldas_clsm_subset, eval = FALSE------------------------------------------
#  gldas_clsm_subset <- subset_spacetime(gldas_clsm_global, years = c(2001, 2010), bbox = c(2,28,42,58))
#  show_info(gldas_clsm_subset)

## ----gldas_clsm_crop, eval = FALSE--------------------------------------------
#  gldas_clsm_cz <- crop_data(gldas_clsm_subset, shp_path = "gadm41_CZE_0.shp")
#  show_info(gldas_clsm_cz)

## ----download_terraclimate, eval=FALSE----------------------------------------
#  download_terraclimate(variable = "t", folder_path = ".")

## ----subset_terraclimate, eval=FALSE------------------------------------------
#  tavg_global <- raster::brick("terraclimate_tavg_land_19580101_20221231_025_monthly.nc") %>% subset_time(years = c(2001, 2010))
#  show_info(tavg_global)

## ----pet, eval=FALSE----------------------------------------------------------
#  pet_oudin_global <- pet(tavg_global, method = "od") %>% muldpm
#  show_info(pet_oudin_global)

## ----pet_oudin_subset, eval = FALSE-------------------------------------------
#  pet_oudin_subset <- subset_space(pet_oudin_global, bbox = c(2,28,42,58))
#  show_info(pet_oudin_subset)

## ----pet_oudin_crop, eval = FALSE---------------------------------------------
#  pet_oudin_cz <- crop_data(pet_oudin_subset, shp_path = "gadm41_CZE_0.shp")
#  show_info(pet_oudin_cz)

## ----gldas_clsm_global_ts, eval=FALSE-----------------------------------------
#  gldas_clsm_global_ts <- make_ts(gldas_clsm_global, name="gldas-clsm")
#  head(gldas_clsm_global_ts, 12)

## ----gldas_clsm_subset_ts, eval=FALSE-----------------------------------------
#  gldas_clsm_subset_ts <- make_ts(gldas_clsm_subset, name="gldas-clsm")
#  head(gldas_clsm_subset_ts, 12)

## ----gldas_clsm_cz_ts, eval=FALSE---------------------------------------------
#  gldas_clsm_cz_ts <- make_ts(gldas_clsm_cz, name = "gldas-clsm")
#  head(gldas_clsm_cz_ts, 12)

## ----pet_oudin_global_ts, eval=FALSE------------------------------------------
#  pet_oudin_global_ts <- make_ts(pet_oudin_global, name = "terraclimate")
#  head(pet_oudin_global_ts, 12)

## ----pet_oudin_subset_ts, eval=FALSE------------------------------------------
#  pet_oudin_subset_ts <- make_ts(pet_oudin_subset, name = "terraclimate")
#  head(pet_oudin_subset_ts, 12)

## ----pet_odin_cz_ts, eval=FALSE-----------------------------------------------
#  pet_oudin_cz_ts <- make_ts(pet_oudin_cz, name = "terraclimate")
#  head(pet_oudin_cz_ts, 12)

## ----map_global, eval = FALSE-------------------------------------------------
#  plot_map(gldas_clsm_global[[13]])
#  plot_map(pet_oudin_global[[1]])

## ----map_subset, eval = FALSE-------------------------------------------------
#  plot_map(gldas_clsm_subset[[1]])
#  plot_map(pet_oudin_subset[[1]])

## ----map_cz, eval = FALSE-----------------------------------------------------
#  plot_map(gldas_clsm_cz[[1]])
#  plot_map(pet_oudin_cz[[1]])

## -----------------------------------------------------------------------------

# Plotting globals
p01 <- plot_line(gldas_clsm_global_ts, var = "Evapotranspiration")
p02 <- plot_line(pet_oudin_global_ts, var = "Potential Evapotranspiration")
ggpubr::ggarrange(p01, p02, ncol = 1)
# Plotting subsets
p01 <- plot_line(gldas_clsm_subset_ts, var = "ET")
p02 <- plot_line(pet_oudin_subset_ts, var = "PET")
ggpubr::ggarrange(p01, p02, ncol = 2)
# Plotting cz
p01 <- plot_line(gldas_clsm_cz_ts, var = "ET")
p02 <- plot_line(pet_oudin_cz_ts, var = "PET")
ggpubr::ggarrange(p01, p02, ncol = 2)


## -----------------------------------------------------------------------------

# Plotting globals
plot_heatmap(gldas_clsm_global_ts)
plot_heatmap(pet_oudin_global_ts)
# Plotting subsets
p01 <- plot_heatmap(gldas_clsm_subset_ts)
p02 <- plot_heatmap(pet_oudin_subset_ts)
ggpubr::ggarrange(p01, p02, ncol = 2, common.legend = TRUE, legend = "right")
# Plotting cz
p01 <- plot_heatmap(gldas_clsm_cz_ts)
p02 <- plot_heatmap(pet_oudin_cz_ts)
ggpubr::ggarrange(p01, p02, ncol = 2, common.legend = TRUE, legend = "right")


## -----------------------------------------------------------------------------

# Plotting globals
p01 <- plot_box(gldas_clsm_global_ts, var = "ET")
p02 <- plot_box(pet_oudin_global_ts, var = "PET")
ggpubr::ggarrange(p01, p02, ncol = 2)
# Plotting subsets
p01 <- plot_box(gldas_clsm_subset_ts, var = "ET")
p02 <- plot_box(pet_oudin_subset_ts, var = "PET")
ggpubr::ggarrange(p01, p02, ncol = 2)
# Plotting cz
p01 <- plot_box(gldas_clsm_cz_ts, var = "ET" )
p02 <- plot_box(pet_oudin_cz_ts, var = "PET" )
ggpubr::ggarrange(p01, p02, ncol = 2)


## -----------------------------------------------------------------------------
# Plotting globals
p01 <- plot_density(gldas_clsm_global_ts, var = "ET")
p02 <- plot_density(pet_oudin_global_ts, var = "PET")
ggpubr::ggarrange(p01, p02, ncol = 2)
# Plotting subsets
p01 <- plot_density(gldas_clsm_subset_ts, var = "ET")
p02 <- plot_density(pet_oudin_subset_ts, var = "PET")
ggpubr::ggarrange(p01, p02, ncol = 2)
# Plotting cz
p01 <- plot_density(gldas_clsm_cz_ts, var = "ET")
p02 <- plot_density(pet_oudin_cz_ts, var = "PET")
ggpubr::ggarrange(p01, p02, ncol = 2)


## ---- eval=FALSE--------------------------------------------------------------
#  plot_summary(gldas_clsm_global_ts, var = "Evapotranspiration")
#  #plot_summary(gldas_clsm_subset_ts, var = "Evapotranspiration")
#  #plot_summary(gldas_clsm_cz_ts, var = "Evapotranspiration")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  plot_summary(pet_oudin_global_ts, var = "Potential Evapotranspiration")
#  #plot_summary(pet_oudin_subset_ts)
#  #plot_summary(pet_oudin_cz_ts)

