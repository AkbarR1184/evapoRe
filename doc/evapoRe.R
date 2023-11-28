## ----start, include = FALSE--------------------------------------------------------
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
data('gldas_clsm_esp_ts')
data('pet_oudin_global_ts')
data('pet_oudin_subset_ts')
data('pet_oudin_esp_ts')


## ----satellite, echo=FALSE, results = 'asis'---------------------------------------
tibble::tribble(
  ~"Data Set", ~"Spatial Resolution", ~Global, ~Land, ~Ocean, ~"Temporal Resolution", ~"Record Length", ~"Get Data", ~Reference, 
"GLEAM V3.7b", "0.25°", "", "x", "", "Monthly", "1980/01-2021/12", "[Download](https://www.gleam.eu/)", "@martens_gleam_2017",
"BESS V2.0", "0.05°", "", "x", "", "Monthly", "1982/01-2019/12", "[Download](https://www.environment.snu.ac.kr/bessv2)", "@li2023bessv2",
"ETMonitor", "1$km$", "", "x", "", "Daily", "2000/06-2019/12", "[Download](https://data.casearth.cn/en/sdo/detail/63291c7e08415d54af833fe5)", "@zheng2022ETMonitor"
) |>
  kbl(align = 'lcccccccr') |>
  kable_styling("striped") |>
  add_header_above(c(" " = 1, " " = 1, "Spatial Coverage" = 3, " " = 1, " " = 1, " " = 1, " " = 1)) |>
  unclass() |> cat()


## ----reanalysis, echo=FALSE, echo=FALSE, results = 'asis'--------------------------
tibble::tribble(
  ~"Data Set", ~"Spatial Resolution", ~Global, ~Land, ~Ocean, ~"Temporal Resolution", ~"Record Length", ~"Get Data", ~Reference,
"ERA5-Land", "0.1°", "", "x", "", "Monthly", "1960/01-2022/12", "[Download](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=form)", "@munoz-sabater_era5-land_2021",
"ERA5", "0.25°", "", "x", "", "Monthly", "1959/01-2021/12", "[Download](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=overview)", "@hersbach_era5_2020",
"JRA-55", "1.25°", "", "x", "", "Monthly", "1958/01-2021/12", "[Download](https://rda.ucar.edu/datasets/ds628.1/dataaccess/)", "@kobayashi_jra-55_2015",
"MERRA-2", "0.5° x 0.625°", "", "x", "", "Monthly", "1980/01-2023/01", "[Download](https://disc.gsfc.nasa.gov/datasets?page=1&project=MERRA-2)", "@gelaro_modern-era_2017",
"CAMELE", "0.25°", "", "x", "", "Monthly", "1980/01-2022/12", "[Download](https://zenodo.org/records/8047038)", "@li2023camele"
) |>
  kbl(align = 'lcccccccr') |>
  kable_styling("striped") |>
  add_header_above(c(" " = 1, " " = 1, "Spatial Coverage" = 3, " " = 1, " " = 1, " " = 1, " " = 1)) |>
  unclass() |> cat()


## ----models, echo=FALSE, results = 'asis'------------------------------------------
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


## ----evapoRe_installation, eval = FALSE--------------------------------------------
## devtools::install_github("AkbarR1184/evapoRe") #latest dev version
## install.packages('evapoRe')                    #latest CRAN release
## library(evapoRe)


## ----download, eval = FALSE--------------------------------------------------------
## download_data(data_name = 'gldas-clsm', path = ".")
## gldas_clsm_global <- raster::brick('gldas-clsm_e_mm_land_200001_202211_025_monthly.nc')
## infoNC(gldas_clsm_global)


## ----gldas-clsm_subset, eval = FALSE-----------------------------------------------
## gldas_clsm_subset <- subset_data(gldas_clsm_global,box = c(-10,40,30,45) ,yrs = c(2001, 2010))
## infoNC(gldas_clsm_subset)


## ----gldas-clsm_crop, eval = FALSE-------------------------------------------------
## gldas_clsm_esp <- crop_data(gldas_clsm_subset, "gadm41_ESP_0.shp")
## infoNC(gldas_clsm_esp)


## ----download_mswx, eval=FALSE-----------------------------------------------------
## download_t_data(data_name ="mswx", variable = "t2m", path = ".")


## ----subset_mswx, eval=FALSE-------------------------------------------------------
## t2m_global <- raster::brick("mswx_t2m_degC_land_197901_202308_025_monthly.nc") %>%
##   subset_data(yrs = c(2001, 2010))
## infoNC(t2m_global)


## ----pet, eval=FALSE---------------------------------------------------------------
## pet_oudin_global <- pet(t2m_global, method = "od") %>% muldpm
## infoNC(pet_oudin_global)


## ----pet_oudin_subset, eval = FALSE------------------------------------------------
## pet_oudin_subset <- subset_data(pet_oudin_global, box = c(-10,40,30,45))
## infoNC(pet_oudin_subset)


## ----pet_oudin_crop, eval = FALSE--------------------------------------------------
## pet_oudin_esp <- crop_data(pet_oudin_subset, "gadm41_ESP_0.shp")
## infoNC(pet_oudin_esp)


## ----gldas_clsm_global_ts, eval=FALSE----------------------------------------------
## gldas_clsm_global_ts <- fldmean(gldas_clsm_global)
## head(gldas_clsm_global_ts, 12)


## ----gldas_clsm_subset_ts, eval=FALSE----------------------------------------------
## gldas_clsm_subset_ts <- fldmean(gldas_clsm_subset)
## head(gldas_clsm_subset_ts, 12)


## ----gldas_clsm_esp_ts, eval=FALSE-------------------------------------------------
## gldas_clsm_esp_ts <- fldmean(gldas_clsm_esp)
## head(gldas_clsm_esp_ts, 12)


## ----pet_oudin_global_ts, eval=FALSE-----------------------------------------------
## pet_oudin_global_ts <- fldmean(pet_oudin_global)
## head(pet_oudin_global_ts, 12)


## ----pet_oudin_subset_ts, eval=FALSE-----------------------------------------------
## pet_oudin_subset_ts <- fldmean(pet_oudin_subset)
## head(pet_oudin_subset_ts, 12)


## ----pet_odin_esp_ts, eval=FALSE---------------------------------------------------
## pet_oudin_esp_ts <- fldmean(pet_oudin_esp)
## head(pet_oudin_esp_ts, 12)


## ----map_global, eval = F----------------------------------------------------------
## plot_map(gldas_clsm_global[[18]])
## plot_map(pet_oudin_global[[6]])


## ----map_subset, eval = FALSE------------------------------------------------------
## plot_map(gldas_clsm_subset[[6]])
## plot_map(pet_oudin_subset[[6]])


## ----map_esp, eval = FALSE---------------------------------------------------------
## plot_map(gldas_clsm_esp[[6]])
## plot_map(pet_oudin_esp[[6]])


## ----lines, eval = FALSE-----------------------------------------------------------
## p01 <- plot_line(gldas_clsm_global_ts, var = "Evapotranspiration")
## p02 <- plot_line(pet_oudin_global_ts, var = "Potential Evapotranspiration")
## ggpubr::ggarrange(p01, p02, ncol = 1)


## ----lines_subset, eval = FALSE----------------------------------------------------
## p01 <- plot_line(gldas_clsm_subset_ts, var = "ET")
## p02 <- plot_line(pet_oudin_subset_ts, var = "PET")
## ggpubr::ggarrange(p01, p02, ncol = 2)


## ----lines_esp, eval = FALSE-------------------------------------------------------
## p01 <- plot_line(gldas_clsm_esp_ts, var = "ET")
## p02 <- plot_line(pet_oudin_esp_ts, var = "PET")
## ggpubr::ggarrange(p01, p02, ncol = 2)


## ----heatmap_gldas, eval = FALSE---------------------------------------------------
## plot_heatmap(gldas_clsm_global_ts)


## ----heatmap_oudin, eval = FALSE---------------------------------------------------
## plot_heatmap(pet_oudin_global_ts)


## ----heatmap_subset, eval = FALSE--------------------------------------------------
## p01 <- plot_heatmap(gldas_clsm_subset_ts)
## p02 <- plot_heatmap(pet_oudin_subset_ts)
## ggpubr::ggarrange(p01, p02, ncol = 2, common.legend = TRUE, legend = "right")


## ----heatmap_esp, eval = FALSE-----------------------------------------------------
## p01 <- plot_heatmap(gldas_clsm_esp_ts)
## p02 <- plot_heatmap(pet_oudin_esp_ts)
## ggpubr::ggarrange(p01, p02, ncol = 2, common.legend = TRUE, legend = "right")


## ----box, eval = FALSE-------------------------------------------------------------
## p01 <- plot_box(gldas_clsm_global_ts, var = "ET")
## p02 <- plot_box(pet_oudin_global_ts, var = "PET")
## ggpubr::ggarrange(p01, p02, ncol = 2)


## ----box_subset, eval = FALSE------------------------------------------------------
## p01 <- plot_box(gldas_clsm_subset_ts, var = "ET")
## p02 <- plot_box(pet_oudin_subset_ts, var = "PET")
## ggpubr::ggarrange(p01, p02, ncol = 2)


## ----box_esp, eval = FALSE---------------------------------------------------------
## p01 <- plot_box(gldas_clsm_esp_ts, var = "ET" )
## p02 <- plot_box(pet_oudin_esp_ts, var = "PET" )
## ggpubr::ggarrange(p01, p02, ncol = 2)


## ----density, eval = FALSE---------------------------------------------------------
## p01 <- plot_density(gldas_clsm_global_ts, var = "ET")
## p02 <- plot_density(pet_oudin_global_ts, var = "PET")
## ggpubr::ggarrange(p01, p02, ncol = 2)


## ----density_subset, eval = FALSE--------------------------------------------------
## p01 <- plot_density(gldas_clsm_subset_ts, var = "ET")
## p02 <- plot_density(pet_oudin_subset_ts, var = "PET")
## ggpubr::ggarrange(p01, p02, ncol = 2)


## ----density_esp, eval = FALSE-----------------------------------------------------
## p01 <- plot_density(gldas_clsm_esp_ts, var = "ET")
## p02 <- plot_density(pet_oudin_esp_ts, var = "PET")
## ggpubr::ggarrange(p01, p02, ncol = 2)


## ----eval=FALSE--------------------------------------------------------------------
## plot_summary(gldas_clsm_global_ts, var = "Evapotranspiration")
## #plot_summary(gldas_clsm_subset_ts, var = "Evapotranspiration")
## #plot_summary(gldas_clsm_esp_ts, var = "Evapotranspiration")
## #plot_summary(pet_oudin_global_ts, var = "Potential Evapotranspiration")
## #plot_summary(pet_oudin_subset_ts)
## #plot_summary(pet_oudin_esp_ts)

