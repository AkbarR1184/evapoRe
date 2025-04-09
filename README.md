# evapoRe

**evapoRe** is an R-based application for exploratory data analysis of global EvapoTranspiration (ET) datasets.  
It enables users to download, validate, visualize, and analyze multi-source ET data across various spatio-temporal scales, 
facilitating consistent and reproducible workflows in hydrology, climate, and agricultural research.

## Features
- **12 PET methods** (expanded from 6)
- **19 ET datasets** (expanded from 11)
- **2 daily** and **4 monthly** PET datasets
- **Flexible input types**: `data.table`, `Raster`, or `character` (file paths)

## Installation

### Install the Latest Version from GitHub

```r
if (!require("devtools")) {
  install.packages("devtools")
  library(devtools)
}

devtools::install_github("AkbarR1184/evapoRe")
