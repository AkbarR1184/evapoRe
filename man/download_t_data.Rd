% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_t_data.R
\name{download_t_data}
\alias{download_t_data}
\title{Temperature Data Downloader}
\usage{
download_t_data(
  dataset,
  path = ".",
  domain = "raw",
  time_res = "monthly",
  variables = "all"
)
}
\arguments{
\item{dataset}{a character string indicating the dataset to download. Suitable options are:
\itemize{
\item{"terraclimate" for TerraClimate dataset,}
\item{"em_earth" for EM-Earth dataset.}
}}

\item{path}{a character string with the path where the data will be downloaded.}

\item{domain}{a character string with the desired domain data set. Suitable options are:
\itemize{
\item{"raw" for default available spatial coverage,}
\item{"global" for data sets with global (land and ocean) coverage,}
\item{"land" for data sets with land only coverage,}
\item{"ocean" for data sets with ocean only coverage.}
}}

\item{time_res}{a character string with the desired time resolution. Suitable options are:
\itemize{
\item{"monthly",}
\item{"yearly".}
}}

\item{variables}{a character string indicating the variable to download for each dataset. Suitable options are:
For TerraClimate dataset:
\itemize{
\item{"tavg" for average temperature,}
\item{"tmin" for minimum temperature,}
\item{"tmax" for maximum temperature.}
}
For EM-Earth dataset:
\itemize{
\item{"tdew" for dew point temperature,}
\item{"tavg" for average temperature,}
\item{"trange" for temperature range.}
}
Use "all" to download all available variables for the dataset.}
}
\value{
No return value, called to download the data set.
}
\description{
Function for downloading Temperature data from different datasets.
}
\keyword{internal}
