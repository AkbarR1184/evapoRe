% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/penman_monteith_f56.R
\name{penman_monteith_f56}
\alias{penman_monteith_f56}
\alias{penman_monteith_f56,missing,missing,missing,missing,missing,missing,missing,missing-method}
\alias{penman_monteith_f56,Raster,Raster,Raster,Raster,Raster,Raster,ANY,ANY-method}
\alias{penman_monteith_f56,character,character,character,character,character,character,ANY,ANY-method}
\title{Calculate Potential Evapotranspiration (PET) using FAO Penman-Monteith}
\usage{
penman_monteith_f56(
  tavg,
  tmin,
  tmax,
  rn,
  u,
  tdew,
  elevation = NULL,
  pres = NULL,
  x = NULL
)

\S4method{penman_monteith_f56}{missing,missing,missing,missing,missing,missing,missing,missing}(x)

\S4method{penman_monteith_f56}{Raster,Raster,Raster,Raster,Raster,Raster,ANY,ANY}(
  tavg,
  tmin,
  tmax,
  rn,
  u,
  tdew,
  elevation = NULL,
  pres = NULL,
  x = NULL
)

\S4method{penman_monteith_f56}{character,character,character,character,character,character,ANY,ANY}(
  tavg,
  tmin,
  tmax,
  rn,
  u,
  tdew,
  elevation = NULL,
  pres = NULL,
  x = NULL
)
}
\arguments{
\item{tavg}{Raster* object or file path; average temperature (°C)}

\item{tmin}{Raster* object or file path; minimum temperature (°C)}

\item{tmax}{Raster* object or file path; maximum temperature (°C)}

\item{rn}{Raster* object or file path; net radiation (MJ m-2 day-1)}

\item{u}{Raster* object or file path; wind speed at 2 meters (m/s)}

\item{tdew}{Raster* object or file path; dew point temperature (°C)}

\item{elevation}{Raster* object or file path; elevation (m). Optional if `pres` is provided.}

\item{pres}{Optional. Raster* object or file path; atmospheric pressure (kPa)}

\item{x}{A `data.table` with columns: "lon", "lat", "date", 
"tavg", "tmin", "tmax", "rn", "u", "tdew", and either "elevation" or "pres".}
}
\value{
RasterBrick or data.table of PET values (mm/day)
}
\description{
The function \code{penman_monteith_f56} computes PET by FAO Penman-Monteith method.
}
\details{
For Raster inputs, provide raster objects or file paths for `tavg`, `tmin`, 
`tmax`, `rn`, `u`, `tdew`, and either `elevation` or `pres`. For data.table 
input, provide a single `data.table` with columns: "lon", "lat", "date", 
"tavg", "tmin", "tmax", "rn", "u", "tdew", and either "elevation" or "pres".
}
\keyword{internal}
