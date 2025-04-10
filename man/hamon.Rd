% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hamon.R
\name{hamon}
\alias{hamon}
\alias{hamon,Raster-method}
\alias{hamon,character-method}
\alias{hamon,data.table-method}
\title{Calculate Potential Evapotranspiration (PET) using Hamon method}
\usage{
hamon(x)

\S4method{hamon}{Raster}(x)

\S4method{hamon}{character}(x)

\S4method{hamon}{data.table}(x)
}
\arguments{
\item{x}{Raster* object, character file path, or data.table (see details)}
}
\value{
RasterBrick or data.table of PET values (mm/day)
}
\description{
The function \code{hamon} computes PET using the Hamon method.
}
\details{
For Raster input, provide a raster object or file path for average temperature.
For `data.table` input, provide a table with columns: "lon", "lat", "date", and "tavg".
}
\keyword{internal}
