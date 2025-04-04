% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jensen_haise.R
\name{jensen_haise}
\alias{jensen_haise}
\alias{jensen_haise,Raster-method}
\alias{jensen_haise,data.table-method}
\alias{jensen_haise,character-method}
\title{PET calculation by Jensen Haise method}
\usage{
jensen_haise(x)

\S4method{jensen_haise}{Raster}(x)

\S4method{jensen_haise}{data.table}(x)

\S4method{jensen_haise}{character}(x)
}
\arguments{
\item{x}{Raster* object; data.table (see details); filename (character; see details)}
}
\value{
Raster* object; data.table
}
\description{
The function \code{jensen_haise} computes Potential Evapotranspiration (PET) by Jensen Haise method.
}
\details{
If `x` is a data.table, its columns should be named: "lon", "lat", "date", and "value".

If `x` is a filename, it should point to a *.nc file.
}
\keyword{internal}
