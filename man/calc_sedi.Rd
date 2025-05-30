% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_sedi.R
\name{calc_sedi}
\alias{calc_sedi}
\title{Calculate Standardized Evapotranspiration Deficit Index (SEDI)}
\usage{
calc_sedi(x, scale = 1)
}
\arguments{
\item{x}{A `data.table` with columns: "lon", "lat", "date", "aet", "pet"}

\item{scale}{Integer. Number of months to aggregate over (e.g., 1 = monthly, 6 = SEDI-6)}
}
\value{
A `data.table` with columns: "lon", "lat", "date", "value" (SEDI)
}
\description{
Computes the standardized anomaly of (ET − PET) over a defined time scale.
}
\details{
SEDI is calculated by aggregating (ET − PET) over a moving window of `scale` months
and standardizing the anomaly per calendar month and location. Rolling aggregation uses
`frollsum()` from the `data.table` package.
}
\references{
Zhang, X., et al. (2019). Assessment of an evapotranspiration deficit drought index in relation to impacts on ecosystems.
Advances in Atmospheric Sciences, 36, 1273–1287. https://doi.org/10.1007/s00376-019-9061-6
}
