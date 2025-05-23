% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_pet_requirements.R
\name{pet_method_requirements}
\alias{pet_method_requirements}
\title{Required Input Variables for PET Methods}
\usage{
pet_method_requirements(x = NULL)
}
\arguments{
\item{x}{Optional. A character string naming a PET method. If NULL (default),
the function returns a named list of all available methods and their required
input variables.}
}
\value{
A character vector of required input variables (if \code{x} is
provided), or a named list of all PET methods and their required inputs
(if \code{x} is NULL).
}
\description{
This function returns the required input variables for a given PET method,
or all methods if none is specified.
}
\examples{
\donttest{
# PET method requirements
pet_method_requirements()
}
}
