% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_lmp_locations.R
\name{merge_lmp_locations}
\alias{merge_lmp_locations}
\title{Takes instance dataframe and merges with location and latitude longitude data}
\usage{
merge_lmp_locations(instance)
}
\arguments{
\item{instance}{A dataframe containing the LMP instance data to be merged with location data.}
}
\value{
A dataframe containing both the LMP instance data and the location information.
}
\description{
This function takes an instance dataframe (with LMP data) and merges it with location data
(including city names and corresponding latitude and longitude), returning a dataframe with
both the LMP and location information.
}
