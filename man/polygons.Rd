% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{polygons}
\alias{polygons}
\title{Random polygons to exemplify post functions}
\format{
An object of class \link{sf} with \code{POLYGON} geometries,
and \code{Date}
with 25 rows and 3 columns:
\describe{
\item{gid}{ID of polygon geometry}
\item{datetime}{the datetime of the polygon geometry}
\item{geometry}{the geometry list column}
}
}
\usage{
polygons
}
\description{
Five random polygons with 5 time-stamp states are artificially
generated to exemplify the main functionality of post.
See \code{data-raw/polygons.R} for code on its creation.
}
\keyword{datasets}
