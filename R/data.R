#' Random polygons to exemplify post functions
#'
#' Five random polygons with 5 time-stamp states are artificially
#' generated to exemplify the main functionality of post.
#' See `data-raw/polygons.R` for code on its creation.
#'
#' @format An object of class [sf] with `POLYGON` geometries,
#' and `Date`
#' with 25 rows and 3 columns:
#' \describe{
#'  \item{gid}{ID of polygon geometry}
#'  \item{datetime}{the datetime of the polygon geometry}
#'  \item{geometry}{the geometry list column}
#' }
"polygons"
