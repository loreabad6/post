#' Random polygons to exemplify {post} functions
#'
#' Five random polygons with 5 timestamp states are artificially
#' generated to exemplify the main funcitonality of post.
#' See [data-raw/polygons.R] for code on its creation.
#'
#' @format An object of class [sf] with `POLYGON` geometries,
#' with 25 rows and 3 columns:
#' \describe{
#'  \item{gid}{ID of polygon geometry}
#'  \item{tid}{ID of polygon timestamp}
#'  \item{geometry}{the geometry list column}
#' }
"polygons"
