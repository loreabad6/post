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

#' Svalbard glaciers subset to exemplify post functions
#'
#' Selected glacier from the digital glacier dataset
#' compiled and released in König et al. (2013).
#'
#' When the glaciers share the same name for a single year,
#' the polygons are combined into a `MULTIPOLYGON`.
#' In these cases, the maximum `LENGTH` and `FWIDTH` are taken
#' for the feature.
#'
#' The two detached glaciers from Midtre Lovenbreen for the year
#' 2007 where delineated originally in 2009.
#' The year was changed to 2007 to keep congruent data.
#'
#' @references
#' König, M., Kohler J., and Nuth C. (2013).
#' “Glacier Area Outlines - Svalbard 1936-2010.”
#' *Norwegian Polar Institute*.
#' <https://doi.org/10.21334/npolar.2013.89f430f8>.
#'
#' König, M., Nuth, C., Kohler, J., Moholdt, G. and Pettersen, R. (2014).
#' “A Digital Glacier Database for Svalbard.”
#' *Global Land Ice Measurements from Space*, 229–39.
#' Springer Berlin Heidelberg.
#' <https://doi.org/10.1007/978-3-540-79818-7_10>.
#'
#' @name svalbard
#' @format An object of class [sf] with `POLYGON` geometries
#' with 25 rows and 3 columns:
#' \describe{
#'  \item{name}{Glacier name}
#'  \item{year}{The year of glacier delineation}
#'  \item{length}{Maximum glacier length from the glacier tongue to
#'  the head of the accumulation area}
#'  \item{fwidth}{Average width of the glacier tongue}
#'  \item{geom}{the geometry list column}
#' }
#'
#' @examples
#' svalbard = st_read(system.file("extdata/svalbard.gpkg", package="post"))
NULL

