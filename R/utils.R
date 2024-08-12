#' @description
#' Utility function to determine duplicated geometries
#' For internal use
#' Copied from https://github.com/luukvdmeer/sfnetworks/blob/main/R/utils.R
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @return A logical vector of the same length as \code{x}.
#'
#' @noRd
#' @importFrom sf st_equals
st_duplicated = function(x) {
  dup = rep(FALSE, length(x))
  dup[unique(do.call("c", lapply(st_equals(x), `[`, - 1)))] = TRUE
  dup
}

#' @description
#' Utility function to compute bounding box per feature.
#' For internal use
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#'
#' @return An object of class \code{\link[sf]{sfc}}.
#'
#' @noRd
#' @importFrom sf st_as_sfc st_bbox st_crs st_set_crs
st_bbox_by_feature = function(x) {
  f = function(y) sf::st_as_sfc(sf::st_bbox(y))
  sf::st_set_crs(do.call("c", lapply(x, f)), sf::st_crs(x))
}
