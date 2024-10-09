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

#' @description
#' Utility function to combine geometries
#'
#' @param x An object of class \code{\link[sf]{sf}} or \code{\link[sf]{sfc}}.
#' @param group_id see `?post_array` for details.
#' Defaults to the first non-spatial, non-temporal column in x.
#' @param sf_column_name (character) name of column with changing
#' geometries. Defaults to active `sf_column`.
#' @param do_union Should the st_union of the groupped polygons be computed?
#' The default is TRUE but is a more expensive computation.
#' Otherwise, a cast to MULTIPOLYGON is performed for faster computation.
#'
#' @return An object of class \code{\link[sf]{sfc}}.
#'
#' @noRd
#' @importFrom sf st_make_valid st_combine st_union
st_summarise_polys = function(x, group_id, sf_column_name,
                              do_union = TRUE, .checks = TRUE) {

  if(.checks){
    # group_id: Defaults to the first column of x, if not sfc or temporal class
    group_id_tmp = group_id
    group_id = check_group_id(x, group_id)
    # Assign group_id to "gid_" temp column if vector is given
    if(group_id == "gid_") {
      cli::cli_warn(c(
        "!" = "vector provided for {.var group_id}, assuming correct order
      and unique timestamps per group"
      ))
      x["gid_"] = group_id_tmp
    }
    # Defaults to the active sf_column
    sf_column_name = check_sf_column(x, sf_column_name)
  }

  x_groupped = split(x[[sf_column_name]], x[[group_id]])
  if(do_union) {
    do.call(
      "c",
      lapply(x_groupped, function(i) sf::st_make_valid(sf::st_union(i)))
    )
  } else {
    do.call(
      "c",
      lapply(
        x_groupped,
        function(i) sf::st_make_valid(sf::st_combine(i))
      )
    )
  }
}
