#' Summarise changing geometries based on a group identifier
#'
#' Changing geometries are summarised for all time periods as a single geometry
#' (of types `POINT`, `POLYGON`, `MULTIPOLYGON`) in order to pass them
#' as a spatial dimension to `post_*` objects.
#' The family of functions `summarise_geometry_*()` or `summarize_geometry_*()`
#' are helpers to create these summarised geometries.
#'
#' @details
#' A custom function to summarise geometries can be passed onto the
#' coercion functions `as_post_*()`, given that the parameters `x`,
#' `group_id` and `sf_column_name` are included in the function.
#' Additional arguments can be passed to the function as necessary.
#' See `vignette("post02_geomsum")` for more information.
#'
#' @inherit as_post_array details
#'
#' @examples
#' # union and dissolve
#' summarise_geometry_union(polygons)
#' # centroid
#' summarise_geometry_centroid(polygons)
#' # bounding box or minimum unrotated rectangle
#' summarise_geometry_bbox(polygons)
#' # minimum rotated rectangle
#' summarise_geometry_bbox(polygons, rotated = TRUE)
#' # convex hull
#' summarise_geometry_convex_hull(polygons)
#'
#' @family Changing geometries summarisers
#' @name summarise_geometry
#'
#' @return object of class `sfc` with summarised geometries based on group_id
NULL

#' @describeIn summarise_geometry computes the geometry summary as the union and
#' dissolve of the changing geometries
#' @inheritParams as_post_array
#' @param x object `POLYGON`/`MULTIPOLYGON` changing geometries to summarise
#' based on `group_id`.
#' @param group_id see `?post_array` for details.
#' Defaults to the first non-spatial, non-temporal column in x.
#' @param .checks internal, should creation arguments be checked?
#' @export
summarise_geometry_union = function(x,
                                    group_id = NULL,
                                    sf_column_name = NULL,
                                    .checks = TRUE) {
  x_unioned = st_summarise_polys(x,
                                 group_id = group_id,
                                 sf_column_name = sf_column_name,
                                 do_union = TRUE,
                                 .checks = .checks)
  x_unioned
}

#' @rdname summarise_geometry
#' @export
summarize_geometry_union = summarise_geometry_union

#' @describeIn summarise_geometry computes the geometry summary as the centroid
#' of the union and dissolve of the changing geometries
#' @inheritParams summarise_geometry_union
#' @importFrom sf st_centroid
#' @export
summarise_geometry_centroid = function(x,
                                       group_id = NULL,
                                       sf_column_name = NULL,
                                       .checks = TRUE) {
  x_unioned = st_summarise_polys(x,
                                 group_id = group_id,
                                 sf_column_name = sf_column_name,
                                 do_union = TRUE,
                                 .checks = .checks)
  x_centroid = sf::st_centroid(x_unioned)
  x_centroid
}

#' @rdname summarise_geometry
#' @export
summarize_geometry_centroid = summarise_geometry_centroid

#' @describeIn summarise_geometry computes the geometry summary as the bounding box or
#' minimum rectangle of the union and dissolve of the changing geometries
#' @param rotated (logical) if TRUE, the minimum rotated rectangle is returned,
#' if FALSE the minimum unrotated rectangle or bounding box is returned.
#' Defaults to FALSE
#' @inheritParams summarise_geometry_union
#' @importFrom sf st_minimum_rotated_rectangle
#' @export
summarise_geometry_bbox = function(x,
                                   group_id = NULL,
                                   sf_column_name = NULL,
                                   .checks = TRUE,
                                   rotated = FALSE) {
  x_unioned = st_summarise_polys(x,
                                 group_id = group_id,
                                 sf_column_name = sf_column_name,
                                 do_union = FALSE,
                                 .checks = .checks)
  if(rotated) {
    x_mrr = sf::st_minimum_rotated_rectangle(x_unioned)
    x_mrr
  } else {
    x_bbox = st_bbox_by_feature(x_unioned)
    x_bbox
  }
}

#' @rdname summarise_geometry
#' @export
summarize_geometry_bbox = summarise_geometry_bbox

#' @describeIn summarise_geometry computes the geometry summary as the
#' convex hull of the union and dissolve of the changing geometries
#' @inheritParams summarise_geometry_union
#' @importFrom sf st_convex_hull
#' @export
summarise_geometry_convex_hull = function(x,
                                       group_id = NULL,
                                       sf_column_name = NULL,
                                       .checks = TRUE) {
  x_unioned = st_summarise_polys(x,
                                 group_id = group_id,
                                 sf_column_name = sf_column_name,
                                 do_union = TRUE,
                                 .checks = .checks)
  x_conv_hull = sf::st_convex_hull(x_unioned)
  x_conv_hull
}

#' @rdname summarise_geometry
#' @export
summarize_geometry_convex_hull = summarise_geometry_convex_hull
