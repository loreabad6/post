#' Summarise changing geometries based on a group identifier
#'
#' Changing geometries are summarised for all time periods as a single geometry
#' (of types `POINT`, `POLYGON`, `MULTIPOLYGON`) in order to pass them
#' as a spatial dimension to `post_*` objects.
#'
#' The family of functions `summarise_geometry_*()` or `summarize_geometry_*()`
#' are helpers to create these summarised geometries.
#' Supported summary types are:
#'
#' a. the union and dissolve of the changing geometries,
#'
#' b. the centroid of **a**
#'
#' c. the bbox of **a**
#'
#' A custom function to summarise geometries can be passed onto the
#' coercion functions `as_post_*()`, given that the parameters `x`,
#' `group_id` and `sf_column_name` are included in the function.
#' Additional arguments can be passed to the function as necessary.
#'
#' @examples
#' summarise_geometry_union(polygons)
#' summarise_geometry_centroid(polygons)
#' summarise_geometry_bbox(polygons)
#'
#' @family Changing geometries summarisers
#' @name summarise_geometry
#'
#' @return object of class `sfc` with summarised geometries based on group_id
NULL

#' @description
#' `summarise_geometry_union` computes the geometry summary as the union and dissolve of
#' the changing geometries
#'
#' @inheritParams as_post_array
#' @inheritSection as_post_array details
#'
#' @param x object `POLYGON`/`MULTIPOLYGON` changing geometries to summarise
#' based on `group_id`.
#' @param .checks perform argument check?
#'
#' @rdname summarise_geometry
#' @importFrom sf st_union st_make_valid
#' @export
summarise_geometry_union = function(x,
                                    group_id = NULL,
                                    sf_column_name = NULL,
                                    .checks = TRUE) {

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
  x_summarised = do.call(
    "c",
    lapply(x_groupped, function(i) sf::st_make_valid(sf::st_union(i)))
  )
  x_summarised
}

#' @rdname summarise_geometry
#' @export
summarize_geometry_union = summarise_geometry_union

#' @description
#' `summarise_geometry_centroid` computes the geometry summary as the centroid of the
#' union and dissolve of the changing geometries
#'
#' @inheritParams summarise_geometry_union
#'
#' @rdname summarise_geometry
#' @importFrom sf st_centroid
#' @export
summarise_geometry_centroid = function(x,
                                       group_id = NULL,
                                       sf_column_name = NULL,
                                       .checks = TRUE) {
  x_unioned = summarise_geometry_union(x,
                                       group_id = group_id,
                                       sf_column_name = sf_column_name,
                                       .checks = .checks)
  x_centroid = sf::st_centroid(x_unioned)
  x_centroid
}

#' @rdname summarise_geometry
#' @export
summarize_geometry_centroid = summarise_geometry_centroid

#' @description
#' `summarise_geometry_bbox` computes the geometry summary as the bounding box of the
#' union and dissolve of the changing geometries
#'
#' @inheritParams summarise_geometry_union
#'
#' @rdname summarise_geometry
#' @export
summarise_geometry_bbox = function(x,
                                   group_id = NULL,
                                   sf_column_name = NULL,
                                   .checks = TRUE) {
  x_unioned = summarise_geometry_union(x,
                                       group_id = group_id,
                                       sf_column_name = sf_column_name,
                                       .checks = .checks)
  x_bbox = st_bbox_by_feature(x_unioned)
  x_bbox
}

#' @rdname summarise_geometry
#' @export
summarize_geometry_bbox = summarise_geometry_bbox
