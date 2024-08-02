#' Create a post_array object
#'
#' `post_array` is a spatio-temporal array data structure that
#' organises polygons that change their shape in space and time.
#' It extends the `stars` class for vector data cubes to support
#' changing geometries as attributes.
#' The `post_array` class supports two dimensions: `geom_sum` and
#' `datetime`. `geom_sum` is the summary geometry of the changing
#' shapes of the polygon geometries.
#'
#' @param x object of class `sf` with `POLYGON` geometries
#'
#' @param group_id (character) name of column containing the grouping
#' identifier for the changing polygons.
#'
#' @param time_column_name (character) name of column with the datetimes
#'
#' @param sf_column_name (character) name of column with changing
#' geometries. Defaults to active `sf_column`.
#'
#' @param geometry_summary (character) function to compute the summary
#' geometry. Supported arguments are `union` to compute the union
#' and dissolve of the changing geometries per group, `centroid` to
#' compute the centroid of the union and dissolve per group, and
#' `bbox` to compute the bbox of the union and dissolve per group.
#'
#' @examples
#' as_post_array(polygons, "gid", "datetime")
#'
#' library(sf, quietly = TRUE)
#' polygons |>
#'   mutate(area = st_area(geometry)) |>
#'   as_post_array("gid", "datetime", geometry_summary = "union")
#' @return an object of class `post_array`.
#'
#' @references
#' Abad, L., Sudmanns, M., and Hölbling, D. (2024)
#' Vector data cubes for features evolving in space and time,
#' *AGILE GIScience Ser.*, 5, 16,
#' <https://doi.org/10.5194/agile-giss-5-16-2024>
#'
#' @export
as_post_array = function(x, group_id, time_column_name,
                         sf_column_name = attr(x, "sf_column"),
                         geometry_summary = "centroid",
                         attributes = NULL) {
  UseMethod("as_post_array")
}

#' @name as_post_array
#'
#' @export
# TODO: not working examples when doing check, figure out why
as_post_array.sf = function(x, group_id, time_column_name,
                            sf_column_name = attr(x, "sf_column"),
                            geometry_summary = "centroid") {

  # Set dimensions
  # TODO: should more dimensions be supported?
  # I would argue no since it is space-time polygons,
  # but then things like polygon changes between time
  # periods could be stored too
  dims = c(
    length(unique(x[[group_id]])),
    length(unique(x[[time_column_name]]))
  )

  # Create attribute arrays
  create_array = function(attribute_col) {
    array(data = x[[attribute_col]], dim = dims)
  }

  # Create geometry array
  a_geom = create_array(attribute_col = sf_column_name)

  # Create array for remaining attributes
  # TODO: pass by default all attributes that are not
  # used as dimensions here
  attrs = names(x)
  attrs = attrs[!attrs %in% c(group_id, time_column_name, sf_column_name)]
  a_attr = lapply(attrs, create_array)
  names(a_attr) = attrs

  # Compute summary geometry
  geom_sum = switch(
    geometry_summary,
    union = compute_geom_summary_union(x, group_id, sf_column_name),
    centroid = compute_geom_summary_centroid(x, group_id, sf_column_name),
    bbox = compute_geom_summary_bbox(x, group_id, sf_column_name),
    # TODO: let the user pass a summary geometry from a geometry column name
    stop(
      "Unsupported summary geometry function: ",
      geometry_summary,
      call. = FALSE
    )
  )

  # Create dimensions object
  d = stars::st_dimensions(
    geom_sum = geom_sum,
    datetime = unique(x[[time_column_name]]),
    # TODO: handle point parameter if more than two dimensions
    # The point parameter indicates if the value refers to
    # a point (location) or to a pixel (area) value
    point = c(TRUE, FALSE)
  )

  # Coerce to cube
  out = stars::st_as_stars(
    c(geometry = list(a_geom), a_attr),
    dimensions = d
  )

  # Give post_array class
  class(out) = c("post_array", class(out))

  # Return post_array object
  out
}

# Compute the geometry summary as the union and dissolve of
# the changing geometries
# INTERNAL USE!
compute_geom_summary_union = function(x, group_id, sf_column_name) {
  x_groupped = split(x[[sf_column_name]], x[[group_id]])
  x_summarised = do.call(
    "c",
    lapply(x_groupped, function(i) st_make_valid(st_union(i)))
  )
  x_summarised
}

# Compute the geometry summary as the centroid of the
# union and dissolve of the changing geometries
# INTERNAL USE!
compute_geom_summary_centroid = function(x, group_id, sf_column_name) {
  x_unioned = compute_geom_summary_union(x, group_id, sf_column_name)
  x_centroid = sf::st_centroid(x_unioned)
  x_centroid
}

# Compute the geometry summary as the bounding box of the
# union and dissolve of the changing geometries
# INTERNAL USE!
compute_geom_summary_bbox = function(x, group_id, sf_column_name) {
  x_unioned = compute_geom_summary_union(x, group_id, sf_column_name)
  x_bbox = st_bbox_by_feature(x_unioned)
  x_bbox
}

# Utility function to compute bounding box per feature
# INTERNAL USE!
st_bbox_by_feature = function(x) {
  f = function(y) sf::st_as_sfc(sf::st_bbox(y))
  do.call("c", lapply(x, f))
}
