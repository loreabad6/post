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
#' @param x object to convert to `post_array` with `POLYGON`/`MULTIPOLYGON`
#' geometries and a date/time column.
#'
#' @param group_id (character) name of column containing the grouping
#' identifier for the changing polygons or group identifier given as an
#' integer or character value that is assigned to all rows in `x`.
#' Defaults to the first column in `x`.
#'
#' @param time_column_name (character) name of column with the temporal
#' dimension information. Defaults to the first temporal column in `x`.
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
#' if(require(dplyr, quietly = TRUE)) {
#'   library(sf, quietly = TRUE)
#'   polygons |>
#'     mutate(area = st_area(geometry)) |>
#'     as_post_array("gid", "datetime", geometry_summary = "union")
#' }
#'
#' @return an object of class `post_array`.
#'
#' @references
#' Abad, L., Sudmanns, M., and Hölbling, D. (2024)
#' Vector data cubes for features evolving in space and time,
#' *AGILE GIScience Ser.*, 5, 16,
#' <https://doi.org/10.5194/agile-giss-5-16-2024>
#'
#' @export
as_post_array = function(x,
                         group_id = NULL,
                         time_column_name = NULL,
                         sf_column_name = NULL,
                         geometry_summary = "centroid") {
  UseMethod("as_post_array")
}

#' @name as_post_array
#'
#' @importFrom stars st_dimensions st_as_stars
#' @importFrom sf st_agr st_drop_geometry st_geometry_type
#'
#' @export
as_post_array.sf = function(x,
                            group_id = NULL,
                            time_column_name = NULL,
                            sf_column_name = NULL,
                            geometry_summary = "centroid") {

  # TODO: geometry_summary should be exported functions provided to users,
  # call them summarise_/summarize_ (?)
  # then users can pass their own functions after passing a check for validity

  # Set argument defaults
  # group_id can be a character/integer value that is assigned to all rows
  # in x or a column name that contains the group identifiers.
  # Defaults to the first column of x
  if(!is.null(group_id)) {
    if(!(group_id %in% names(x))) {
      x["_gid_"] = group_id
      group_id = "_gid_"
    } else group_id = group_id
  } else if(is.null(group_id)){
    group_id = names(x)[1]
  }

  # time_column_name is the name of the column containing the
  # temporal information. Defaults to the first temporal column.
  if(!is.null(time_column_name)) {
    stopifnot(
      "time_column_name not found" =
      time_column_name %in% names(x)
    )
  } else if(is.null(time_column_name)) {
    # Check for columns matching the temp_class classes
    temp_class = c("POSIXct", "POSIXt", "Date", "PCICt")
    # If there are more than two temporal dimensions, the first one is taken
    x_ = sf::st_drop_geometry(x)
    time_column_name = names(
      x_[which(lapply(x_, class) %in% temp_class)[1]]
    )
    if (is.na(time_column_name))
      stop("x does not have a temporal dimension", call. = FALSE)
  }

  # sf_column_name is the name of the column with the `POLYGON` geometries.
  # Defaults to the active sf_column
  if(!is.null(sf_column_name)) {
    stopifnot(
      "sf_column_name not found" =
      sf_column_name %in% names(x)
    )
  } else if(is.null(sf_column_name)) {
    sf_column_name = attr(x, "sf_column")
  }
  # Check if sf_column has `POLYGON` or `MULTIPOLYGON` geometries
  if(any(!(sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON")))) {
    stop("sf_column is not of type POLYGON/MULTIPOLYGON")
  }

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
    # TODO: let the user pass a summary geometry function from a geometry column name
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
  arrays = c(list(a_geom), a_attr)
  names(arrays) = c(sf_column_name, names(a_attr))
  out = stars::st_as_stars(
    arrays,
    dimensions = d
  )

  # Return post_array object with respective structure
  structure(
    out,
    class = c("post_array", class(out)),
    sf_column = sf_column_name,
    time_column = time_column_name,
    geom_sum_fun = geometry_summary,
    agr = sf::st_agr(x)[names(a_attr)]
  )
}

# Compute the geometry summary as the union and dissolve of
# the changing geometries
# INTERNAL USE!
#' @importFrom sf st_union st_make_valid
compute_geom_summary_union = function(x, group_id, sf_column_name) {
  x_groupped = split(x[[sf_column_name]], x[[group_id]])
  x_summarised = do.call(
    "c",
    lapply(x_groupped, function(i) sf::st_make_valid(sf::st_union(i)))
  )
  x_summarised
}

# Compute the geometry summary as the centroid of the
# union and dissolve of the changing geometries
# INTERNAL USE!
#' @importFrom sf st_centroid
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
#' @importFrom sf st_as_sfc st_bbox
st_bbox_by_feature = function(x) {
  f = function(y) sf::st_as_sfc(sf::st_bbox(y))
  do.call("c", lapply(x, f))
}
