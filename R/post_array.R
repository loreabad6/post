#' Create a post_array object
#'
#' `post_array` is a spatio-temporal array data structure that
#' organises polygons that change their shape in space and time.
#' It extends the `stars` class for vector data cubes to support
#' changing geometries as attributes.
#' The `post_array` class supports two dimensions: `geom_sum` and
#' `datetime`.
#' `geom_sum` is the summary geometry of the changing
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
#' @param geometry_summary (function) function to compute the summary
#' geometry. Alternatively an `sfc` object with summary geometries (i.e.,
#' the result of a `summarise_geometry_*()` function or a custom `sfc`).
#' See summarise_geometry for functions or pass your own
#' summarise_geometry function.
#'
#' @param geometry_summary_name (character) name of the column with summary
#' geometries.Defaults to "geom_sum"
#'
#' @param ... additional parameters passed on to `geometry_summary` function.
#'
#' @examples
#' as_post_array(polygons)
#'
#' if(require(dplyr, quietly = TRUE)) {
#'   library(sf, quietly = TRUE)
#'   polygons |>
#'     mutate(area = st_area(geometry)) |>
#'     as_post_array(
#'      geometry_summary = summarise_geometry_union
#'    )
#' }
#'
#' @name as_post_array
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
                         geometry_summary = summarise_geometry_centroid,
                         geometry_summary_name = NULL,
                         ...) {
  UseMethod("as_post_array")
}

#' @rdname as_post_array
#'
#' @importFrom rlang `!!` sym
#' @importFrom sf st_agr st_drop_geometry st_geometry_type
#' @importFrom stars st_dimensions st_as_stars
#' @importFrom tidyr complete
#'
#' @export
as_post_array.sf = function(x,
                            group_id = NULL,
                            time_column_name = NULL,
                            sf_column_name = NULL,
                            geometry_summary = summarise_geometry_centroid,
                            geometry_summary_name = NULL,
                            ...) {

  # Set argument defaults
  # group_id: Defaults to the first column of x, if not sfc or temporal class
  group_id = check_group_id(x, group_id)
  # Defaults to the first temporal column.
  time_column_name = check_time_column(x, time_column_name)
  # Defaults to the active sf_column
  sf_column_name = check_sf_column(x, sf_column_name)
  # Defaults to "geom_sum"
  geometry_summary_name = geometry_summary_name %||% "geom_sum"

  # Complete missing cases
  x = sf::st_as_sf(
    tidyr::complete(x, !!rlang::sym(group_id), !!rlang::sym(time_column_name))
  )

  # Order x by time_column and group_id (to simulate byrow = TRUE in array)
  x = x[order(x[[time_column_name]], x[[group_id]]), ]

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

  # Compute geometry summary
  geom_sum = if(is.function(geometry_summary)) {
    geometry_summary(x, group_id, sf_column_name, ...)
  } else if (inherits(geometry_summary, "sfc")) {
    if(length(geometry_summary) != length(unique(x[[group_id]]))) {
      stop("geometry_summary has more geometries than groups", call. = FALSE)
    }
    geometry_summary
  } else {
    stop("geometry_summary not recognized", call. = FALSE)
  }

  # Create dimensions object
  d = stars::st_dimensions(
    geom_sum = geom_sum,
    temporal = unique(x[[time_column_name]]),
    # TODO: handle point parameter if more than two dimensions
    # The point parameter indicates if the value refers to
    # a point (location) or to a pixel (area) value
    point = c(TRUE, FALSE)
  )

  names(d) = c(geometry_summary_name, time_column_name)

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
    group_id_colname = group_id,
    group_ids = sort(unique(x[[group_id]])),
    sf_column = sf_column_name,
    agr = sf::st_agr(x)
  )
}

#' @rdname as_post_array
#'
#' @importFrom sf st_as_sfc st_as_sf
#'
#' @export
as_post_array.post_table = function(x, ...) {
  # Identify sf_column for summary geometries
  sf_column_sum = attr(face_spatial(x), "sf_column")

  # Coerce to sf using post_table method
  x_ = sf::st_as_sf(x)

  # Define the geometry summary as an sfc object of unique geometries
  sf_col = x_[[sf_column_sum]]
  geom_sum = sf_col[!st_duplicated(sf_col)]

  # Remove unnecessary columns (created by cubble)
  x_["long"] = x_["lat"] = x_[sf_column_sum] = NULL

  # Coerce with as_post_array.sf
  out = as_post_array(
    x = x_,
    geometry_summary = geom_sum,
    geometry_summary_name = sf_column_sum
  )

  out
}

#' @rdname as_post_array
#' @export
as_post_array.post_array = function(x, ...) {
  x
}

#' Utility functions for post_array objects
#' @name utils-post-array
#'
#' @rdname utils-post-array
#' @details
#' `get_group_ids()` gets the group_id values of the post_array object as a named data.frame
#' @param x a post_array object
#' @return a data.frame with the group_id values
#' @export
get_group_ids = function(x) {
  out = as.data.frame(attr(x, "group_ids"))
  names(out) = attr(x, "group_id_colname")
  out
}

# TODO: create accessor function to group id names and column name
