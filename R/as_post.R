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
#' @param geometry_summary (function) function to compute the summary
#' geometry. Alternatively an `sfc` object with summary geometries (i.e.,
#' the result of a `summarise_geometry_*()` function or a custom `sfc`).
#' See summarise_geometry for functions or pass your own
#' summarise_geometry function.
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
                         geometry_summary = summarise_geometry_centroid) {
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
                            geometry_summary = summarise_geometry_centroid) {

  # Set argument defaults
  # group_id: Defaults to the first column of x, if not sfc or temporal class
  group_id = check_group_id(x, group_id)
  # Defaults to the first temporal column.
  time_column_name = check_time_column(x, time_column_name)
  # Defaults to the active sf_column
  sf_column_name = check_sf_column(x, sf_column_name)

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
    geometry_summary(x, group_id, sf_column_name)
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
