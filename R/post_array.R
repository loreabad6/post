#' Create a post_array object
#'
#' `post_array` is a spatio-temporal array data structure that
#' organises polygons that change their shape in space and time.
#' It extends the `stars` class for vector data cubes to support
#' changing geometries as attributes.
#' The `post_array` class supports two dimensions: a summary geometry
#' dimension (default name: `geom_sum`) and a temporal dimension.
#' The summary geometry is a unique geometry per group
#' which summarises the changing shapes of the polygon geometries in the group.
#'
#' @param x object to convert to `post_array` with `POLYGON`/`MULTIPOLYGON`
#' geometries and a date/time column.
#' @param group_id See Details.
#' Defaults to the first non-spatial, non-temporal column in `x`.
#' @param time_column_name (character) name of column with the temporal
#' dimension information. Defaults to the first temporal column in `x`.
#' @param sf_column_name (character) name of column with changing
#' geometries. Defaults to active `sf_column`.
#' @param geometry_summary (function) function to compute the summary
#' geometry. Alternatively an `sfc` object with summary geometries (i.e.,
#' the result of a `summarise_geometry_*()` function or a custom `sfc`).
#' See summarise_geometry for functions or pass your own
#' summarise_geometry function.
#' @param geometry_summary_name (character) name of the column with summary
#' geometries.Defaults to "geom_sum"
#' @param ... additional parameters passed on to `geometry_summary` function.
#'
#' @details
#' group_id should be the name of the column containing the grouping
#' identifier for the changing polygons or a vector with group identifiers.
#' Character, integer, double and factor vectors are supported.
#' The vector should be of length 1 or the same as `nrow(x)`.
#' A vector of length 1 repeats the value for all the rows.
#' Providing a vector assumes that the ordering of the groups
#' is correct and that there are no duplicated timestamps per group
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
#' @param point_st point argument passed onto `stars::st_dimensions()`.
#' Defaults to `TRUE` for the spatial dimension and to `FALSE` for the
#' temporal dimension (assumes interval times)
#' @importFrom rlang `!!` `%||%` sym
#' @importFrom sf st_agr
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
                            ...,
                            point_st = c(TRUE, FALSE)) {

  # Set argument defaults
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
  geom_sum = check_geometry_summary(x, geometry_summary,
                                    group_id, sf_column_name, ...)

  # Create dimensions object
  d = stars::st_dimensions(
    geom_sum = geom_sum,
    temporal = unique(x[[time_column_name]]),
    point = point_st
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
#' @importFrom sf st_as_sfc st_as_sf
#' @export
as_post_array.post_table = function(x, ...) {
  # Identify sf_column for summary geometries
  sf_column_sum = attr(
    suppressMessages(face_spatial(x), "cliMessage"),
    "sf_column"
  )

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
#' `get_group_ids()` gets the group_id values of the post_array object as a
#' named data.frame
#' @param x a post_array object
#' @return a data.frame with the group_id values
#' @export
get_group_ids = function(x) {
  out = as.data.frame(attr(x, "group_ids"))
  names(out) = attr(x, "group_id_colname")
  out
}
