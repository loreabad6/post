#' Create a post_table object
#'
#' `post_table` is a spatio-temporal tabular data structure that
#' organises polygons that change their shape in space and time.
#' It extends the `cubble_df` classes for vector data cubes to support
#' changing geometries in the temporal face of the cube.
#' The `post_table` geometry for the spatial face of the cube is
#' `geom_sum`, while the changing geometry provided is passed
#' to the temporal face of the cube.
#'
#' @inheritParams as_post_array
#'
#' @param x object to convert to `post_table` with `POLYGON`/`MULTIPOLYGON`
#' geometries and a date/time column.
#'
#' @examples
#' as_post_table(polygons)
#'
#' if(require(dplyr, quietly = TRUE)) {
#'   library(sf, quietly = TRUE)
#'   polygons |>
#'     mutate(area = st_area(geometry)) |>
#'     as_post_table(
#'      geometry_summary = summarise_geometry_union
#'    )
#' }
#'
#' @return an object of class `post_table`.
#'
#' @references
#' Abad, L., Sudmanns, M., and Hölbling, D. (2024)
#' Vector data cubes for features evolving in space and time,
#' *AGILE GIScience Ser.*, 5, 16,
#' <https://doi.org/10.5194/agile-giss-5-16-2024>
#'
#' @export
as_post_table = function(x,
                         group_id = NULL,
                         time_column_name = NULL,
                         sf_column_name = NULL,
                         geometry_summary = summarise_geometry_centroid,
                         ...) {
  UseMethod("as_post_table")
}

#' @importFrom cubble as_cubble
#' @importFrom sf st_sf st_as_sf
#'
#' @export
as_post_table.sf = function(x,
                            group_id = NULL,
                            time_column_name = NULL,
                            sf_column_name = NULL,
                            geometry_summary = summarise_geometry_centroid,
                            ...) {
  # Set argument defaults
  # group_id: Defaults to the first column of x, if not sfc or temporal class
  group_id = check_group_id(x, group_id)
  # Defaults to the first temporal column.
  time_column_name = check_time_column(x, time_column_name)
  # Defaults to the active sf_column
  sf_column_name = check_sf_column(x, sf_column_name)

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

  # Combine summary geometry as a data frame with the original x object
  geom_sum_df = sf::st_sf(gid = unique(x[[group_id]]), geom_sum = geom_sum)

  x_ = sf::st_as_sf(
    merge(
      as.data.frame(x),
      as.data.frame(geom_sum_df)
    ), sf_column_name = "geom_sum"
  )

  out = cubble::as_cubble(
    data = x_,
    key = {{group_id}},
    index = {{time_column_name}}
  )

  # Return post_array object with respective structure
  structure(
    out,
    class = c("post_table", class(out)),
    # TODO: figure out how to hable sf_column among post_table and post_array
    # sf_column = sf_column_name,
    time_column = time_column_name,
    geom_sum_fun = geometry_summary
    # TODO: handle agr in post_table
    # agr = sf::st_agr(x)[names(a_attr)]
  )
}
