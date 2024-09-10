#' Create a post_table object
#'
#' `post_table` is a spatio-temporal tabular data structure that
#' organises polygons that change their shape in space and time.
#' It extends the `cubble_df` classes for vector data cubes to support
#' changing geometries in the temporal face of the cube.
#' The `post_table` geometry for the spatial face of the cube defaults
#' to a column named `geom_sum`, while the changing geometry provided is
#' passed on to the temporal face of the cube.
#'
#' @inheritParams as_post_array
#' @inherit as_post_array details
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
#' @name as_post_table
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

#' @rdname as_post_table
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

  # Order x by group_id and time_column
  x = x[order(x[[group_id]], x[[time_column_name]]), ]

  # Compute geometry summary
  geom_sum = check_geometry_summary(x, geometry_summary,
                                    group_id, sf_column_name, ...)

  # Stop if only one timestamp is present
  if(length(unique(x[[time_column_name]])) <= 1) {
    cli::cli_abort(c(
      "{.var x} has only one time value per group",
      "{.cls post_table} requires at least two time values"
    ))
  }

  # Combine summary geometry as a data frame with the original x object
  geom_sum_df = sf::st_sf(
    # sort unique IDs since the result of geom_sum is sorted
    gid = sort(unique(x[[group_id]])),
    geom_sum = geom_sum
  )

  x_ = sf::st_as_sf(
    merge(
      as.data.frame(x),
      as.data.frame(geom_sum_df),
      by.x = group_id,
      by.y = "gid"
    ), sf_column_name = "geom_sum"
  )

  out = cubble::as_cubble(
    data = x_,
    key = {{group_id}},
    index = {{time_column_name}}
  )

  # Coerce ts values to changing geometries to sf class
  out$ts  = lapply(out$ts, st_as_sf)

  # Return post_array object with respective structure
  structure(
    out,
    class = c("post_table", class(out))
  )
}

#' @rdname as_post_table
#' @param drop_empty (logical) should empty geometries be dropped
#' during post_table creation? Defaults to TRUE
#' @importFrom cubble as_cubble face_spatial face_temporal update_cubble
#' @importFrom dplyr filter relocate
#' @importFrom rlang `!!` sym
#' @importFrom sf st_is_empty
#' @export
as_post_table.post_array = function(x, ..., drop_empty = TRUE) {
  # Get time_column by checking dimension refsys
  refsys_time = c("POSIXct", "POSIXt", "Date", "PCICt")
  time_dim = names(
    which(
      sapply(st_dimensions(x), \(i) any(i$refsys %in% refsys_time))
    ))[1]
  # Stop if only one timestamp is present
  if(dim(x)[[time_dim]] <= 1) {
    cli::cli_abort(c(
      "{.var x} has only one time value per group",
      "{.cls post_table} requires at least two time values"
    ))
  }
  # Coerce to cubble
  x_ = cubble::as_cubble(
    data = remove_post_array(x),
    # key = geom_sum should be used here but
    # it works when creating the cubble object while the algorithm
    # to display the temporal face gets stuck as it tries to add a
    # WKT geometry to every entry.
    # The hack used when coercing from a stars object:
    # Internally, cubble adds an id column to the data, so we can
    # call key = id and the cube creation in its spatial and temporal
    # forms will work.
    key = "id",
    index = !!rlang::sym(time_dim)
  )
  # Drop empty geometries
  if(drop_empty) {
    x_ = cubble::face_spatial(
      dplyr::filter(
        cubble::face_temporal(x_),
        !sf::st_is_empty(!!rlang::sym(attr(x, "sf_column")))
      )
    )
  }
  # Restore original group_id column name and values when coercing to
  # post_table
  # All those values are stored as attributes of post_array
  group_id_name = attr(x, "group_id_colname")
  x_[group_id_name] = attr(x, "group_ids")
  # remove id column
  x_["id"] = NULL
  # reorder columns to have group_id column as first
  x_ = dplyr::relocate(x_, {{group_id_name}})
  # the cubble classes are lost, reinstate
  class(x_) = c("spatial_cubble_df","cubble_df", class(x_))

  out = cubble::update_cubble(x_, key = group_id_name)

  # Coerce ts values changing geometries to sf class
  out$ts  = lapply(out$ts, st_as_sf)

  # Return post_array object with respective structure
  structure(
    out,
    class = c("post_table", class(out)),
    agr = attr(x, "agr")
  )
}

#' @rdname as_post_table
#' @export
as_post_table.post_table = function(x, ...) {
  x
}
