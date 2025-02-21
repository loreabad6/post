#' sf methods for post_table objects
#'
#' @param x a post_table object
#' @param ... additional arguments passed onto the respective sf function
#' @name sf-post-table

#' @rdname sf-post-table
#' @details
#' `sf::st_as_sf()` for post_table objects sets the changing geometry as the
#' active sf_column but keeps the summarised geometry in a separate column.
#'
#' @importFrom sf `st_geometry<-`
#' @importFrom tidyr unnest
#' @export
st_as_sf.post_table = function(x, ...) {
  # Check if post_table is in spatial form
  if("temporal_cubble_df" %in% class(x)) {
    x = face_spatial(x)
  }

  # First coerce to sf by unnesting the ts column
  out = tidyr::unnest(x, cols = "ts")

  # Identify the sf_column for the changing geometries
  sf_column_ts = lapply(x$ts, attr, "sf_column")[[1]]

  # Reassign geometry column
  st_geometry(out) = sf_column_ts
  out
}
#' @rdname sf-post-table
#' @importFrom sf `st_crs<-`
#' @inheritParams sf::`st_crs<-`
#' @export
`st_crs<-.post_table` = function(x, value) {
  change_geom(x, `st_crs<-`, value)
}
#' @rdname sf-post-table
#' @details
#' `st_normalize()` takes as domain the bounding box of the post_table
#' in the nested form (spatial face)
#' @importFrom sf st_normalize
#' @inheritParams sf::st_normalize
#' @export
st_normalize.post_table = function(
    x,
    domain = st_bbox(suppressMessages(face_spatial(x), "cliMessage")),
    ...) {
  change_geom(x, st_normalize, domain = domain, ...)
}
# TODO: issue on cubble, precision attribute is not set when
# called on cubble object
# #' @rdname sf-post-table
# #' @importFrom sf `st_precision<-`
# #' @export
# `st_precision<-.post_table` = function() {
#
# }
# #' @rdname sf-post-table
# #' @importFrom sf st_set_precision
# #' @export
# st_set_precision.post_table = function() {
#
# }
#' @rdname sf-post-table
#' @importFrom sf st_shift_longitude
#' @inheritParams sf::st_shift_longitude
#' @export
st_shift_longitude.post_table = function(x, ...) {
  change_geom(x, st_shift_longitude, ...)
}
#' @rdname sf-post-table
#' @importFrom sf st_transform
#' @inheritParams sf::st_transform
#' @export
st_transform.post_table = function(x, crs, ...) {
  change_geom(x, st_transform, crs, ...)
}
#' @rdname sf-post-table
#' @importFrom sf st_wrap_dateline
#' @inheritParams sf::st_wrap_dateline
#' @export
st_wrap_dateline.post_table = function(x, ...) {
  change_geom(x, st_wrap_dateline, ...)
}
#' @rdname sf-post-table
#' @importFrom sf st_zm
#' @inheritParams sf::st_zm
#' @export
st_zm.post_table = function(x, ...) {
  change_geom(x, st_zm, ...)
}

#' @noRd
change_geom = function(x, op, ...) {
 if (inherits(x, "temporal_cubble_df")) {
   x = remove_post_table(x)
   x = op(x, ...)
   x = restore_temporal_post_table(x)
   x = face_spatial(x)
   x = remove_post_table(x)
   x = op(x, ...)
   x = restore_spatial_post_table(x)
   x = face_temporal(x)
   x
 } else if (inherits(x, "spatial_cubble_df")) {
   x = remove_post_table(x)
   x = op(x, ...)
   x = restore_spatial_post_table(x)
   x = face_temporal(x)
   x = remove_post_table(x)
   x = op(x, ...)
   x = restore_temporal_post_table(x)
   x = face_spatial(x)
   x$ts  = lapply(x$ts, st_as_sf)
   x = restore_spatial_post_table(x)
   x
 }
}

#' sf methods for post_array objects
#'
#' @param x a post_array object
#' @param ... other arguments passed on to `sf::st_as_sf()`
#' @name sf-post-array
#' @details
#' `sf::st_as_sf()` for post_array objects sets the changing geometry as the
#' active sf_column but keeps the summarised geometry in a separate column.
#' @importFrom dplyr as_tibble last_col relocate
#' @importFrom rlang `!!` sym
#' @importFrom sf st_as_sf
#' @export
st_as_sf.post_array = function(x, ...) {
  # Get the spatial dimension name
  sf_dim = names(
    which(
      sapply(st_dimensions(x), \(i) inherits(i$value, "sfc"))
    ))[1]
  # Fetch the group_id column name from attributes
  group_id_colname = attr(x, "group_id_colname")
  # Convert to sf by first parsing to tibble
  out = sf::st_as_sf(
    dplyr::as_tibble(x),
    sf_column_name = attr(x, "sf_column"),
    ...
  )
  # Regenerate group_id column
  out[group_id_colname] = rep(
    attr(x, "group_ids"),
    times = dim(x)[[sf_dim]],
    length.out = prod(dim(x))
  )
  # Reorganise sf columns
  out = dplyr::relocate(out, !!rlang::sym(group_id_colname))
  out = dplyr::relocate(
    out,
    names(st_dimensions(x)), attr(x, "sf_column"),
    .after = dplyr::last_col()
  )
  # Reorder the rows
  out[order(out[[group_id_colname]]), ]
}

#' sf geometry predicates for post_array objects
#' @param x a post_array object
#' @param y an object of class `sf`, `sfc`, `sfg`
#' @param .predicate binary predicate to use (see `?sf::geos_binary_pred`), defaults to `st_intersects`
#' @param ... passed on to the geometry predicate
#' @name binary-predicates-post-array
#' @importFrom sf st_intersects
#' @return a one column matrix as the result of any features in y relating to
#' any feature in x
#' @examples
#' library(sf)
#' arr = as_post_array(polygons, geometry_summary = summarise_geometry_union)
#' circles = list(st_point(c(0.55,0.5)), st_point(c(0.75,0.4))) |>
#'   st_sfc(crs = 4326) |>
#'   st_buffer(10000)
#' st_mask(arr, circles)
#' arr$circles_disjoint = st_mask(arr, circles, .predicate = st_disjoint)
#' @export
st_mask = function(x, y, .predicate = st_intersects, ...) {
  UseMethod("st_mask")
}

#' @export
st_mask.post_array = function(x, y, .predicate = st_intersects, ...) {
  NextMethod()
}

#' @export
st_mask.stars = function(x, y, .predicate = st_intersects, ...) {
  sfcol = names(which(sapply(x, inherits, "sfc")))
  if (length(sfcol > 1)) {
    sfcol = sfcol[[1]]
    cli::cli_warn(c(
      "i" = "more than one {.cls sfc} attribute in {.var x}",
      "!" = "applying {.fn st_mask} to first {.cls sfc} attribute"
    ))
  }
  mat = .predicate(x[[sfcol]], y, sparse = FALSE, ...)
  matrix(apply(mat, 1, any), ncol = 1)
}
