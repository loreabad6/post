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
st_as_sf.post_table = function(x) {
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
#' @importFrom sf st_set_crs
#' @inheritParams sf::st_set_crs
#' @export
st_set_crs.post_table = function(x, value) {
  change_geom(x, st_set_crs, value)
}
#' @rdname sf-post-table
#' @details
#' st_normalize takes as domain the bounding box of the post_table
#' in the nested form (spatial face)
#' @importFrom sf st_normalize
#' @inheritParams sf::st_normalize
#' @export
st_normalize.post_table = function(x, domain = st_bbox(face_spatial(x)), ...) {
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
#' @name sf-post-array
#' @importFrom dplyr as_tibble last_col relocate
#' @importFrom rlang `!!` sym
#' @importFrom sf st_as_sf
#' @export
st_as_sf.post_array = function(x) {
  sf_dim = names(
    which(
      sapply(st_dimensions(x_orig), \(i) inherits(i$value, "sfc"))
    ))[1]
  group_id_colname = attr(x, "group_id_colname")
  out = sf::st_as_sf(dplyr::as_tibble(x), sf_column_name = attr(x, "sf_column"))
  out[group_id_colname] = rep(
    attr(x, "group_ids"),
    times = dim(x)[[sf_dim]]
  )
  out = dplyr::relocate(out, !!rlang::sym(group_id_colname))
  dplyr::relocate(
    out,
    names(st_dimensions(x)), attr(x, "sf_column"),
    .after = dplyr::last_col()
  )
}
