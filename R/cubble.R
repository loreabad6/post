#' Face spatial/temporal method for post_table
#'
#' `face_spatial` pivots a `post_table` object from a long (temporal)
#' to a nested (spatial) form.
#' `face_temporal()` pivots a `post_table` object from a nested (spatial)
#' to a long (temporal) form.
#' For `post_table` objects, it activates the changing geometry
#' in the long form.
#'
#' @param data a post_table object
#' @param col see `cubble::face_temporal()` for details.
#'
#' @name cubble-face
#' @return a post_table object
#'
#' @importFrom cubble face_spatial
#' @export
face_spatial.post_table = function(data) {
  data = remove_post_table(data)
  out = NextMethod()
  restore_spatial_post_table(out)
}

#' @rdname cubble-face
#'
#' @importFrom cubble face_temporal
#' @importFrom sf st_set_geometry
#' @export
face_temporal.post_table = function(data, col) {
  data = remove_post_table(data)
  out = st_as_sf(NextMethod())
  restore_temporal_post_table(out)
}

#' Extract cubble attributes
#'
#' @name cubble-attrs
#'
#' @importFrom cubble spatial
#' @param data a post_table object
#' @return a post_table object
#'
#' @export
spatial.post_table = function(data) {
  data = remove_post_table(data)
  cubble::spatial(data)
}

# TODO: issue on cubble: handling empty cubble objects
