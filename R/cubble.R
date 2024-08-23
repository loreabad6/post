#' Face spatial/temporal method for post_table
#'
#' `face_spatial` pivots a `post_table` object from a long (temporal)
#' to a nested (spatial) form.
#'
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
#' @importFrom cubble face_temporal
#' @importFrom sf st_set_geometry
#' @export
face_temporal.post_table = function(data, col) {
	structure(
	  sf::st_set_geometry(
	    NextMethod(),
	    attr(data, "sf_column_post")
	  ),
	  class = union(c("post_table", "sf"), class(NextMethod())),
	  sf_column_post = attr(data, "sf_column_post"),
	  time_column = attr(data, "time_column"),
	  geom_sum_fun = attr(data, "geom_sum_fun")
	)
}

#' @rdname cubble-face
#'
#' @importFrom cubble face_spatial
#' @export
face_spatial.post_table = function(data, col) {
  structure(
    NextMethod(),
    class = union(c("post_table"), class(NextMethod()))
  )
}
