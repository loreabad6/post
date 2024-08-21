#' Face temporal method for post_table
#'
#' `face_temporal()` pivots a `post_table` object from a nested (spatial)
#' to a long (temporal) form. 
#' The method for `post_table` objects activates the changing geometry 
#' in the long form.
#'
#' @param data a post_table object
#' @param col see the documentation of cubble::face_temporal for details.
#'
#' @name cubble
#' @return a post_table object
#'
#' importFrom cubble face_temporal
#' @export
face_temporal.post_table = function(data, col) {
	out = cubble::face_temporal(data, col)
	st_geometry(out) = attr(data, "sf_column_post")
	out
}