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
face_spatial.post_table = function(data, col) {
  rebuild_post_table_attr(NextMethod(), data)
}

#' @rdname cubble-face
#'
#' @importFrom cubble face_temporal
#' @importFrom sf st_set_geometry
#' @export
face_temporal.post_table = function(data, col) {
  rebuild_post_table_attr(
    sf::st_set_geometry(
      NextMethod(),
      attr(data, "sf_column_post")
    ),
    data,
    classes = c("post_table", "sf")
  )
}

#' dplyr methods for post_table
#'
#' See `cubble::dplyr()` for details
#' @name cubble-dplyr
#'
#' @importFrom dplyr arrange
#' @export
arrange.post_table = function(.data, ...) {
  rebuild_post_table_attr(NextMethod(), .data)
}
#' @rdname cubble-dplyr
#' @importFrom dplyr filter
#' @export
filter.post_table = function(.data, ...) {
  rebuild_post_table_attr(NextMethod(), .data)
}
#' @importFrom dplyr group_by
#' @export
group_by.post_table = function(.data, ...) {
  rebuild_post_table_attr(NextMethod(), .data)
}
#' @importFrom dplyr rename
#' @export
rename.post_table = function(.data, ...) {
  # TODO: rename should update the sf_column_post attribute
  rebuild_post_table_attr(NextMethod(), .data)
}
#' @importFrom dplyr rowwise
#' @export
rowwise.post_table = function(.data, ...) {
  rebuild_post_table_attr(NextMethod(), .data)
}
#' @importFrom dplyr select
#' @export
select.post_table = function(.data, ...) {
  rebuild_post_table_attr(NextMethod(), .data)
}
#' @importFrom dplyr summarise
#' @export
summarise.post_table = function(.data, ...) {
  rebuild_post_table_attr(NextMethod(), .data)
}
#' @importFrom dplyr ungroup
#' @export
ungroup.post_table = function(.data, ...) {
  rebuild_post_table_attr(NextMethod(), .data)
}

#' @param x_new object after applying cubble/dplyr function
#' @param x_orig object before applying cubble/dplyr function
#' @param classes class to assign to the object
#' @noRd
rebuild_post_table_attr = function(x_new, x_orig,
                                   classes = "post_table") {
  structure(
    x_new,
    class = union(classes, class(x_new)),
    sf_column_post = attr(x_orig, "sf_column_post"),
    time_column = attr(x_orig, "time_column"),
    geom_sum_fun = attr(x_orig, "geom_sum_fun")
  )
}
