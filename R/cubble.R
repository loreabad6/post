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
  out = NextMethod()
  structure(out, class = union("post_table", class(out)))
}

#' @rdname cubble-face
#'
#' @importFrom cubble face_temporal
#' @importFrom sf st_set_geometry
#' @export
face_temporal.post_table = function(data, col) {
  cubble_classes = c("temporal_cubble_df", "cubble_df")
  out = st_as_sf(NextMethod())
  structure(
    out,
    class = c(
      "post_table",
      cubble_classes,
      setdiff(class(out), cubble_classes)
    )
  )
}

#' dplyr methods for post_table
#'
#' See `cubble::dplyr()` for details
#'
#' @name cubble-dplyr
#' @param data,.data a post_table object
#' @param ... see corresponding dplyr function
#'
#' @importFrom dplyr arrange
#' @export
arrange.post_table = function(.data, ...) {
  out = NextMethod()
  structure(out, class = union("post_table", class(out)))
}
#' @rdname cubble-dplyr
#' @importFrom dplyr filter
#' @export
filter.post_table = function(.data, ...) {
  out = NextMethod()
  structure(out, class = union("post_table", class(out)))
}
#' @importFrom dplyr group_by
#' @export
group_by.post_table = function(.data, ...) {
  out = NextMethod()
  structure(out, class = union("post_table", class(out)))
}
#' @importFrom dplyr mutate
#' @export
mutate.post_table = function(.data, ...) {
  out = NextMethod()
  structure(out, class = union("post_table", class(out)))
}
#' @importFrom dplyr rename
#' @export
rename.post_table = function(.data, ...) {
  # TODO: rename should update the sf_column_post attribute
  out = NextMethod()
  structure(out, class = union("post_table", class(out)))
}
#' @importFrom dplyr rowwise
#' @export
rowwise.post_table = function(data, ...) {
  out = NextMethod()
  structure(out, class = union("post_table", class(out)))
}
#' @importFrom dplyr select
#' @export
select.post_table = function(.data, ...) {
  out = NextMethod()
  structure(out, class = union("post_table", class(out)))
}
#' @importFrom dplyr ungroup
#' @export
ungroup.post_table = function(x, ...) {
  out = NextMethod()
  structure(out, class = union("post_table", class(out)))
}
