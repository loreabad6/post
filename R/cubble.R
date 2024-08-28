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
  cubble_temp_classes = c("temporal_cubble_df", "cubble_df")
  out = st_as_sf(NextMethod())
  structure(
    out,
    class = c(
      "post_table",
      cubble_temp_classes,
      setdiff(class(out), cubble_temp_classes)
    )
  )
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
  class(data) = setdiff(class(data), "post_table")
  cubble::spatial(data)
}

#' dplyr methods for post_table
#'
#' See `cubble::dplyr()` for details
#'
#' @name cubble-dplyr
#' @param data,.data a post_table object
#' @param ... see corresponding dplyr function
#' @inheritParams dplyr::dplyr_reconstruct
#'
#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.post_table = function(data, template) {
  class(data) = setdiff(class(data), "post_table")
  NextMethod()
}

#' @importFrom dplyr arrange
#' @export
arrange.post_table = function(.data, ...) {
  if(inherits(.data, "temporal_cubble_df")) {
    cubble_temp_classes = c("temporal_cubble_df", "cubble_df")
    out = st_as_sf(NextMethod())
    structure(
      out,
      class = c(
        "post_table",
        cubble_temp_classes,
        setdiff(class(out), cubble_temp_classes)
      )
    )
  } else if (inherits(.data, "spatial_cubble_df")) {
    out = NextMethod()
    structure(out, class = union("post_table", class(out)))
  }
}
#' @rdname cubble-dplyr
#' @importFrom dplyr filter
#' @export
filter.post_table = function(.data, ...) {
  if(inherits(.data, "temporal_cubble_df")) {
    cubble_temp_classes = c("temporal_cubble_df", "cubble_df")
    out = st_as_sf(NextMethod())
    structure(
      out,
      class = c(
        "post_table",
        cubble_temp_classes,
        setdiff(class(out), cubble_temp_classes)
      )
    )
  } else if (inherits(.data, "spatial_cubble_df")) {
    out = NextMethod()
    structure(out, class = union("post_table", class(out)))
  }
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
  if(inherits(.data, "temporal_cubble_df")) {
    cubble_temp_classes = c("temporal_cubble_df", "cubble_df")
    out = st_as_sf(NextMethod())
    structure(
      out,
      class = c(
        "post_table",
        cubble_temp_classes,
        setdiff(class(out), cubble_temp_classes)
      )
    )
  } else if (inherits(.data, "spatial_cubble_df")) {
    out = NextMethod()
    structure(out, class = union("post_table", class(out)))
  }
}
#' @importFrom dplyr rename
#' @export
rename.post_table = function(.data, ...) {
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
  class(.data) = setdiff(class(.data), "post_table")
  out = NextMethod()
  if(inherits(.data, "temporal_cubble_df")) {
    cubble_temp_classes = c("temporal_cubble_df", "cubble_df")
    st_as_sf(out)
    structure(
      out,
      class = c(
        "post_table",
        cubble_temp_classes,
        setdiff(class(out), cubble_temp_classes)
      )
    )
  } else if (inherits(.data, "spatial_cubble_df")) {
    structure(out, class = union("post_table", class(out)))
  }
}
#' @importFrom dplyr ungroup
#' @export
ungroup.post_table = function(x, ...) {
  out = NextMethod()
  structure(out, class = union("post_table", class(out)))
}
