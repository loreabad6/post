#' dplyr methods for post_table
#' @name dplyr-post-table
#'
#' @details
#' See `??cubble::dplyr` for details
#' @param data,.data,x a post_table object
#' @param ... see corresponding function in package dplyr
#'
#' @rdname dplyr-post-table
#' @inheritParams dplyr::dplyr_reconstruct
#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.post_table = function(data, template) {
  remove_post_table(data)
  NextMethod()
}

#' @rdname dplyr-post-table
#' @importFrom dplyr arrange
#' @export
arrange.post_table = function(.data, ...) {
  if(inherits(.data, "temporal_cubble_df")) {
    out = st_as_sf(NextMethod())
    restore_temporal_post_table(out)
  } else if (inherits(.data, "spatial_cubble_df")) {
    out = NextMethod()
    restore_spatial_post_table(out)
  }
}
#' @rdname dplyr-post-table
#' @importFrom dplyr filter
#' @export
filter.post_table = function(.data, ...) {
  if(inherits(.data, "temporal_cubble_df")) {
    out = st_as_sf(NextMethod())
    restore_temporal_post_table(out)
  } else if (inherits(.data, "spatial_cubble_df")) {
    out = NextMethod()
    restore_spatial_post_table(out)
  }
}
#' @rdname dplyr-post-table
#' @importFrom dplyr group_by
#' @export
group_by.post_table = function(.data, ...) {
  out = NextMethod()
  restore_post_table(out)
}
#' @rdname dplyr-post-table
#' @importFrom dplyr mutate
#' @export
mutate.post_table = function(.data, ...) {
  if(inherits(.data, "temporal_cubble_df")) {
    out = st_as_sf(NextMethod())
    restore_temporal_post_table(out)
  } else if (inherits(.data, "spatial_cubble_df")) {
    out = NextMethod()
    restore_spatial_post_table(out)
  }
}
#' @rdname dplyr-post-table
#' @importFrom dplyr rename
#' @export
rename.post_table = function(.data, ...) {
  .data = remove_post_table(.data)
  out = NextMethod()
  restore_post_table(out)
}
#' @rdname dplyr-post-table
#' @importFrom dplyr rowwise
#' @export
rowwise.post_table = function(data, ...) {
  out = NextMethod()
  restore_post_table(out)
}
#' @rdname dplyr-post-table
#' @importFrom dplyr select
#' @export
select.post_table = function(.data, ...) {
  .data = remove_post_table(.data)
  out = NextMethod()
  if(inherits(.data, "temporal_cubble_df")) {
    out = st_as_sf(out)
    restore_temporal_post_table(out)
  } else if (inherits(.data, "spatial_cubble_df")) {
    restore_spatial_post_table(out)
  }
}
#' @rdname dplyr-post-table
#' @importFrom dplyr ungroup
#' @export
ungroup.post_table = function(x, ...) {
  out = NextMethod()
  restore_post_table(out)
}

#' @details
#' See `?sf::tidyverse` for details
#' @rdname dplyr-post-table
#' @inheritParams sf::tidyverse
#' @importFrom dplyr slice
#' @export
slice.post_table = function(.data, ..., .dots) {
  if(inherits(.data, "temporal_cubble_df")) {
    out = NextMethod()
    restore_temporal_post_table(out)
  } else if (inherits(.data, "spatial_cubble_df")) {
    out = NextMethod()
    restore_spatial_post_table(out)
  }
}


#' dplyr methods for post_array
#' @name dplyr-post-array
#'
#' @details
#' See `?stars::dplyr` for details
#' @param data,.data,x a post_array object
#' @param ... see corresponding function in package dplyr
#'
#' @rdname dplyr-post-array
#' @importFrom dplyr filter
#' @export
filter.post_array = function(.data, ...) {
  out = NextMethod()
  restore_post_array(out, .data)
}
#' @rdname dplyr-post-array
#' @importFrom dplyr mutate
#' @export
mutate.post_array = function(.data, ...) {
  out = NextMethod()
  restore_post_array(out, .data)
}
#' @rdname dplyr-post-array
#' @importFrom dplyr rename
#' @export
rename.post_array = function(data, ...) {
  out = NextMethod()
  restore_post_array(out, data)
}
#' @rdname dplyr-post-array
#' @importFrom dplyr select
#' @export
select.post_array = function(.data, ...) {
  out = NextMethod()
  restore_post_array(out, .data)
}
#' @rdname dplyr-post-array
#' @importFrom dplyr slice
#' @export
slice.post_array = function(.data, ..., drop = FALSE) {
  out = NextMethod()
  restore_post_array(out, .data)
}
#' @rdname dplyr-post-array
#' @importFrom dplyr transmute
#' @export
transmute.post_array = function(.data, ...) {
  out = NextMethod()
  restore_post_array(out, .data)
}
