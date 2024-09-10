#' Remove or restore post_* classes
#'
#' Utility functions to remove/restore post_table/post_array classes when
#' passing functions onto stars, spatial_cubble_df and temporal_cubble_df
#' @name classes
#' @return An object that inherits or removes the classes post_table/post_array
#' and sf.
#'
#' @rdname classes
#' @param x An object that inherits post_table class.
#' @export
remove_post_table = function(x) {
  class(x) = setdiff(class(x), "post_table")
  x
}
#' @rdname classes
#' @param x An object that inherits cubble class.
#' @export
restore_post_table = function(x) {
  structure(x, class = union("post_table", class(x)))
}
#' @rdname classes
#' @param x An object that inherits spatial_cubble_df class.
#' @export
restore_spatial_post_table = function(x) {
  if(class(x)[[1]] == "sf") {
    cubble_spat_classes = c("spatial_cubble_df", "cubble_df")
    structure(
      x,
      class = c(
        "post_table",
        cubble_spat_classes,
        setdiff(class(x), cubble_spat_classes)
      )
    )
  } else {
    structure(x, class = union("post_table", class(x)))
  }
}
#' @rdname classes
#' @param x An object that inherits temporal_cubble_df class.
#' @export
restore_temporal_post_table = function(x) {
  cubble_temp_classes = c("temporal_cubble_df", "cubble_df")
  structure(
    x,
    class = c(
      "post_table",
      cubble_temp_classes,
      setdiff(class(x), cubble_temp_classes)
    )
  )
}
#' @rdname classes
#' @param x An object that inherits stars class.
#' @export
remove_post_array = function(x) {
  class(x) = setdiff(class(x), "post_array")
  x
}
#' @rdname classes
#' @param x An object that inherits stars class.
#' @export
restore_post_array = function(x, x_orig) {
  # Extract sf_column name, default to first sfc attribute in stars object
  sf_column = names(which(sapply(x, \(i) inherits(i, "sfc"))))[1]
  # Identify spatial dimension
  sf_dim = names(
    which(
      sapply(st_dimensions(x_orig), \(i) inherits(i$value, "sfc"))
  ))[1]
  # Extract to and from indices for spatial dimension
  from = stars::st_dimensions(x)[[sf_dim]]$from
  to = stars::st_dimensions(x)[[sf_dim]]$to

  structure(
    x,
    class = union("post_array", class(x)),
    group_id_colname = attr(x_orig, "group_id_colname"),
    group_ids = attr(x_orig, "group_ids")[from:to],
    sf_column = sf_column,
    agr =  sf::st_agr(x)
  )
}
