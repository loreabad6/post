#' @description
#' Utility functions to remove/restore post_table/post_array classes when
#' passing functions onto stars, spatial_cubble_df and temporal_cubble_df
#'
#' @return An object that inherits or removes the classes post_table/post_array
#' and sf.
#'
#' @param x An object that inherits post_table class.
#' @noRd
remove_post_table = function(x) {
  class(x) = setdiff(class(x), "post_table")
  x
}
#' @param x An object that inherits cubble class.
#' @noRd
restore_post_table = function(x) {
  structure(x, class = union("post_table", class(x)))
}
#' @param x An object that inherits spatial_cubble_df class.
#' @noRd
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

#' @param x An object that inherits temporal_cubble_df class.
#' @noRd
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
#' @param x An object that inherits stars class.
#' @noRd
remove_post_array = function(x) {
  class(x) = setdiff(class(x), "post_array")
  x
}
#' @param x An object that inherits stars class.
#' @noRd
restore_post_array = function(x, x_orig) {
  # Extract sf_column name, default to first sfc attribute in stars object
  sf_column = names(lapply(x, \(i) inherits(i, "sfc")))[[1]]
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
    time_column = attr(x_orig, "time_column"),
    agr =  sf::st_agr(x)
  )
}
