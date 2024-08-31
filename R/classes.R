#' @description
#' Utility functions to remove/restore post_table classes when passing
#' functions onto spatial_cubble_df and temporal_cubble_df
#'
#' @return An object that inherits or removes the classes post_table and sf.
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
