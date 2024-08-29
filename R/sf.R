#' sf methods for post_table objects
#'
#' @param x a post_table object
#' @param ... additional arguments passed onto the respective sf function
#' @name sf-post_table

#' @rdname sf-post_table
#' @details
#' `sf::st_as_sf()` for post_table objects sets the changing geometry as the
#' active sf_column but keeps the summarised geometry in a separate column.
#'
#' @importFrom sf `st_geometry<-`
#' @importFrom tidyr unnest
#' @export
st_as_sf.post_table = function(x, ...) {
  # Check if post_table is in spatial form
  if("temporal_cubble_df" %in% class(x)) {
    x = face_spatial(x)
  }

  # First coerce to sf by unnesting the ts column
  out = tidyr::unnest(x, cols = "ts")

  # Identify the sf_column for the changing geometries
  sf_column_ts = lapply(x$ts, attr, "sf_column")[[1]]

  # Reassign geometry column
  st_geometry(out) = sf_column_ts
  out
}
#' #' @importFrom sf st_set_crs
#' #' @export
#' st_set_crs.post_table = function() {
#'
#' }
#' #' @importFrom sf `st_crs<-`
#' #' @export
#' st_crs.post_table = function() {
#'
#' }
#' #' @importFrom sf st_set_precision
#' #' @export
#' st_set_precision.post_table = function() {
#'
#' }
#' #' @importFrom sf `st_precision<-`
#' #' @export
#' st_precision.post_table = function() {
#'
#' }
#' #' @importFrom sf st_transform
#' #' @export
#' st_transform.post_table = function() {
#'
#' }
#'
#' #' sf methods for post_array objects
#' #'
#' #' @param x a post_array object
#' #' @name sf-post_array
#' #' @importFrom sf st_as_sf
#' #' @export
#' st_as_sf.post_array = function() {
#'
#' }
