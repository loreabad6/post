#' Plot post_array with plot.stars default
#'
#' Plots a post_array with the default `plot.stars()` function by taking the first
#' non-spatial attribute. The summary geometry is used for plotting.
#' When no non-spatial attribute is available, the plot function returns an error.
#' Try `autoplot.post_array()` to achieve those type of plots and to plot changing
#' geometries.
#'
#' @name plot
#' @param x an object of class post_array
#' @param y ignored
#' @param ... passed on to `plot.stars()`
#'
#' @examples
#' library(sf)
#' arr = as_post_array(polygons)
#' arr$area = st_area(arr$geometries)
#' plot(arr)
#'
#' @seealso [autoplot.post_array()]
#' @export
plot.post_array = function(x, y, ...) {
  x[[attr(x, "sf_column")]] = NULL
  if(identical(attr(x, "names"), character(0)))
    cli::cli_abort(c(
      "x" = "no non-spatial attribute present in {.var x}",
      "i" = "try using autoplot.post_array() instead"
    ))
  x = remove_post_array(x)
  NextMethod()
}

#' Plot post_array or post_table with ggplot2
#'
#' Plots the changing or summary geometries in a post_array or post_table as facets
#' defined by the first non spatial dimension. Other dimensions are discarded.
#'
#' @param object a post_array or post_table object
#' @param attribute the attribute to fill the geometries
#' @param max_plot maximum number of subplots rendered
#' @param geom_sum should the geom_sum geometries be used for plotting? Defaults to `FALSE`
#' @param ... ignored
#'
#' @importFrom dplyr slice
#' @importFrom rlang sym `!!`
#' @importFrom stars st_dimensions
#' @importFrom sf st_geometry
#'
#' @examples
#' library(ggplot2)
#' library(sf)
#' arr = as_post_array(polygons, geometry_summary = summarise_geometry_union)
#' arr$area = st_area(arr$geometry)
#' autoplot(arr, area)
#' autoplot(arr, area, geom_sum = TRUE)
#' autoplot(arr, area, max_plot = 3)
#'
#' tab = as_post_table(polygons, geometry_summary = summarise_geometry_union)
#' autoplot(tab, gid)
#' autoplot(tab, gid, geom_sum = TRUE)
#'
#' @name autoplot
#'
#' @return an object of class `ggplot`
#' @details See `ggplot2::autoplot()`.
#' @export
autoplot.post_array = function(object, attribute = NULL,
                               max_plot = 9,
                               geom_sum = FALSE, ...) {
  # Get the first spatial dimension name
  sf_dim = names(
    which(
      sapply(st_dimensions(object), \(i) inherits(i$value, "sfc"))
    ))[1]
  # Get the first non-spatial dimension
  nonsf_dim = names(
    which(
      sapply(st_dimensions(object), \(i) !inherits(i$value, "sfc"))
    ))[1]
  # Get only the levels to be plotted from the nonsf_dim according to max.plot
  if (st_dimensions(object)[[nonsf_dim]]$to <= max_plot) {
    max_plot = st_dimensions(object)[[nonsf_dim]]$to
  }
  object = slice(object, along = !!nonsf_dim, index = seq(1, max_plot))
  # Convert to sf
  object = st_as_sf(object)
  # Assign summary geometry as sf_column if geom_sum = TRUE
  if (geom_sum)
    st_geometry(object) = sf_dim
  # Plot
  ggplot2::ggplot(object) +
    aes_post(object, {{attribute}}) +
    ggplot2::geom_sf() +
    ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(nonsf_dim)))
}

#' @rdname autoplot
#' @importFrom cubble face_spatial face_temporal index_var index
#' @importFrom dplyr filter slice
#' @export
autoplot.post_table = function(object, attribute = NULL,
                               max_plot = 9,
                               geom_sum = FALSE, ...) {
  suppressWarnings({object = face_temporal(object)})
  index_values = unique(object[[index_var(object)]])
  if (length(index_values) <= max_plot) {
    max_plot = length(index_values)
  }
  object = filter(object, !!index(object) %in% index_values[1:max_plot])
  if (geom_sum) object = face_spatial(object)
  ggplot2::ggplot(object) +
    aes_post(object, {{attribute}}) +
    ggplot2::geom_sf() +
    ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(attr(object, "index"))))
}

#' @noRd
#' @importFrom sf st_geometry_type
aes_post = function(object, attribute) {
  if (unique(st_geometry_type(object)) %in% c("POINT", "LINESTRING")) {
    ggplot2::aes(color = {{attribute}})
  } else ggplot2::aes(fill = {{attribute}})
}
