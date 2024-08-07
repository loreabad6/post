# TODO: document
# Compute the geometry summary as the union and dissolve of
# the changing geometries
# INTERNAL USE!
#' @importFrom sf st_union st_make_valid
summarise_geometry_union = function(x,
                                    group_id = NULL,
                                    sf_column_name = NULL) {

  # group_id: Defaults to the first column of x, if not sfc or temporal class
  group_id = check_group_id(x, group_id)
  # Defaults to the active sf_column
  sf_column_name = check_sf_column(x, sf_column_name)

  x_groupped = split(x[[sf_column_name]], x[[group_id]])
  x_summarised = do.call(
    "c",
    lapply(x_groupped, function(i) sf::st_make_valid(sf::st_union(i)))
  )
  x_summarised
}

summarize_geometry_union = summarise_geometry_union

# Compute the geometry summary as the centroid of the
# union and dissolve of the changing geometries
# INTERNAL USE!
#' @importFrom sf st_centroid
summarise_geometry_centroid = function(x,
                                       group_id = NULL,
                                       sf_column_name = NULL) {

  # group_id: Defaults to the first column of x, if not sfc or temporal class
  group_id = check_group_id(x, group_id)
  # Defaults to the active sf_column
  sf_column_name = check_sf_column(x, sf_column_name)

  x_unioned = summarise_geometry_union(x)
  x_centroid = sf::st_centroid(x_unioned)
  x_centroid
}

summarize_geometry_centroid = summarise_geometry_centroid

# Compute the geometry summary as the bounding box of the
# union and dissolve of the changing geometries
# INTERNAL USE!
summarise_geometry_bbox = function(x,
                                   group_id = NULL,
                                   sf_column_name = NULL) {

  # group_id: Defaults to the first column of x, if not sfc or temporal class
  group_id = check_group_id(x, group_id)
  # Defaults to the active sf_column
  sf_column_name = check_sf_column(x, sf_column_name)

  x_unioned = summarise_geometry_union(x)
  x_bbox = st_bbox_by_feature(x_unioned)
  x_bbox
}

summarize_geometry_bbox = summarise_geometry_bbox

# Utility function to compute bounding box per feature
# INTERNAL USE!
#' @importFrom sf st_as_sfc st_bbox
st_bbox_by_feature = function(x) {
  f = function(y) sf::st_as_sfc(sf::st_bbox(y))
  do.call("c", lapply(x, f))
}
