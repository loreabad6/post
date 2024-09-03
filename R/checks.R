#' Check and assign defaults to post object arguments
#' @noRd

# group_id can be a character/integer value that is assigned to all rows
# in x or a column name that contains the group identifiers.
# Defaults to the first column of x

check_group_id = function(x = x, group_id = NULL) {
  if(!is.null(group_id)) {
    if(!(group_id %in% names(x))) {
      x["_gid_"] = group_id
      "_gid_"
    } else group_id
  } else if(is.null(group_id)) {
    group_id = names(x)[1]
    temp_class = c("POSIXct", "POSIXt", "Date", "PCICt")
    if(inherits(group_id, "sfc") | inherits(group_id, temp_class))
      stop("no appropriate group_id column found", call. = FALSE)
    group_id
  }
}

# time_column_name is the name of the column containing the
# temporal information. Defaults to the first temporal column.
check_time_column = function(x = x, time_column_name = NULL) {
  if(!is.null(time_column_name)) {
    stopifnot(
      "time_column_name not found" =
        time_column_name %in% names(x)
    )
    time_column_name
  } else if(is.null(time_column_name)) {
    # Check for columns matching the temp_class classes
    temp_class = c("POSIXct", "POSIXt", "Date", "PCICt")
    # If there are more than two temporal dimensions, the first one is taken
    x_ = sf::st_drop_geometry(x)
    time_column_name = names(
      x_[which(lapply(x_, class) %in% temp_class)[1]]
    )
    if (is.na(time_column_name))
      stop("x does not have a temporal dimension", call. = FALSE)
    time_column_name
  }
}


# sf_column_name is the name of the column with the `POLYGON` geometries.
# Defaults to the active sf_column
check_sf_column = function(x = x, sf_column_name = NULL) {
  if(!is.null(sf_column_name)) {
    stopifnot(
      "sf_column_name not found" =
        sf_column_name %in% names(x)
    )
  } else if(is.null(sf_column_name)) {
    sf_column_name = attr(x, "sf_column")
    if(is.null(sf_column_name)) {
      stop("x does not have a geometry column", call. = FALSE)
    }
  }
  # Check if sf_column inherits `sfc`
  stopifnot(
    "sf_column is not an sfc object" =
    inherits(x[[sf_column_name]], "sfc")
  )

  # Check if sf_column has `POLYGON` or `MULTIPOLYGON` geometries
  stopifnot(
    "sf_column is not of type POLYGON/MULTIPOLYGON" =
      any((sf::st_geometry_type(x[[sf_column_name]]) %in%
           c("POLYGON", "MULTIPOLYGON")))
    )
  sf_column_name
}

check_geometry_summary = function(x = x,
                                  geometry_summary = NULL,
                                  group_id = NULL,
                                  sf_column_name = NULL,
                                  ...) {
  if(is.function(geometry_summary)) {
    geometry_summary(x, group_id, sf_column_name, ...)
  } else if (inherits(geometry_summary, "sfc")) {
    if(length(geometry_summary) != length(unique(x[[group_id]]))) {
      stop("geometry_summary has more geometries than groups", call. = FALSE)
      # cli::cli_abort(c(
      #   "{length(geometry_summary)} geometry_summary row{?s} provided but {length(unique(x[[group_id]]))} group{?s} present",
      #   "x" = "number of geometry_summary rows and groups do not match"
      # ))
    }
    geometry_summary
  } else {
    stop("geometry_summary not recognised", call. = FALSE)
  }
}
