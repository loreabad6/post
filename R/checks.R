#' Check and assign defaults to post object arguments
#' @importFrom cli cli_abort cli_warn
#' @importFrom sf st_drop_geometry st_geometry_type
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
    if(inherits(x[[group_id]], "sfc") | inherits(x[[group_id]], temp_class))
      cli::cli_abort(c(
        "a valid {.var group_id} column is required",
        "i" = "{.var group_id} should not inherit {.cls sfc}
        or {.cls {temp_class}}",
        "x" = "no appropriate {.var group_id} column found"
      ))
    group_id
  }
}

# time_column_name is the name of the column containing the
# temporal information. Defaults to the first temporal column.
check_time_column = function(x = x, time_column_name = NULL) {
  if(!is.null(time_column_name)) {
    if(!(time_column_name %in% names(x))) {
      cli::cli_abort(c(
      "x" = "{.var time_column_name} {.val {time_column_name}} not found"
      ))
    }
    time_column_name
  } else if(is.null(time_column_name)) {
    # Check for columns matching the temp_class classes
    temp_class = c("POSIXct", "POSIXt", "Date", "PCICt")
    # If there are more than one temporal columns, the first one is taken
    x_ = sf::st_drop_geometry(x)
    temp_cols_idx = which(lapply(x_, class) %in% temp_class)
    if(length(temp_cols_idx) > 1) {
      cli::cli_warn(c(
        "!" = "{.var x} has {length(temp_cols_idx)} temporal columns,
        the first one is taken as the {.var time_column}"
      ))
    }
    if (length(temp_cols_idx) == 0) {
      cli::cli_abort(c(
        "x" = "{.var x} has no temporal column"
      ))
    }
    time_column_name = names(x_[temp_cols_idx[1]])
    time_column_name
  }
}


# sf_column_name is the name of the column with the `POLYGON` geometries.
# Defaults to the active sf_column
check_sf_column = function(x = x, sf_column_name = NULL) {
  if(!is.null(sf_column_name)) {
    if(!(sf_column_name %in% names(x))) {
      cli::cli_abort(c(
        "x" = "{.var sf_column_name} {.val {sf_column_name}} not found"
      ))
    }
  } else if(is.null(sf_column_name)) {
    sf_column_name = attr(x, "sf_column")
  }
  # Check if sf_column inherits `sfc`
  if(!inherits(x[[sf_column_name]], "sfc")) {
    cli::cli_abort(c(
      "x"  = "{.var sf_column} {.val {sf_column_name}} is
      not an {.cls sfc} list-column"
    ))
  }
  # Check if sf_column has `POLYGON` or `MULTIPOLYGON` geometries
  geom_type_sup = c("POLYGON", "MULTIPOLYGON")
  geom_type_cur = sf::st_geometry_type(x[[sf_column_name]])
  if(!(any((geom_type_cur %in% geom_type_sup)))) {
    cli::cli_abort(c(
      "{.var sf_column} {.val {sf_column_name}} has
      {.cls {as.character(unique(geom_type_cur))}} geometry type",
      "i" = "{.pkg post} only supports {.cls {geom_type_sup}} at the moment",
      "x" = "not supported geometry type"
    ))
  }
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
    len_geoms = length(geometry_summary)
    len_group = length(unique(x[[group_id]]))
    if(len_geoms != len_group) {
      cli::cli_abort(c(
        "{len_geoms} geometry_summary row{?s} but {len_group} group{?s} found",
        "x" = "number of geometry_summary rows and groups do not match"
      ))
    }
    geometry_summary
  } else {
    cli::cli_abort(c(
      "x" = "{.var geometry_summary} {.val {geometry_summary}} not found"
    ))
  }
}
