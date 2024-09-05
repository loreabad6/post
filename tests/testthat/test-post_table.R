library(dplyr, warn.conflicts = FALSE)
suppressPackageStartupMessages(library(sf))

poly = st_point(c(0.5,0.5)) |>
  st_sfc(crs = 4326) |>
  st_buffer(10000)

test_that("as_post_table works as expected", {
  expect_snapshot(as_post_table(polygons))
  expect_snapshot(as_post_table(as_post_array(polygons)))
  expect_snapshot(as_post_table(as_post_table(polygons)))
})

test_that("as_post_table default to the expected values", {
  expect_equal(
    as_post_table(polygons),
    as_post_table(polygons, group_id = "gid",
                  time_column_name = "datetime",
                  sf_column_name = "geometry",
                  geometry_summary = summarise_geometry_centroid)
  )
})

test_that("post_table fails when only one temporal row per group is given", {
  err = "requires at least two time values"
  expect_error(
    as_post_table(filter(polygons, datetime == "2020-10-01")),
    err
  )
  expect_error(
    as_post_table(as_post_array(filter(polygons, datetime == "2020-10-01"))),
    err
  )
})

test_that("empty geometries are dropped or not when coercing from post_array", {
  p_sst = st_filter(polygons, poly)
  arr = as_post_array(p_sst)
  tab_emp = as_post_table(arr, drop_empty = FALSE)
  tab_not_emp = as_post_table(arr)
  expect_false(any(st_is_empty(face_temporal(tab_not_emp))))
  expect_true(any(st_is_empty(face_temporal(tab_emp))))
})
