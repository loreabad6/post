library(dplyr, warn.conflicts = FALSE)
# group_ids
test_that("passing an sf object without a proper group_id column fails", {
  err = "no appropriate `group_id` column found"
  expect_error(as_post_array(select(polygons, -gid)), err)
  expect_error(as_post_table(select(polygons, -gid)), err)
})
test_that("passing a vector to group_id gives a warning", {
  wrn = "assuming correct order and unique timestamps per group"
  expect_warning(as_post_array(polygons, group_id = rep(1:5, times = 5)), wrn)
  expect_warning(as_post_table(polygons, group_id = rep(1:5, times = 5)), wrn)
  expect_warning(as_post_array(filter(polygons, gid == "a"), group_id = 1), wrn)
  expect_warning(as_post_table(filter(polygons, gid == "a"), group_id = 1), wrn)
})
test_that("passing a vector to group_id with incorrect length fails", {
  err = "length does not match requirements"
  expect_error(as_post_array(polygons, group_id = 1:3))
  expect_error(as_post_table(polygons, group_id = 1:3))
})
# time_column_name
test_that("passing unexisting time_column_name column fails", {
  err = "not found"
  expect_error(as_post_array(polygons, time_column_name = "foo"), err)
  expect_error(as_post_table(polygons, time_column_name = "foo"), err)
})
test_that("finding more than one temporal column gives a warning
          when creating post object from temporal_column_name = NULL", {
  wrn = "the first one is taken"
  expect_warning(as_post_array(mutate(polygons, date = datetime)), wrn)
  expect_warning(as_post_table(mutate(polygons, date = datetime)), wrn)
})
test_that("passing an sf without a temporal column fails", {
  err = "has no temporal column"
  expect_error(as_post_array(select(polygons, -datetime)), err)
  expect_error(as_post_table(select(polygons, -datetime)), err)
})
# sf_column_name
test_that("passing unexisting sf_column_name column fails", {
  err = "not found"
  expect_error(as_post_array(polygons, sf_column_name = "foo"), err)
  expect_error(as_post_table(polygons, sf_column_name = "foo"), err)
})
test_that("passing an sf_column_name that is not an sfc fails", {
  err = "is not an <sfc> list-column"
  expect_error(
    as_post_array(mutate(polygons, foo = "bar"), sf_column_name = "foo"),
    err
  )
  expect_error(
    as_post_table(mutate(polygons, foo = "bar"), sf_column_name = "foo"),
    err
  )
})
test_that("passing non-polygon geometries fails", {
  err = "not supported geometry type"
  polygons_centroid = suppressWarnings(st_centroid(polygons))
  expect_error(as_post_array(polygons_centroid))
  expect_error(as_post_table(polygons_centroid))
})
# geometry_summary
test_that("passing unequal number of geometry_summary rows and groups fails", {
  err = "do not match"
  geoms = summarise_geometry_union(polygons)
  expect_error(as_post_array(polygons, geometry_summary = c(geoms, geoms)), err)
  expect_error(as_post_table(polygons, geometry_summary = c(geoms, geoms)), err)
})

test_that("passing unexisting geometry_summary column fails", {
  err = "not found"
  expect_error(as_post_array(polygons, geometry_summary = "foo"), err)
  expect_error(as_post_table(polygons, geometry_summary = "foo"), err)
})

