test_that("as_post_array works as expected", {
  expect_snapshot(as_post_array(polygons))
  expect_snapshot(as_post_array(as_post_table(polygons)))
  expect_snapshot(as_post_array(as_post_array(polygons)))
})

test_that("as_post_table default to the expected values", {
  expect_equal(
    as_post_array(polygons),
    as_post_array(polygons, group_id = "gid",
                  time_column_name = "datetime",
                  sf_column_name = "geometry",
                  geometry_summary = summarise_geometry_centroid)
  )
})

test_that("get_group_ids works as expected", {
  expect_snapshot(get_group_ids(as_post_array(polygons)))
})
