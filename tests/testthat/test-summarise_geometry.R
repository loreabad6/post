test_that("summarise_geometry_* functions works as expected", {
  expect_snapshot(summarise_geometry_bbox(polygons))
  expect_snapshot(summarise_geometry_union(polygons))
  expect_snapshot(summarise_geometry_centroid(polygons))
})
