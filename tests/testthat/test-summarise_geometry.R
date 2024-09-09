test_that("summarise_geometry_* functions works as expected", {
  expect_snapshot(summarise_geometry_bbox(polygons))
  expect_snapshot(summarise_geometry_union(polygons))
  expect_snapshot(summarise_geometry_centroid(polygons))
  expect_snapshot(summarise_geometry_bbox(polygons, rotated = TRUE))
  expect_snapshot(summarise_geometry_convex_hull(polygons))
  expect_warning(
    summarise_geometry_union(polygons, group_id = rep(1:5, times = 5)),
    "assuming correct order and unique timestamps per group"
  )
})
