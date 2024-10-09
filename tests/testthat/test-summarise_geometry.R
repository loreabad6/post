test_that("summarise_geometry_* functions outputs the
          expected number of groups", {
  expect_equal(length(summarise_geometry_bbox(polygons)), 5)
  expect_equal(length(summarise_geometry_union(polygons)), 5)
  expect_equal(length(summarise_geometry_centroid(polygons)), 5)
  expect_equal(length(summarise_geometry_convex_hull(polygons)), 5)
  expect_equal(
    length(suppressWarnings(
      summarise_geometry_bbox(polygons, rotated = TRUE)
    )), 5)
})

test_that("summarise_geometry_* functions outputs the
          expected geometry type", {
  expect_equal(as.character(unique(st_geometry_type(
    summarise_geometry_bbox(polygons)))), "POLYGON")
  expect_equal(as.character(unique(st_geometry_type(
    summarise_geometry_union(polygons)))), "POLYGON")
  expect_equal(as.character(unique(st_geometry_type(
    summarise_geometry_centroid(polygons)))), "POINT")
  expect_equal(as.character(unique(st_geometry_type(
    summarise_geometry_convex_hull(polygons)))), "POLYGON")
  expect_equal(as.character(unique(st_geometry_type(
    suppressWarnings(summarise_geometry_bbox(polygons, rotated = TRUE))))),
    "POLYGON")
})

test_that("summarise_geometry_* functions outputs the
          expected bounding box", {
  expect_equal(as.vector(st_bbox(summarise_geometry_bbox(polygons))),
               c(-0.2974337, -0.0029756, 0.9730806, 1.1535583))
  expect_equal(as.vector(st_bbox(summarise_geometry_union(polygons))),
               c(-0.2974337, -0.0029756,  0.9730806,  1.1535583))
  expect_equal(as.vector(st_bbox(summarise_geometry_centroid(polygons))),
               c(-0.002543201 , 0.177720021, 0.752585036, 0.901858839))
  expect_equal(as.vector(st_bbox(summarise_geometry_convex_hull(polygons))),
               c(-0.2974337, -0.0029756,  0.9730806,  1.1535583))
  # expect_equal(as.vector(st_bbox(suppressWarnings(
  #   summarise_geometry_bbox(polygons, rotated = TRUE)))),
  #              c(-0.41571489, -0.01302435,  0.98811672,  1.16517691))
})

test_that("providing a vector to summarise_geoemtry is supported
          with a warning", {
  expect_warning(
    summarise_geometry_union(polygons, group_id = rep(1:5, times = 5)),
    "assuming correct order and unique timestamps per group"
  )
})
