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
  expect_equal(as.vector(round(st_bbox(summarise_geometry_bbox(polygons)), 4)),
               c(-0.2974, -0.0030, 0.9731, 1.1536))
  expect_equal(as.vector(round(st_bbox(summarise_geometry_union(polygons)), 4)),
               c(-0.2974, -0.0030,  0.9731,  1.1536))
  expect_equal(as.vector(round(st_bbox(summarise_geometry_centroid(polygons)), 4)),
               c(-0.0095 , 0.1952, 0.7628, 0.9018))
  expect_equal(as.vector(round(st_bbox(summarise_geometry_convex_hull(polygons)), 4)),
               c(-0.2974, -0.0030,  0.9731,  1.1536))
  expect_equal(as.vector(round(st_bbox(suppressWarnings(
    summarise_geometry_bbox(polygons, rotated = TRUE))), 4)),
               c(-0.4157, -0.0130,  0.9881,  1.1652))
})

test_that("providing a vector to summarise_geoemtry is supported
          with a warning", {
  expect_warning(
    summarise_geometry_union(polygons, group_id = rep(1:5, times = 5)),
    "assuming correct order and unique timestamps per group"
  )
})
