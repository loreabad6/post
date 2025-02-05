library(sf, quietly = TRUE, verbose = FALSE)
arr = st_as_sf(as_post_array(polygons))
tab_sp = st_as_sf(as_post_table(polygons))
tab_tm = st_as_sf(face_temporal(as_post_table(polygons)))
cls = "sf"
test_that("st_as_sf returns a sf object", {
  out1 = arr
  out2 = tab_sp
  out3 = tab_tm
  expect_s3_class(out1, cls)
  expect_s3_class(out2, cls)
  expect_s3_class(out3, cls)
})
cls = "sfc"
test_that("st_as_sf returns a sf object with two geometry-list columns", {
  out1 = sum(sapply(arr, function(i) inherits(i, "sfc")))
  out2 = sum(sapply(tab_sp, function(i) inherits(i, "sfc")))
  out3 = sum(sapply(tab_tm, function(i) inherits(i, "sfc")))
  expect_equal(out1, 2)
  expect_equal(out2, 2)
  expect_equal(out3, 2)
})
test_that("st_as_sf returns the same sf object from similar post objects", {
  out1 = arr
  out2 = tab_sp
  out3 = tab_tm
  expect_equal(st_bbox(out1), st_bbox(out2))
  expect_equal(st_bbox(out1), st_bbox(out3))
  expect_equal(st_bbox(out2), st_bbox(out3))
  expect_identical(out1$geometry, out2$geometry)
  expect_identical(out1$geometry, out3$geometry)
  expect_identical(out2$geometry, out3$geometry)
})
#post_table
tab = as_post_table(polygons)
cls = c("post_table", "sf")
test_that("st_crs<- works as expected, updates geometries and returns a post_table", {
  out1 = tab
  suppressWarnings(st_crs(out1) <- 3035)
  expect_equal(st_crs(out1)$epsg, st_crs(face_temporal(out1))$epsg)
  expect_s3_class(out1, cls)
  out2 = face_temporal(tab)
  suppressWarnings(st_crs(out2) <- 3035)
  expect_equal(st_crs(out2)$epsg, st_crs(face_spatial(out2))$epsg)
  expect_s3_class(out2, cls)
})
test_that("st_normalize works as expected, updates geometries and returns a post_table", {
  out1 = st_normalize(tab)
  out2 = face_temporal(out1)
  expect_false(all(st_coordinates(out1) < 0))
  expect_false(all(st_coordinates(out1) > 1))
  expect_false(all(st_coordinates(out2) < 0))
  expect_false(all(st_coordinates(out2) > 1))
  expect_s3_class(out1, cls)
  expect_s3_class(out2, cls)
})
test_that("st_shift_longitude works as expected, updates geometries and returns a post_table", {
  expect_true(all(st_bbox(st_shift_longitude(tab))[['xmin']] > 0))
  expect_true(all(st_bbox(st_shift_longitude(tab))[['xmax']] > 0))
  expect_s3_class(st_shift_longitude(tab), cls)
  expect_true(all(st_bbox(face_temporal(st_shift_longitude(tab)))[['xmin']] > 0))
  expect_true(all(st_bbox(face_temporal(st_shift_longitude(tab)))[['xmax']] > 0))
  expect_s3_class(st_shift_longitude(face_temporal(tab)), cls)
})
test_that("st_transform works as expected, updates geometries and returns a post_table", {
  out1 = st_transform(tab, 3035)
  expect_equal(st_crs(out1)$epsg, st_crs(face_temporal(out1))$epsg)
  expect_s3_class(out1, cls)
  out2 = st_transform(face_temporal(tab), 3035)
  expect_equal(st_crs(out2)$epsg, st_crs(face_spatial(out2))$epsg)
  expect_s3_class(out2, cls)
})
test_that("st_wrap_dateline works as expected, updates geometries and returns a post_table", {
  expect_s3_class(st_wrap_dateline(tab), cls)
  expect_s3_class(st_wrap_dateline(face_temporal(tab)), cls)
})
test_that("st_zm works as expected, updates geometries and returns a post_table", {
  expect_false(all(c("Z", "M") %in% colnames(st_coordinates(st_zm(tab)))))
  expect_s3_class(st_zm(tab), cls)
  expect_false(all(c("Z", "M") %in% colnames(st_coordinates(face_temporal(st_zm(tab))))))
  expect_s3_class(st_zm(face_temporal(tab)), cls)
})
