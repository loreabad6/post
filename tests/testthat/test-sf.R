library(sf)

test_that("st_as_sf works as expected", {
  expect_snapshot(st_as_sf(as_post_array(polygons)))
  expect_snapshot(st_as_sf(as_post_table(polygons)))
  expect_snapshot(st_as_sf(face_temporal(as_post_table(polygons))))
})
#post_table
tab = as_post_table(polygons)
cls = c("post_table", "sf")
test_that("st_crs<- works as expected, updates geometries and returns a post_table", {
  out1 = tab
  suppressWarnings(st_crs(out1) <- 3035)
  expect_equal(st_crs(out1)$epsg, st_crs(face_temporal(out1))$epsg)
  expect_s3_class(out1, cls)
})
test_that("st_normalize works as expected, updates geometries and returns a post_table", {
  out1 = st_normalize(tab)
  out2 = face_temporal(out1)
  expect_snapshot(out1)
  expect_snapshot(out2)
  expect_s3_class(out1, cls)
  expect_s3_class(out2, cls)
})
test_that("st_shift_longitude works as expected, updates geometries and returns a post_table", {
  expect_snapshot(st_shift_longitude(tab))
  expect_s3_class(st_shift_longitude(tab), cls)
})
test_that("st_transform works as expected, updates geometries and returns a post_table", {
  out1 = st_transform(tab, 3035)
  expect_equal(st_crs(out1)$epsg, st_crs(face_temporal(out1))$epsg)
  expect_s3_class(out1, cls)
})
test_that("st_wrap_dateline works as expected, updates geometries and returns a post_table", {
  expect_snapshot(st_wrap_dateline(tab))
  expect_s3_class(st_wrap_dateline(tab), cls)
})
test_that("st_zm works as expected, updates geometries and returns a post_table", {
  expect_snapshot(st_zm(tab))
  expect_s3_class(st_zm(tab), cls)
})
