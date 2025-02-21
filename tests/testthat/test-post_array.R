suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(stars))

# test_that("as_post_array works as expected", {
  # expect_snapshot(as_post_array(polygons))
  # expect_snapshot(as_post_array(as_post_table(polygons)))
  # expect_snapshot(as_post_array(as_post_array(polygons)))
# })

test_that("as_post_table default to the expected values", {
  expect_equal(
    as_post_array(polygons),
    as_post_array(polygons, group_id = "gid",
                  time_column_name = "datetime",
                  sf_column_name = "geometry",
                  geometry_summary = summarise_geometry_centroid)
  )
})

pnt = st_sample(st_as_sfc(st_bbox(polygons)), 10)
dates = c(Sys.Date()-1, Sys.Date())

vdc1 = st_sf(rdm = rnorm(20), polygons = st_buffer(st_sample(st_bbox(pnt), 20), 500),
             geometry = rep(pnt, 2), date = rep(dates, each = 10)) |>
  st_as_stars(dims = c("geometry", "date"))

vdc2 = st_sf(rdm = rnorm(20),
             geometry = rep(pnt, 2), date = rep(dates, each = 10)) |>
  st_as_stars(dims = c("geometry", "date"))

vdc3 = st_as_stars(polygons, dims = c("gid", "datetime"))

test_that("as_post_array for stars keeps stars object with added attributes", {
  expect_equal(as_post_array(vdc1), vdc1, ignore_attr = TRUE)
  expect_s3_class(as_post_array(vdc1), "post_array")
  expect_equal(attr(as_post_array(vdc1), "group_ids"), 1:10)
  expect_equal(attr(as_post_array(vdc1), "group_id_colname"), "gid")
  expect_equal(attr(as_post_array(vdc1), "sf_column"), "polygons")
})

test_that("as_post_array fails for stars without sfc attributes or dimensions", {
  expect_error(as_post_array(vdc2), "`x` does not have a <sfc> attribute")
  expect_error(as_post_array(vdc3), "`x` does not have a <sfc> dimension")
})

# test_that("get_group_ids works as expected", {
  # expect_snapshot(get_group_ids(as_post_array(polygons)))
# })
