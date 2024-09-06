suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(stars))

poly = st_point(c(0.5,0.5)) |>
  st_sfc(crs = 4326) |>
  st_buffer(10000)
cls = "post_array"

test_that("subsetting post_array works as expected", {
  out1 = as_post_array(
    polygons,
    geometry_summary = summarise_geometry_union
  )
  expect_equal(length(out1[poly]$geometry), 10)
  expect_equal(attr(out1[poly], "group_ids"), c("c", "d"))
  expect_equal(st_dimensions(out1[poly])$geom_sum$from, 3)
  expect_equal(st_dimensions(out1[poly])$geom_sum$to, 4)
  expect_s3_class(out1[poly], cls)
})
