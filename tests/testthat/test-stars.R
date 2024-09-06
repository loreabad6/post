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
  expect_snapshot(out1[poly])
  expect_s3_class(out1[poly], cls)
})
