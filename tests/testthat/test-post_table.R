library(dplyr, warn.conflicts = FALSE)
suppressPackageStartupMessages(library(sf))

poly = st_point(c(0.5,0.5)) |>
  st_sfc(crs = 4326) |>
  st_buffer(10000)

test_that("post_table fails when only one temporal row per group is given", {
  err = "requires at least two time values"
  expect_error(
    as_post_table(filter(polygons, datetime == "2020-10-01")),
    err
  )
  expect_error(
    as_post_table(as_post_array(filter(polygons, datetime == "2020-10-01"))),
    err
  )
})
