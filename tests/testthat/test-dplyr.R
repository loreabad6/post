library(cubble)
library(dplyr)
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(stars))

# post_table
tab = as_post_table(polygons)
cls_tab = c("post_table", "sf")

test_that("arrange works as expected and returns a post_table", {
  out1 = arrange(tab, gid)
  expect_snapshot(out1)
  expect_s3_class(out1, cls_tab)
  out2 = arrange(face_temporal(tab), datetime)
  expect_snapshot(out2)
  expect_s3_class(out2, cls_tab)
})
if (requireNamespace("cubelyr")) {
  library(cubelyr)
  test_that("filter works as expected and returns a post_table", {
    out1 = filter(tab, gid %in% c("b", "c"))
    expect_snapshot(out1)
    expect_s3_class(out1, cls_tab)
    out2 = filter(face_temporal(tab), gid %in% c("b", "c"))
    expect_snapshot(out2)
    expect_s3_class(out2, cls_tab)
  })
}
test_that("mutate works as expected and returns a post_table", {
  out1 = mutate(tab, foo = "bar")
  expect_snapshot(out1)
  expect_s3_class(out1, cls_tab)
  out2 = mutate(face_temporal(tab), foo = "bar")
  expect_snapshot(out2)
  expect_s3_class(out2, cls_tab)
})
test_that("rename works as expected and returns a post_table", {
  out1 = rename(tab, foo = gid)
  expect_snapshot(out1)
  expect_s3_class(out1, cls_tab)
  out2 = rename(face_temporal(tab), foo = gid)
  expect_snapshot(out2)
  expect_s3_class(out2, cls_tab)
})
test_that("rename updates sf_column", {
  expect_equal(
    attr(rename(tab, foo = geom_sum), "sf_column"),
    "foo"
  )
  expect_equal(
    attr(rename(face_temporal(tab), foo = geometry), "sf_column"),
    "foo"
  )
})
test_that("select works as expected and returns a post_table", {
  expect_message(
    out1 <- select(tab, lat),
    "Missing attribute `gid`, `long`, and `ts`, add it back."
  )
  expect_snapshot(out1)
  expect_s3_class(out1, cls_tab)
  expect_message(
    out2 <- select(face_temporal(tab), datetime),
    "Missing attribute `gid`, add it back."
  )
  expect_snapshot(out2)
  expect_s3_class(out2, cls_tab)
})
test_that("slice works as expected and returns a post_table", {
  out1 = slice(tab, 2:3)
  expect_snapshot(out1)
  expect_s3_class(out1, cls_tab)
  out2 = slice(face_temporal(tab), 2:3)
  expect_snapshot(out2)
  expect_s3_class(out2, cls_tab)
})

# post_array
arr = as_post_array(polygons)
cls_arr = c("post_array")

test_that("filter works as expected and returns a post_array", {
  out1 = filter(arr, datetime == "2020-10-03")
  expect_snapshot(out1)
  expect_s3_class(out1, cls_arr)
})
test_that("mutate works as expected and returns a post_array", {
  out1 = mutate(arr, foo = "bar")
  expect_snapshot(out1)
  expect_s3_class(out1, cls_arr)
})
test_that("rename works as expected and returns a post_array", {
  out1 = rename(arr, foo = "geometry")
  expect_snapshot(out1)
  expect_s3_class(out1, cls_arr)
})
test_that("rename updates sf_column in post_array", {
  expect_equal(attr(rename(arr, foo = "geometry"), "sf_column"), "foo")
})
test_that("select works as expected and returns a post_array", {
  out1 = select(arr, -1)
  expect_snapshot(out1)
  expect_s3_class(out1, cls_arr)
})
test_that("slice works as expected and returns a post_array", {
  out1 = slice(arr, "geom_sum", 3:4)
  expect_snapshot(out1)
  expect_s3_class(out1, cls_arr)
})
test_that("slice updates group_ids in post_array", {
  expect_equal(attr(slice(arr, "geom_sum", 3:4), "group_ids"), c("c", "d"))
})
test_that("transmute works as expected and returns a post_array", {
  out1 = transmute(arr, foo = "bar")
  expect_snapshot(out1)
  expect_s3_class(out1, cls_arr)
})
