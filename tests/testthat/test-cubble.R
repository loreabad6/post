tab = as_post_table(polygons)
cls_tab = c("post_table", "sf")

test_that("face_spatial works as expected and returns a post_table", {
  expect_message(
    out1 <- face_spatial(tab),
    "The cubble is already in the nested form"
  )
  expect_snapshot(out1)
  expect_s3_class(out1, cls_tab)
  expect_no_message(
    out2 <- face_spatial(face_temporal(tab))
  )
  expect_snapshot(out2)
  expect_s3_class(out2, cls_tab)
})
test_that("face_temporal works as expected and returns a post_table", {
  expect_no_message(
    out1 <- face_temporal(tab)
  )
  expect_snapshot(out1)
  expect_s3_class(out1, cls_tab)
  expect_message(
    out2 <- face_temporal(face_temporal(tab)),
    "The cubble is already in the long form"
  )
  expect_snapshot(out2)
  expect_s3_class(out2, cls_tab)
})
test_that("spatial works as expected", {
  expect_snapshot(spatial(tab))
})
