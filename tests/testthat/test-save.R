test_that("save block constructor", {
  block <- new_save_gt_block()
  expect_s3_class(block, "save_gt_block")
})
