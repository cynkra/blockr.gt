test_that("block ui returns a tag list", {
  expect_s3_class(
    block_ui("test", new_basic_gt_block()),
    "shiny.tag.list"
  )
})
