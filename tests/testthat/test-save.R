test_that("save block constructor", {
  expect_s3_class(new_save_gt_block(), "save_gt_block")
})

test_that("save block server handles format changes", {
  testServer(
    app = new_save_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(format = "pdf")
      expect_equal(format(), "pdf")

      session$setInputs(format = "html")
      expect_equal(format(), "html")

      session$setInputs(format = "png")
      expect_equal(format(), "png")
    }
  )
})

test_that("save block server handles expand parameter", {
  testServer(
    app = new_save_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(expand = 5)
      expect_equal(expand(), 5)

      session$setInputs(expand = 20)
      expect_equal(expand(), 20)
    }
  )
})

test_that("block state is correctly returned", {
  testServer(
    app = new_save_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      expect_equal(session$returned$state$format(), character())
      expect_equal(session$returned$state$expand(), numeric())

      session$setInputs(format = "pdf")
      expect_equal(session$returned$state$format(), "pdf")

      session$setInputs(expand = 10)
      expect_equal(session$returned$state$expand(), 10)
    }
  )
})
