test_that("basic block constructor", {
  expect_s3_class(new_basic_gt_block(), "basic_gt_block")
})

test_that("basic block server handles title changes", {
  testServer(
    app = new_basic_gt_block()$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      session$setInputs(title = "New Title")
      expect_equal(title(), "New Title")

      session$setInputs(title = "Another Title")
      expect_equal(title(), "Another Title")
    }
  )
})

test_that("basic block server handles subtitle changes", {
  testServer(
    app = new_basic_gt_block()$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      session$setInputs(subtitle = "New Subtitle")
      expect_equal(subtitle(), "New Subtitle")

      session$setInputs(subtitle = "Another Subtitle")
      expect_equal(subtitle(), "Another Subtitle")
    }
  )
})

test_that("basic block server handles footnotes changes", {
  testServer(
    app = new_basic_gt_block()$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      session$setInputs(footnotes = "New Footnote")
      expect_equal(footnotes(), "New Footnote")

      session$setInputs(footnotes = "Another Footnote")
      expect_equal(footnotes(), "Another Footnote")
    }
  )
})

test_that("block state is correctly returned", {
  testServer(
    app = new_basic_gt_block()$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      expect_equal(session$returned$state$title(), character())
      expect_equal(session$returned$state$subtitle(), character())
      expect_equal(session$returned$state$footnotes(), character())

      session$setInputs(title = "Test Title")
      expect_equal(session$returned$state$title(), "Test Title")

      session$setInputs(subtitle = "Test Subtitle")
      expect_equal(session$returned$state$subtitle(), "Test Subtitle")

      session$setInputs(footnotes = "Test Footnote")
      expect_equal(session$returned$state$footnotes(), "Test Footnote")
    }
  )
})

test_that("expr evaluates correctly", {
  testServer(
    app = new_basic_gt_block()$expr_server,
    args = list(data = reactive(mtcars)),
    expr = {
      session$setInputs(title = "Test Title")
      session$setInputs(subtitle = "Test Subtitle")
      session$setInputs(footnotes = "Test Footnote")
      evaluated_expr <- eval(session$returned$expr())
      expect_s3_class(evaluated_expr, "gt_tbl")
    }
  )
})
