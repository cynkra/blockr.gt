test_that("spanner block constructor", {
  expect_s3_class(new_spanner_gt_block(), "spanner_block")
})

test_that("spanner block server handles label changes", {
  testServer(
    app = new_spanner_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt(mtcars))),
    expr = {
      session$setInputs(label = "New Label")
      expect_equal(input$label, "New Label")

      session$setInputs(label = "Another Label")
      expect_equal(input$label, "Another Label")
    }
  )
})

test_that("spanner block server handles column changes", {
  testServer(
    app = new_spanner_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt(mtcars))),
    expr = {
      session$setInputs(columns = c("mpg", "cyl"))
      expect_equal(input$columns, c("mpg", "cyl"))

      session$setInputs(columns = c("disp", "hp"))
      expect_equal(input$columns, c("disp", "hp"))
    }
  )
})

test_that("spanner state is correctly returned", {
  testServer(
    app = new_spanner_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt(mtcars))),
    expr = {
      session$setInputs(label = "Test Label")
      expect_equal(session$returned$state$label(), "Test Label")

      session$setInputs(columns = c("mpg", "cyl"))
      expect_equal(session$returned$state$columns(), c("mpg", "cyl"))
    }
  )
})

test_that("expr evaluates correctly", {
  testServer(
    app = new_spanner_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt(mtcars))),
    expr = {
      session$setInputs(label = "Test Spanner")
      session$setInputs(columns = c("mpg", "cyl"))
      gt_obj <- gt_obj()
      evaluated_expr <- eval(session$returned$expr())
      expect_s3_class(evaluated_expr, "gt_tbl")
    }
  )
})
