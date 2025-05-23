test_that("style block constructor", {
  expect_s3_class(new_style_gt_block(), "style_block")
})

test_that("style block server handles style changes", {
  testServer(
    app = new_style_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt(mtcars))),
    expr = {
      session$setInputs(style = 1)
      expect_equal(input$style, 1)

      session$setInputs(style = 6)
      expect_equal(input$style, 6)
    }
  )
})

test_that("style block server handles color changes", {
  testServer(
    app = new_style_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt(mtcars))),
    expr = {
      session$setInputs(color = "green")
      expect_equal(input$color, "green")

      session$setInputs(color = "cyan")
      expect_equal(input$color, "cyan")
    }
  )
})

test_that("style block server handles striping changes", {
  testServer(
    app = new_style_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt(mtcars))),
    expr = {
      session$setInputs(striping = "no")
      expect_equal(input$striping, "no")

      session$setInputs(striping = "yes")
      expect_equal(input$striping, "yes")
    }
  )
})

test_that("style state is correctly returned", {
  testServer(
    app = new_style_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt(mtcars))),
    expr = {
      session$setInputs(style = 1)
      expect_equal(session$returned$state$style(), 1)

      session$setInputs(color = "pink")
      expect_equal(session$returned$state$color(), "pink")

      session$setInputs(striping = "no")
      expect_equal(session$returned$state$striping(), "no")
    }
  )
})

test_that("expr evaluates correctly", {
  testServer(
    app = new_style_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt(mtcars))),
    expr = {
      session$setInputs(style = 1)
      session$setInputs(color = "cyan")
      session$setInputs(striping = "yes")
      gt_obj <- gt_obj()
      evaluated_expr <- eval(session$returned$expr())
      expect_s3_class(evaluated_expr, "gt_tbl")
    }
  )
})

test_that("incorrect colors throw an error", {
  testServer(
    app = new_style_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt(mtcars))),
    expr = {
      session$setInputs(style = 1)
      session$setInputs(color = "Ooops")
      session$setInputs(striping = "yes")
      expect_error(eval(session$returned$expr()))
    }
  )
})
