test_that("colour block constructor", {
  expect_s3_class(new_colour_gt_block(), "colour_block")
})

test_that("colour block server handles column changes", {
  testServer(
    app = new_colour_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(columns = c("mpg", "cyl"))
      expect_equal(input$columns, c("mpg", "cyl"))

      session$setInputs(columns = c("disp", "hp"))
      expect_equal(input$columns, c("disp", "hp"))
    }
  )
})

test_that("colour block server handles rows changes", {
  testServer(
    app = new_colour_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(rows = c(1, 5))
      expect_equal(input$rows[1], 1)
      expect_equal(input$rows[2], 5)

      session$setInputs(rows = c(2, 7))
      expect_equal(input$rows[1], 2)
      expect_equal(input$rows[2], 7)
    }
  )
})

test_that("colour block server handles direction changes", {
  testServer(
    app = new_colour_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(direction = "column")
      expect_equal(input$direction, "column")

      session$setInputs(direction = "row")
      expect_equal(input$direction, "row")
    }
  )
})

test_that("colour block server handles method changes", {
  testServer(
    app = new_colour_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(method = "auto")
      expect_equal(input$method, "auto")

      session$setInputs(method = "numeric")
      expect_equal(input$method, "numeric")

      session$setInputs(method = "bin")
      expect_equal(input$method, "bin")

      session$setInputs(method = "quantile")
      expect_equal(input$method, "quantile")

      session$setInputs(method = "factor")
      expect_equal(input$method, "factor")
    }
  )
})

test_that("colour block server handles palette changes", {
  testServer(
    app = new_colour_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(palette = "viridis")
      expect_equal(input$palette, "viridis")

      session$setInputs(palette = "Blues")
      expect_equal(input$palette, "Blues")
    }
  )
})

test_that("colour block server handles bins changes", {
  testServer(
    app = new_colour_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(bins = 5)
      expect_equal(input$bins, 5)

      session$setInputs(bins = 10)
      expect_equal(input$bins, 10)
    }
  )
})

test_that("colour block server handles quantiles changes", {
  testServer(
    app = new_colour_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(quantiles = 5)
      expect_equal(input$quantiles, 5)

      session$setInputs(quantiles = 10)
      expect_equal(input$quantiles, 10)
    }
  )
})

test_that("colour block server handles alpha changes", {
  testServer(
    app = new_colour_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(alpha = 0.5)
      expect_equal(input$alpha, 0.5)

      session$setInputs(alpha = 1.0)
      expect_equal(input$alpha, 1.0)
    }
  )
})

test_that("colour block server handles reverse changes", {
  testServer(
    app = new_colour_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(reverse = TRUE)
      expect_equal(input$reverse, TRUE)

      session$setInputs(reverse = FALSE)
      expect_equal(input$reverse, FALSE)
    }
  )
})

test_that("colour block server handles apply_to changes", {
  testServer(
    app = new_colour_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(apply_to = "fill")
      expect_equal(input$apply_to, "fill")

      session$setInputs(apply_to = "text")
      expect_equal(input$apply_to, "text")
    }
  )
})

test_that("colour state is correctly returned", {
  testServer(
    app = new_colour_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(columns = c("mpg", "cyl"))
      expect_equal(session$returned$state$columns(), c("mpg", "cyl"))

      session$setInputs(rows = c(1, 5))
      expect_equal(session$returned$state$rows(), 1:5)

      session$setInputs(direction = "column")
      expect_equal(session$returned$state$direction(), "column")

      session$setInputs(method = "bin")
      expect_equal(session$returned$state$method(), "bin")

      session$setInputs(palette = "viridis")
      expect_equal(session$returned$state$palette(), "viridis")

      session$setInputs(bins = 5)
      expect_equal(session$returned$state$bins(), 5)

      session$setInputs(quantiles = 5)
      expect_equal(session$returned$state$quantiles(), 5)

      session$setInputs(alpha = 0.2)
      expect_equal(session$returned$state$alpha(), 0.2)

      session$setInputs(reverse = TRUE)
      expect_equal(session$returned$state$reverse(), TRUE)

      session$setInputs(apply_to = "text")
      expect_equal(session$returned$state$apply_to(), "text")
    }
  )
})

test_that("expr evaluates correctly", {
  testServer(
    app = new_colour_gt_block()$expr_server,
    args = list(gt_obj = reactive(gt::gt(mtcars))),
    expr = {
      session$setInputs(columns = c("mpg", "disp"))
      session$setInputs(rows = c(1, 5))
      session$setInputs(direction = "column")
      session$setInputs(method = "numeric")
      session$setInputs(palette = "viridis")
      session$setInputs(alpha = 0.8)
      session$setInputs(reverse = FALSE)
      session$setInputs(apply_to = "fill")

      evaluated_expr <- eval(session$returned$expr())
      expect_s3_class(evaluated_expr, "gt_tbl")
    }
  )
})
