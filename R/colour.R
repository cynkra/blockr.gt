#' Color a gt block
#'
#' Color targeted cells in a GT block.
#'
#' @param columns The columns to which cell data color operations are
#'   constrained.
#' @param rows In conjunction with columns, we can specify which rows should
#'   form a constraint for cell data color operations. Defaults to everything.
#' @param direction Should the color computations be performed column-wise
#'   (default) or row-wise? Defeaults to colour down columns.
#' @param method A method for computing color based on the data within body
#'   cells. Can be "auto" (the default), "numeric", "bin", "quantile", or
#'   "factor". The "auto" method will automatically choose the "numeric" method
#'   for numerical input data or the "factor" method for any non-numeric inputs.
#' @param palette Name of the colour palette to use.
#' @param ... Forwarded to [blockr.core::new_block()]
#'
#' @return A gt block with additional cell colouring.
#'
#' @examples
#' \dontrun{
#' serve(new_colour_gt_block(), data = list(gt_obj = gt::gt(head(mtcars))))
#' }
#'
#' @export
new_colour_gt_block <- function(
  columns = numeric(),
  rows = character(),
  direction = character(),
  method = character(),
  palette = character(),
  bins = integer(),
  quantiles = integer(),
  alpha = numeric(),
  reverse = logical(),
  apply_to = character(),
  ...
) {
  ui <- function(id) {
    tagList(
      selectInput(
        NS(id, "columns"),
        label = "Select columns to color:",
        choices = NULL,
        multiple = TRUE
      ),
      sliderInput(
        NS(id, "rows"),
        label = "Select range of row numbers to color:",
        min = 1,
        max = 10,
        value = c(1, 1),
        step = 1
      ),
      selectInput(
        NS(id, "direction"),
        label = "Should color computations be performed down columns or across rows?",
        choices = c("column", "row")
      ),
      selectInput(
        NS(id, "method"),
        label = "Select method for computing color:",
        choices = c("auto", "numeric", "bin", "quantile", "factor")
      ),
      conditionalPanel(
        condition = "input.method == 'bin'",
        numericInput(
          NS(id, "bins"),
          label = "Select the number of bins",
          value = 8,
          min = 2,
          step = 1
        ),
        ns = NS(id)
      ),
      conditionalPanel(
        condition = "input.method == 'quantile'",
        numericInput(
          NS(id, "quantiles"),
          label = "Select the number of (equal-size) quantiles:",
          value = 4,
          min = 1,
          step = 1
        ),
        ns = NS(id)
      ),
      selectInput(
        NS(id, "palette"),
        label = "Select colour palette:",
        choices = c(
          # Viridis palettes
          "viridis",
          "magma",
          "plasma",
          "inferno",
          # RColorBrewer palettes
          "BrBG",
          "PiYG",
          "PRGn",
          "PuOr",
          "RdBu",
          "RdYlBu",
          "RdGy",
          "RdYlGn",
          "Spectral",
          "Dark2",
          "Paired",
          "Set1",
          "Set2",
          "Set3",
          "Accent",
          "Pastel1",
          "Pastel2",
          "Blues",
          "BuGn",
          "BuPu",
          "GnBu",
          "Greens",
          "Greys",
          "Oranges",
          "OrRd",
          "PuBu",
          "PuBuGn",
          "PuRd",
          "Purples",
          "RdPu",
          "Reds",
          "YlGn",
          "YlGnBu",
          "YlOrBr",
          "YlOrRd"
        ),
      ),
      numericInput(
        NS(id, "alpha"),
        label = "Select color transparency level: ",
        value = 0.8,
        min = 0,
        max = 1,
        step = 0.1
      ),
      checkboxInput(
        NS(id, "reverse"),
        label = "Should the colors be computed in reverse order?"
      ),
      selectInput(
        NS(id, "apply_to"),
        label = "Should colors fill the cell background or the text?",
        choices = c("fill", "text")
      ),
      gt_output(
        NS(id, "table")
      )
    )
  }

  server <- function(id, gt_obj) {
    moduleServer(id, function(input, output, session) {
      updateSelectInput(
        session,
        "columns",
        choices = isolate(gt_obj())$`_boxhead`$column_label
      )

      updateSliderInput(
        session,
        "rows",
        max = nrow(isolate(gt_obj())$`_stub_df`),
        value = c(1, nrow(isolate(gt_obj())$`_stub_df`))
      )

      output$table <- render_gt({
        gt_obj() |>
          data_color(
            columns = input$columns,
            rows = input$rows[1]:input$rows[2],
            direction = input$direction,
            method = input$method,
            palette = input$palette,
            bins = input$bins,
            quantiles = input$quantiles,
            alpha = input$alpha,
            reverse = input$reverse,
            apply_to = input$apply_to
          )
      })

      list(
        expr = reactive(
          bquote(
            gt_obj() |>
              data_color(
                columns = .(columns),
                rows = .(rows),
                direction = .(direction),
                method = .(method),
                palette = .(palette),
                bins = .(bins),
                quantiles = .(quantiles),
                alpha = .(alpha),
                reverse = .(reverse),
                apply_to = .(apply_to)
              ),
            list(
              columns = input$columns,
              rows = input$rows[1]:input$rows[2],
              direction = input$direction,
              method = input$method,
              palette = input$palette,
              bins = input$bins,
              quantiles = input$quantiles,
              alpha = input$alpha,
              reverse = input$reverse,
              apply_to = input$apply_to
            )
          )
        ),
        state = list(
          columns = reactive(input$columns),
          rows = reactive(input$rows[1]:input$rows[2]),
          direction = reactive(input$direction),
          method = reactive(input$method),
          palette = reactive(input$palette),
          bins = reactive(input$bins),
          quantiles = reactive(input$quantiles),
          alpha = reactive(input$alpha),
          reverse = reactive(input$reverse),
          apply_to = reactive(input$apply_to)
        )
      )
    })
  }

  new_gt_block(
    ui = ui,
    server = server,
    class = "colour_block",
    ...
  )
}
