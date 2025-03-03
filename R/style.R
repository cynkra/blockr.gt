new_style_gt_block <- function(
  style = numeric(),
  color = character(),
  add_row_striping = logical()
) {
  ui <- function(id) {
    tagList(
      numericInput(
        NS(id, "style"),
        label = "Choose a default style from 1 to 6:",
        value = 1,
        min = 1,
        max = 6,
        step = 1
      ),
      selectInput(
        NS(id, "color"),
        label = "Select color:",
        choices = c("blue", "cyan", "pink", "green", "red", "gray"),
        selected = "gray"
      ),
      selectInput(
        NS(id, "striping"),
        label = "Should rows be striped?",
        choices = c("yes", "no"),
        selected = "yes"
      ),
      gt_output(
        NS(id, "table")
      )
    )
  }

  server <- function(id, gt_obj) {
    moduleServer(id, function(input, output, session) {
      # Initialise module with ctor values
      style <- reactiveVal(style)
      color <- reactiveVal(color)
      striping <- reactiveVal(striping)

      # Update values from the UI
      observeEvent(input$style, style(input$style))
      observeEvent(input$color, color(input$color))
      observeEvent(input$striping, striping(input$striping))

      output$table <- render_gt({
        gt(gt_obj()) |>
          opt_stylize(
            style = style(),
            color = color(),
            add_row_striping = ifelse(striping() == "yes", TRUE, FALSE)
          )
      })

      list(
        expr = reactive(
          bquote(
            gt(gt_obj()) |>
              opt_stylize(
                style = .(style),
                color = .(color),
                add_row_striping = ifelse(.(striping) == "yes", TRUE, FALSE)
              ),
            list(
              style = style(),
              color = color(),
              striping = striping()
            )
          )
        ),
        state = list(
          style = reactive(style()),
          color = reactive(color()),
          striping = reactive(striping())
        )
      )
    })
  }

  new_block(
    ui = ui,
    server = server,
    class = "style_gt_block",
    ctor = "new_style_gt_block",
    ctor_pkg = "blockr.gt"
  )
}
