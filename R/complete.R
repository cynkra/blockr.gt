new_complete_gt_block <- function(title = character(), subtitle = character(), footnotes = character()) {
  ui <- function(id) {
    tagList(
      textInput(
        NS(id, "title"),
        label = "Add title (accepts markdown formatting)"
      ),
      textInput(
        NS(id, "subtitle"),
        label = "Add subtitle (accepts markdown formatting)"
      ),
      textInput(
        NS(id, "footnotes"),
        label = "Add footnores (accepts markdown formatting)",
      ),
      gt_output(
        NS(id, "gt_output")
      )
    )
  }

  server <- function(id) {
    moduleServer(id, function(input, output, session) {
      render_gt({
        head(mtcars, 5) |>
          gt() |>
          tab_header(title = md(input$title), subtitle = md(input$subtitle)) |>
          tab_footnote(md(input$footnotes))
      })

      list(
        expr = reactive(
          bquote(
            gt() |>
              tab_header(title = md(.(title)), subtitle = md(.subtitle)) |>
              tab_footnote(md(.footnotes)),
            list(
              title = input$title,
              subtitle = input$subtitle,
              footnotes = input$footnotes
            )
          )
        ),
        state = list(
          input$title,
          input$subtitle,
          input$footnotes
        )
      )
    })
  }

  new_block(ui = ui, server = server, class = "complete_gt_block")
}
