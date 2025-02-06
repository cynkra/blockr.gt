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
        NS(id, "table")
      )
    )
  }

  server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      output$table <- render_gt({
        gt(head(data())) |>
          tab_header(title = md(input$title), subtitle = md(input$subtitle)) |>
          tab_footnote(md(input$footnotes))
      })

      list(
        expr = reactive(
          bquote(
            gt(data) |>
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
          title = reactive(input$title),
          subtitle = reactive(input$subtitle),
          footnotes = reactive(input$footnotes)
        )
      )
    })
  }

  new_transform_block(
    ui = ui, server = server, class = "complete_gt_block"
  )
}
