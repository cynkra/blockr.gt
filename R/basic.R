#' Create a new basic GT block
#'
#' Creates a new GT block with inputs for title, subtitle, and footnotes. This
#' block provides an interface for a basic GT table.
#'
#' @param title,subtitle,footnotes Initial text for each field (character).
#' All default to empty and accept markdown formatting.
#' @param ... Forwarded to [blockr.core::new_block()]
#'
#' @return A basic gt block object that can be used with the serve function.
#'
#' @examples
#' \dontrun{
#' serve(new_basic_gt_block(title = "test"), data = list(data = mtcars))
#' }
#'
#' @export
new_basic_gt_block <- function(
  title = character(),
  subtitle = character(),
  footnotes = character(),
  ...
) {
  ui <- function(id) {
    tagList(
      textInput(
        NS(id, "title"),
        label = "Add title (accepts markdown formatting)",
        value = title
      ),
      textInput(
        NS(id, "subtitle"),
        label = "Add subtitle (accepts markdown formatting)",
        value = subtitle
      ),
      textInput(
        NS(id, "footnotes"),
        label = "Add footnotes (accepts markdown formatting)",
        value = footnotes
      ),
      gt_output(
        NS(id, "table")
      )
    )
  }

  server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      output$table <- render_gt({
        gt(data()) |>
          tab_header(title = md(input$title), subtitle = md(input$subtitle)) |>
          tab_footnote(md(input$footnotes))
      })

      list(
        expr = reactive(
          bquote(
            gt(data()) |>
              tab_header(title = md(.(title)), subtitle = md(.(subtitle))) |>
              tab_footnote(md(.(footnotes))),
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

  new_gt_block(
    ui = ui,
    server = server,
    class = "basic_block",
    ...
  )
}
