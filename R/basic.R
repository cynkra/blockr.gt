#' Create a new basic GT block
#'
#' Creates a new GT block with inputs for title, subtitle, and footnotes. This
#' block provides an interface for a basic GT table.
#'
#' @param title,subtitle,footnotes Initial text for each field (character).
#' All default to empty and accept markdown formatting.
#'
#' @return A basic gt block object that can be used with the serve function.
#'
#' @examples
#' \dontrun{
#' serve(new_basic_gt_block(), data = list(data = mtcars))
#' }
#'
#' @export
new_basic_gt_block <- function(
  title = character(),
  subtitle = character(),
  footnotes = character()
) {
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
        label = "Add footnotes (accepts markdown formatting)",
      ),
      gt_output(
        NS(id, "table")
      )
    )
  }

  server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      # The module must first be initialised with the values from the ctor
      title <- reactiveVal(title)
      subtitle <- reactiveVal(subtitle)
      footnotes <- reactiveVal(footnotes)

      # The initial values can then be updated with values from the UI
      observeEvent(input$title, title(input$title))
      observeEvent(input$subtitle, subtitle(input$subtitle))
      observeEvent(input$footnotes, footnotes(input$footnotes))

      output$table <- render_gt({
        gt(data()) |>
          tab_header(title = md(title()), subtitle = md(subtitle())) |>
          tab_footnote(md(footnotes()))
      })

      list(
        expr = reactive(
          bquote(
            gt(data()) |>
              tab_header(title = md(.(title)), subtitle = md(.(subtitle))) |>
              tab_footnote(md(.(footnotes))),
            list(
              title = title(),
              subtitle = subtitle(),
              footnotes = footnotes()
            )
          )
        ),
        state = list(
          title = reactive(title()),
          subtitle = reactive(subtitle()),
          footnotes = reactive(footnotes())
        )
      )
    })
  }

  new_block(
    ui = ui,
    server = server,
    class = "basic_gt_block",
    ctor = "new_basic_gt_block",
    ctor_pkg = "blockr.gt"
  )
}
