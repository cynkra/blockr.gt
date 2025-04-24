#' Add a spanner to a gt table
#'
#' @param label The text to use for the spanner label. Accepts markdown
#'   formatted text.
#' @param columns The columns to serve as components of the spanner.
#' @param ... Forwarded to [new_block()]
#'
#' @return A gt block object with an added spanner.
#'
#' @examples
#' \dontrun{
#' serve(new_spanner_gt_block(), data = list(gt_obj = gt::gt(mtcars)))
#' }
#'
#' @export
new_spanner_gt_block <- function(
  label = character(),
  columns = character(),
  ...
) {
  ui <- function(id, gt_obj) {
    tagList(
      textInput(
        NS(id, "label"),
        label = "Spanner label (accepts markdown formatting)",
        value = label
      ),
      selectInput(
        NS(id, "columns"),
        label = "Select columns:",
        choices = gt_obj$`_boxhead`$column_label,
        multiple = TRUE
      ),
      gt_output(
        NS(id, "table")
      )
    )
  }

  server <- function(id, gt_obj) {
    moduleServer(id, function(input, output, session) {
      # Initialise module with ctor values
      label <- reactiveVal(label)
      columns <- reactiveVal(columns)

      # Update values from the UI
      observeEvent(input$label, label(input$label))
      observeEvent(input$columns, columns(input$columns))

      output$table <- render_gt({
        gt_obj() |>
          tab_spanner(label = label(), columns = columns())
      })

      list(
        expr = reactive(
          bquote(
            gt_obj() |>
              tab_spanner(lable = .(label), columns = .(columns)),
            list(
              label = label(),
              columns = columns()
            )
          )
        ),
        state = list(
          label = reactive(label()),
          columns = reactive(columns())
        )
      )
    })
  }

  new_gt_block(
    ui = ui,
    server = server,
    class = "spanner_block",
    ...
  )
}
