#' Add a spanner to a gt table
#'
#' @param label The text to use for the spanner label. Accepts markdown
#'   formatted text.
#' @param columns The columns to serve as components of the spanner.
#' @param ... Forwarded to [blockr.core::new_block()]
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
  ui <- function(id) {
    tagList(
      textInput(
        NS(id, "label"),
        label = "Spanner label (accepts markdown formatting)",
        value = label
      ),
      selectInput(
        NS(id, "columns"),
        label = "Select columns:",
        choices = NULL,
        multiple = TRUE
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

      list(
        expr = reactive(
          bquote(
            gt_obj() |>
              tab_spanner(label = .(label), columns = .(columns)),
            list(
              label = input$label,
              columns = input$columns
            )
          )
        ),
        state = list(
          label = reactive(input$label),
          columns = reactive(input$columns)
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
