#' Color a gt block
#'
#' Color targeted cells in a GT block.
#'
#' @param columns The columns to which cell data color operations are
#'   constrained. Defaults to everything.
#' @param rows In conjunction with columns, we can specify which of their rows
#'   should form a constraint for cell data color operations. Defaults to
#'   everything().
#' @param direction Should the color computations be performed column-wise or
#'   row-wise? Defeaults to colour down columns.
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
  ...
) {
  ui <- function(id) {
    tagList(
      gt_output(
        NS(id, "table")
      )
    )
  }

  server <- function(id, gt_obj) {
    moduleServer(id, function(input, output, session) {
      # Initialise module with ctor values
      columns <- reactiveVal()
      rows <- reactiveVal()
      direction <- reactiveVal()
      method <- reactiveVal()
      palette <- reactiveVal()

      # Update values from the UI

      output$table <- render_gt({
        gt_obj() |>
          data_color()
      })

      list(
        expr = reactive(
          bquote(
            gt_obj() |>
              data_color(),
            list(
              columns = columns(),
              rows = rows(),
              direction = direction(),
              method = method(),
              palette = palette()
            )
          )
        ),
        state = list(
          columns = reactive(columns()),
          rows = reactive(rows()),
          direction = reactive(direction()),
          method = reactive(method()),
          palette = reactive(palette())
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
