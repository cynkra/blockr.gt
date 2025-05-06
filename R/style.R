#' Style a GT block
#'
#' Style a GT block using one of six prebuilt defaults with further options to
#' control the table colour and row striping.
#'
#' @param style Numeric from 1 to 6 indicating which prebuilt default to use.
#' @param color One of "blue", "cyan", "pink", "green", "red", and "gray".
#' @param striping Logical indicating whether optional row striping
#'   should be enabled.
#' @param ... Forwarded to [blockr.core::new_block()]
#'
#' @return A styled gt block object that can be used with the serve function.
#'
#' @examples
#' \dontrun{
#' serve(new_style_gt_block(), data = list(gt_obj = gt::gt(head(mtcars))))
#' }
#'
#' @export
new_style_gt_block <- function(
  style = 1,
  color = "gray",
  striping = "yes",
  ...
) {
  if (style < 1 || style > 6) stop("`style` must be a value from 1-6")

  color_choices <- c("blue", "cyan", "pink", "green", "red", "gray")
  match.arg(color, color_choices)

  striping_choices <- c("yes", "no")
  match.arg(striping, striping_choices)

  ui <- function(id) {
    tagList(
      numericInput(
        NS(id, "style"),
        label = "Choose a default style from 1 to 6:",
        value = style,
        min = 1,
        max = 6,
        step = 1
      ),
      selectInput(
        NS(id, "color"),
        label = "Select color:",
        choices = color_choices,
        selected = color
      ),
      selectInput(
        NS(id, "striping"),
        label = "Should rows be striped?",
        choices = striping_choices,
        selected = striping
      )
    )
  }

  server <- function(id, gt_obj) {
    moduleServer(id, function(input, output, session) {
      list(
        expr = reactive(
          bquote(
            gt_obj |>
              opt_stylize(
                style = .(style),
                color = .(color),
                add_row_striping = ifelse(.(striping) == "yes", TRUE, FALSE)
              ),
            list(
              style = input$style,
              color = input$color,
              striping = input$striping
            )
          )
        ),
        state = list(
          style = reactive(input$style),
          color = reactive(input$color),
          striping = reactive(input$striping)
        )
      )
    })
  }

  new_gt_block(
    ui = ui,
    server = server,
    class = "style_block",
    ...
  )
}
