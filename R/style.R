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
        NS(id, "add_row_striping"),
        label = "Should rows be striped?",
        choices = c("yes", "no"),
        selected = "yes"
      )
    )
  }

  server <- function(id, gt_obj) {
    moduleServer(id, function(input, output, session) {
      list(
        expr = list(),
        state = list()
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
