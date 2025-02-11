#' Save a gt table to multiple formats
#'
#' This block takes a gt table object and saves it to file in the specified
#' format(s). Supports HTML, PDF, and PNG output formats.
#'
#' @param formats Vector of formats to save ("html", "pdf", "png")
#' @param filename Base filename for the output (without extension)
#' @param expand Numeric, expansion factor for PNG output (default 10)
#'
#' @return Invisibly returns NULL, called for side effects
#' @export
new_save_gt_block <- function(format = character(), filename = character(), expand = numeric()) {
  ui <- function(id) {
    tagList(
      selectInput(
        NS(id, "format"),
        label = "File format",
        choices = c("pdf", "html", "png")
      ),
      textInput(
        NS(id, "filename"),
        label = "Filename, without extension"
      ),
      numericInput(
        NS(id, "expand"),
        label = "PNG files are often Cropped. Set whitespace with an expansion factor."
      )
    )
  }

  server <- function(id, gt_obj) {
    moduleServer(id, function(input, output, session) {
      format <- reactiveVal(format)
      filename <- reactiveVal(filename)
      expand <- reactiveVal(expand)

      observeEvent(input$format, format(input$format))
      observeEvent(input$filename, filename(input$filename))
      observeEvent(input$expand, expand(input$expand))

      # Alter gtsave call depending upon input format
      # gtsave(gt_obj, filename = "gt-table.html", inline_css = TRUE)
      # gtsave(gt_obj, filename = "gt-table.pdf")
      # gtsave(gt_obj, filename = "gt-table.png", expand = 10)

      # Make call to shiny::downloadHandler() to manage file download. An action
      # button should be added to the UI to trigger the download.

      list(
        expr = "create expression here",
        state = list(
          format = "reactive format variable goes here"
        )
      )
    })
  }

  new_block(
    ui = ui,
    server = server,
    class = "save_gt_block",
    ctor = "new_complete_gt_block",
    ctor_pkg = "blockr.gt"
  )
}
