#' Save a gt table to multiple formats
#'
#' This block takes a gt table object and saves it to file in the specified
#' format(s). Supports HTML, PDF, and PNG output formats.
#'
#' @param format Vector of formats to save ("html", "pdf", "png")
#' @param expand Numeric, expansion factor for PNG output (default 10)
#' @param ... Forwarded to [blockr.core::new_block()]
#'
#' @return Invisibly returns NULL, called for side effects
#'
#' @examples
#' \dontrun{
#' serve(new_save_gt_block(), data = list(gt_obj = gt::gt(mtcars)))
#' }
#'
#' @export
new_save_gt_block <- function(format = "pdf", expand = 10, ...) {
  format_choices <- c("pdf", "html", "png")
  match.arg(format, format_choices)
  if (expand < 1 || expand > 100) stop("Expand must be between 1-100")

  ui <- function(id) {
    tagList(
      selectInput(
        NS(id, "format"),
        label = "File format",
        choices = format_choices,
        selected = format
      ),
      numericInput(
        NS(id, "expand"),
        label = "Expansion factor to prevent cropping (png only)",
        value = expand,
        min = 1,
        max = 100
      ),
      downloadButton(
        NS(id, "download"),
        "Download table"
      )
    )
  }

  server <- function(id, gt_obj) {
    moduleServer(id, function(input, output, session) {
      output$download <- downloadHandler(
        filename = reactive(paste0("gt-table", ".", input$format)),
        content = \(file) {
          switch(
            input$format,
            pdf = gtsave(gt_obj(), filename = file),
            html = gtsave(gt_obj(), filename = file, inline_css = TRUE),
            png = gtsave(gt_obj(), filename = file, expand = input$expand)
          )
        }
      )

      list(
        expr = reactive(
          bquote(
            {
              file <- paste0("gt-table", ".", .(format))
              switch(
                .(format),
                pdf = gtsave(gt_obj(), filename = file),
                html = gtsave(gt_obj(), filename = file, inline_css = TRUE),
                png = gtsave(gt_obj(), filename = file, expand = .(expand))
              )
            },
            list(
              format = input$format,
              expand = input$expand
            )
          )
        ) |>
          bindEvent(output$download),
        state = list(
          format = reactive(input$format),
          expand = reactive(input$expand)
        )
      )
    })
  }

  new_gt_block(
    ui = ui,
    server = server,
    class = "save_block",
    ...
  )
}
