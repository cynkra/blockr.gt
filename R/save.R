#' Save a gt table to multiple formats
#'
#' This block takes a gt table object and saves it to file in the specified
#' format(s). Supports HTML, PDF, and PNG output formats.
#'
#' @param format Vector of formats to save ("html", "pdf", "png")
#' @param expand Numeric, expansion factor for PNG output (default 10)
#'
#' @return Invisibly returns NULL, called for side effects
#'
#' @examples
#' \dontrun{
#' serve(new_save_gt_block(), data = list(gt_obj = gt::gt(mtcars)))
#' }
#'
#' @export
new_save_gt_block <- function(format = character(), expand = numeric()) {
  ui <- function(id) {
    tagList(
      selectInput(
        NS(id, "format"),
        label = "File format",
        choices = c("pdf", "html", "png")
      ),
      numericInput(
        NS(id, "expand"),
        label = "Expansion factor to prevent cropping (png only)",
        value = 10,
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
      format <- reactiveVal(format)
      expand <- reactiveVal(expand)

      observeEvent(input$format, format(input$format))
      observeEvent(input$expand, expand(input$expand))

      output$download <- downloadHandler(
        filename = reactive(paste0("gt-table", ".", format())),
        content = \(file) {
          switch(
            format(),
            pdf = gtsave(gt_obj(), filename = file),
            html = gtsave(gt_obj(), filename = file, inline_css = TRUE),
            png = gtsave(gt_obj(), filename = file, expand = expand())
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
              format = format(),
              expand = expand()
            )
          )
        ) |>
          bindEvent(output$download),
        state = list(
          format = reactive(format()),
          expand = reactive(expand())
        )
      )
    })
  }

  new_gt_block(
    ui = ui,
    server = server,
    class = "save_block"
  )
}
