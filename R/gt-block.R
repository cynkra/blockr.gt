#' @keywords internal
#' @noRd
new_gt_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "gt_block"), ctor, ...)
}

#' @export
block_output.gt_block <- function(x, result, session) {
  render_gt(result)
}

#' @export
block_ui.gt_block <- function(id, x, ...) {
  tagList(
    gt_output(NS(id, "result"))
  )
}
