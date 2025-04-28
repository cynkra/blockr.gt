#' @keywords internal
#' @noRd
new_gt_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "gt_block"), ctor, ...)
}

#' @keywords internal
#' @noRd
block_output.gt_block <- function(x, result, session) {
  gt_result <- attr(result, "gt_tbl")
  req(gt_result)
  render_gt(gt_result)
}

#' @keywords internal
#' @noRd
block_ui.gt_block <- function(id, x, ...) {
  tagList(
    gt_output(NS(id, "result"))
  )
}
