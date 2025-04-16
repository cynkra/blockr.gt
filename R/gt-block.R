new_gt_block <- function(server, ui, class, ctor = sys.parent(), ...) {
    new_block(server, ui, c(class, "gt_block"), ctor, ...)
}
