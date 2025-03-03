register_gt_blocks <- function() {
  blockr.core::register_blocks(
    ctor = c(
      "basic_gt_block",
      "save_gt_block"
    ),
    name = c(
      "Basic GT Table",
      "Save GT Table"
    ),
    description = c(
      "Add a basic GT table",
      "Save a GT table"
    ),
    overwrite = TRUE
  )
}
