register_gt_blocks <- function() {
  blockr.core::register_blocks(
    ctor = c(
      "complete_gt_block",
      "save_gt_block"
    ),
    name = c(
      "Complete GT Table",
      "Save GT Table"
    ),
    description = c(
      "Add a complete GT table",
      "Save a GT table"
    ),
    overwrite = TRUE
  )
}
