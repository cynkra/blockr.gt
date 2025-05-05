register_gt_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_basic_gt_block",
      "new_colour_gt_block",
      "new_save_gt_block",
      "new_spanner_gt_block",
      "new_style_gt_block"
    ),
    name = c(
      "Basic GT Table",
      "Colour GT Table",
      "Save GT Table",
      "Spanner GT Table",
      "Style GT Table"
    ),
    description = c(
      "Add a basic GT table",
      "Colour a GT table",
      "Save a GT table",
      "Add spanners to a GT table",
      "Style a GT table"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
