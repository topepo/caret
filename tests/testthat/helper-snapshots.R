# Transforms for `expect_snapshot(transform = )`, for output that is stable in
# structure but not in every character.

# Rounded numbers can differ in their last digit across platforms (64- vs
# 80-bit long double), so numeric snapshots mask the values and keep the
# surrounding text.
mask_decimals <- function(lines) {
  gsub("[0-9]+[.][0-9]+", "<num>", lines)
}

# Printing an object that carries a formula shows the formula's environment,
# whose address changes every session.
mask_env <- function(lines) {
  gsub("<environment: [^>]+>", "<environment>", lines)
}
