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

# R 4.6.0 relabelled the missing-value count in summary() output from "NA's" to
# "NAs", so anything that prints summary(<data frame>) differs by R version.
# Rewriting one spelling as the other is not enough: the label is one character
# shorter now, and how that character is absorbed depends on the column. In a
# numeric summary the labels are padded out to the width of the widest one
# ("1st Qu."), so the difference lands between the label and the colon; where
# the column is only as wide as its contents (an all-missing logical column
# prints "Mode:logical"), the label sits against the colon and the difference
# lands in the alignment of everything after it. Normalise the label, then
# collapse the runs of spaces on the lines that carry it, so neither can leak.
mask_na_label <- function(lines) {
  lines <- gsub("NA'?s *:", "NAs:", lines)
  carries_count <- grepl("NAs:", lines, fixed = TRUE)
  lines[carries_count] <- gsub("[ ]+", " ", lines[carries_count])
  lines
}
