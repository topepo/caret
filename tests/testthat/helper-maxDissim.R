# Shared test data for test-maxDissim.R

# One "base" point at the origin plus a pool of points along the x-axis at
# known distances (1, 5, 10), so the maximum-dissimilarity choices are
# deterministic and can be worked out by hand.
md_a <- matrix(c(0, 0), nrow = 1)
md_b <- matrix(c(1, 0, 5, 0, 10, 0), ncol = 2, byrow = TRUE)
rownames(md_b) <- c("p1", "p2", "p3")

# A small numeric matrix for the splitter()/splitByDissim() helpers
md_x <- matrix(as.numeric(1:20), ncol = 2)
