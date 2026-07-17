# Shared test data for test-maxDissim.R

# One "base" point at the origin plus a pool of candidate points along the
# x-axis at known distances (1, 5, 10), so the maximum-dissimilarity choices
# are deterministic and can be worked out by hand.
maxdiss_base <- matrix(c(0, 0), nrow = 1)
maxdiss_pool <- matrix(c(1, 0, 5, 0, 10, 0), ncol = 2, byrow = TRUE)
rownames(maxdiss_pool) <- c("p1", "p2", "p3")

# Distances from the base point to each pool point (p1, p2, p3):
#   as.numeric(proxy::dist(maxdiss_pool, maxdiss_base))  # 1  5  10
#
# So the first (most dissimilar) pick is always p3 (index 3), the farthest:
#   maxDissim(maxdiss_base, maxdiss_pool, n = 1)  # 3
#
# For the second pick the objective matters. Distances to the selected set
# {base, p3} for the two remaining candidates are:
#   p1 (x = 1): to base = 1,  to p3 = 9  ->  min = 1,  sum = 10
#   p2 (x = 5): to base = 5,  to p3 = 5  ->  min = 5,  sum = 10
# maxDissim keeps the candidate with the *largest* objective value:
#   - minDiss (default): p2 wins (min 5 > min 1)
#       maxDissim(maxdiss_base, maxdiss_pool, n = 2)               # 3 2
#   - sumDiss: p1 and p2 tie at 10, so the first (p1) is chosen
#       maxDissim(maxdiss_base, maxdiss_pool, n = 2, obj = sumDiss) # 3 1

# A small numeric matrix for the splitter()/splitByDissim() helpers
maxdiss_split_x <- matrix(as.numeric(1:20), ncol = 2)
