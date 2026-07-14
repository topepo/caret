# Shared fixtures for the collinearity tests
# (test-findLinearCombos.R and test-findCorrelation.R).

# --- findLinearCombos: numeric matrices with known linear dependencies -------
# Three columns where the third is the sum of the first two (rank 2)
flc_x1 <- c(1, 2, 3, 4)
flc_x2 <- c(2, 1, 4, 3)
flc_simple <- cbind(flc_x1, flc_x2, flc_x1 + flc_x2)

# A full-rank matrix (no linear combinations)
flc_full <- diag(3)

# Two linear combinations that require iterative removal (from the docs):
# column 1 = column 2 + column 3, and column 1 = column 4 + column 5 + column 6
flc_multi <- matrix(0, nrow = 6, ncol = 6)
flc_multi[, 1] <- 1
flc_multi[, 2] <- c(1, 1, 1, 0, 0, 0)
flc_multi[, 3] <- c(0, 0, 0, 1, 1, 1)
flc_multi[, 4] <- c(1, 0, 0, 1, 0, 0)
flc_multi[, 5] <- c(0, 1, 0, 0, 1, 0)
flc_multi[, 6] <- c(0, 0, 1, 0, 0, 1)

# --- findCorrelation: correlation matrices (from the docs) -------------------
# fmt: skip
corr_R1 <- structure(
  c(1.00, 0.86, 0.56, 0.32, 0.85,
    0.86, 1.00, 0.01, 0.74, 0.32,
    0.56, 0.01, 1.00, 0.65, 0.91,
    0.32, 0.74, 0.65, 1.00, 0.36,
    0.85, 0.32, 0.91, 0.36, 1.00),
  .Dim = c(5L, 5L)
)
colnames(corr_R1) <- rownames(corr_R1) <- paste0("x", 1:5)

# A correlation matrix with negative off-diagonal entries
corr_R2 <- diag(rep(1, 5))
corr_R2[2, 3] <- corr_R2[3, 2] <- 0.7
corr_R2[5, 3] <- corr_R2[3, 5] <- -0.7
corr_R2[4, 1] <- corr_R2[1, 4] <- -0.67
# Give column 4 an extra sub-cutoff correlation so that, of the highly
# correlated pair (1, 4), column 4 has the clearly larger mean absolute
# correlation and is the one dropped. Without this, columns 1 and 4 are
# symmetric and their mean correlations tie at the floating-point level; the
# tie then breaks differently across platforms (64- vs 80-bit long double), so
# findCorrelation removes column 4 on macOS but column 1 on Linux.
corr_R2[4, 2] <- corr_R2[2, 4] <- 0.5
