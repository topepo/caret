# Shared train/test datasets for the test-trim-*.R files.
# (seeds match the per-test setup these replaced, so results are unchanged)

set.seed(6104)
trim_class_tr <- twoClassSim(200)
trim_class_te <- twoClassSim(200)

set.seed(2853)
trim_reg_tr <- SLC14_1(200)
trim_reg_te <- SLC14_1(200)
