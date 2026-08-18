# Stand-ins for the objects that a few internal helpers in R/misc.R summarise.
# The real ones come from packages caret only suggests (or does not depend on at
# all), and every helper here reads just a handful of fields, so a fixture that
# matches those fields exercises the same code.
#
# The methods are registered explicitly: the helpers call `summary()` and
# `print()` from inside caret's namespace, which is not where a test file's
# S3 methods would be found.

# `varSeq()` reads summary(x)$which from a leaps::regsubsets object: one row per
# subset size, one logical column per term, the intercept included.
fake_regsubsets <- function() {
  which <- rbind(
    c(TRUE, TRUE, FALSE, FALSE),
    c(TRUE, TRUE, TRUE, FALSE),
    c(TRUE, TRUE, TRUE, TRUE)
  )
  dimnames(which) <- list(
    as.character(1:3),
    c("(Intercept)", "x1", "x2", "x3")
  )
  structure(list(which = which), class = "fake_regsubsets")
}

summary.fake_regsubsets <- function(object, ...) {
  list(which = object$which)
}

registerS3method("summary", "fake_regsubsets", summary.fake_regsubsets)

# `partRuleSummary()` reads $terms and $predictions and parses the printed
# rules, so the fixture prints in the layout RWeka::PART() uses: one indented
# condition per line, each rule ending in ": <class> (<counts>)".
fake_part_rules <- function() {
  structure(
    list(
      terms = terms(Species ~ Petal.Width + Petal.Length, data = iris),
      predictions = factor(
        c("setosa", "versicolor", "virginica"),
        levels = c("setosa", "versicolor", "virginica")
      )
    ),
    class = "fake_part_rules"
  )
}

print.fake_part_rules <- function(x, ...) {
  cat("PART decision list\n------------------\n\n")
  cat("Petal.Width <= 0.6: setosa (50.0)\n\n")
  cat("Petal.Width <= 1.7 AND\n")
  cat("Petal.Length <= 4.9: versicolor (48.0/1.0)\n\n")
  cat(": virginica (52.0/3.0)\n\n")
  cat("Number of Rules  : \t3\n\n")
  invisible(x)
}

registerS3method("print", "fake_part_rules", print.fake_part_rules)

# `ripperRuleSummary()` parses RWeka::JRip() output instead: parenthesised
# conditions, and the predicted class written as "<outcome>=<class>". It also
# drops everything up to the first blank line, so the header matters.
fake_ripper_rules <- function() {
  structure(
    list(
      terms = terms(Species ~ Petal.Width + Petal.Length, data = iris),
      predictions = factor(
        c("setosa", "versicolor", "virginica"),
        levels = c("setosa", "versicolor", "virginica")
      )
    ),
    class = "fake_ripper_rules"
  )
}

print.fake_ripper_rules <- function(x, ...) {
  cat("JRIP rules:\n===========\n\n")
  cat("(Petal.Width <= 0.6) => Species=setosa (50.0/0.0)\n")
  cat(
    "(Petal.Length <= 4.9) and (Petal.Width <= 1.6)",
    "=> Species=versicolor (47.0/1.0)\n"
  )
  cat(" => Species=virginica (53.0/3.0)\n\n")
  cat("Number of Rules : 3\n\n")
  invisible(x)
}

registerS3method("print", "fake_ripper_rules", print.fake_ripper_rules)

# Fake fitted objects for the get_resample_perf() methods, which only read the
# resampled results, the chosen settings and the control object.
fake_resample_perf_obj <- function(class, return_resamp = "final") {
  perf_names <- c("RMSE", "Rsquared")
  resample <- data.frame(
    RMSE = c(1, 2, 3),
    Rsquared = c(0.5, 0.6, 0.7),
    Resample = paste0("Fold", 1:3)
  )
  out <- list(
    control = list(returnResamp = return_resamp),
    perfNames = perf_names
  )
  if (class == "train") {
    out$resample <- cbind(resample, k = 5)
    out$bestTune <- data.frame(k = 5)
  }
  if (class == "rfe") {
    out$resample <- cbind(resample, Variables = rep(c(2, 4), length.out = 3))
    out$bestSubset <- 2
  }
  if (class == "sbf") {
    out$resample <- resample
  }
  if (class %in% c("safs", "gafs")) {
    out$external <- cbind(resample, Iter = rep(c(1, 2), length.out = 3))
    out$optIter <- 1
  }
  structure(out, class = class)
}
