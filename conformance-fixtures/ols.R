# Conformance reference values for the general least-squares solver.
#
# `lm.fit()` / `lm.wfit()` is not a function family of this package; it is the
# primitive underneath three of them — the regression line, every IRLS step of
# the logit fit, and the moderation surface. Its rank-deficiency, pivoting,
# zero-weight and rank-tolerance behavior therefore gets its own script rather
# than being buried in one family's fixtures. The ordinary weighted and
# unweighted fits are in logit.R section 3; the simple-regression agreement
# fixtures are in regression.R.
#
# Consumed by (TypeScript port, compstatslib-ts):
#   src/core/ols.test.ts
#
# Source: the R script in the header of src/core/ols.test.ts, which was run
# for that task and never made it into a fixture document.
# Verified under: R 4.5.3 (2026-03-11)
#
# Re-run with: Rscript conformance-fixtures/ols.R   (from the package root)
#
# Values print at %.17g so ports can pin bit-exact doubles. Editing this
# script invalidates the pinned values in every port.

## Extra R-verified fixtures for core/ols.ts (rank deficiency, pivoting,
## zero weights, tolerance sensitivity). R 4.5.3.

fmt <- function(x) if (is.na(x)) "NA" else sprintf("%.17g", x)
fmtv <- function(v) paste(sapply(v, fmt), collapse = ", ")

report <- function(label, fit) {
  cat("\n---- ", label, " ----\n", sep = "")
  cat("coefficients: ", fmtv(fit$coefficients), "\n", sep = "")
  cat("rank: ", fit$rank, "\n", sep = "")
  cat("fitted:    ", fmtv(fit$fitted.values), "\n", sep = "")
  cat("residuals: ", fmtv(fit$residuals), "\n", sep = "")
}

x <- c(1, 3, 5, 8)
y <- c(2, 4, 6, 8)

report("duplicate middle column", lm.fit(cbind(1, 1, x), y))
report("constant x", lm.fit(cbind(1, c(20, 20, 20, 20)), c(0, 1, 0, 1)))
report("n = 1, p = 2", lm.fit(matrix(c(1, 10), nrow = 1), 3))
report("zero weight", lm.wfit(cbind(1, x), y, c(1, 1, 0, 1)))
lm.fit(cbind(1, x[-3]), y[-3])$coefficients   ## same as the line above
lm.wfit(cbind(1, x), y, 10 * c(0.5, 2, 1, 4))$coefficients  ## scale free

## The same two calls at full precision (the two lines above autoprint at R's
## default 7 digits; added here so a port can pin the doubles).
cat("dropped row directly: ",
    fmtv(lm.fit(cbind(1, x[-3]), y[-3])$coefficients), "\n", sep = "")
cat("weights x 10:        ",
    fmtv(lm.wfit(cbind(1, x), y, 10 * c(0.5, 2, 1, 4))$coefficients), "\n", sep = "")

X_near <- cbind(1, c(1, 1, 1, 1 + 1e-8))
report("near collinear, tol = 1e-7", lm.fit(X_near, y, tol = 1e-7))
report("near collinear, tol = 1e-11", lm.fit(X_near, y, tol = 1e-11))

## Both calls error. They were run one at a time in the original session; the
## `try()` wrappers keep the script running so the rest of the fixtures print.
try(lm.wfit(cbind(1, x), y, c(1, -1, 1, 1)))  ## error
try(lm.fit(matrix(numeric(0), nrow = 0, ncol = 2), numeric(0)))  ## error

z <- c(2, 5, 1, 9, 4, 6)
xx <- c(1, 2, 3, 4, 5, 6)
yy <- c(3.1, 4.4, 2.2, 9.9, 5.5, 7.7)
report("moderation shaped", lm.fit(cbind(1, xx, z, xx * z), yy))
