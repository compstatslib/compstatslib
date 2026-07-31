# Conformance reference values for the simple-regression family.
#
# Generates the expected values that language ports of compstatslib assert
# against for `plot_regr()` / `interactive_regression()`: intercept, slope,
# correlation, SSR, SSE, SST, R-squared, and fitted values, plus the
# degenerate n = 1, constant-x and constant-y cases.
#
# Consumed by (TypeScript port, compstatslib-ts):
#   src/core/regression.test.ts
#   src/core/ols.test.ts       (Q6 matrix-vs-scalar agreement fixtures)
#
# Originating fixture document: regression-fixtures.md
# Verified under: R 4.5.3 (2026-03-11)
#
# Re-run with: Rscript conformance-fixtures/regression.R   (from the package root)
#
# Values print at %.17g so ports can pin bit-exact doubles. Editing this
# script invalidates the pinned values in every port.

## ---------------------------------------------------------------------------
## Section 1 — fixtures A and B, plus the n = 1 and constant-x edge cases
## ---------------------------------------------------------------------------

## Compute R-verified expected values for linear-regression test fixtures.

fmt <- function(x) sprintf("%.17g", x)

report_fixture <- function(label, x, y) {
  cat("\n==== ", label, " ====\n", sep = "")
  regr <- lm(y ~ x)
  co <- coef(regr)
  intercept <- co[["(Intercept)"]]
  slope <- co[["x"]]
  correlation <- cor(x, y)
  fitted <- regr$fitted.values
  ssr <- sum((fitted - mean(y))^2)
  sse <- sum((y - fitted)^2)
  sst <- sum((y - mean(y))^2)
  r2 <- summary(regr)$r.squared

  cat("intercept:", fmt(intercept), "\n")
  cat("slope:", fmt(slope), "\n")
  cat("cor(x, y):", fmt(correlation), "\n")
  cat("fitted.values:\n")
  print(fmt(fitted))
  cat("SSR:", fmt(ssr), "\n")
  cat("SSE:", fmt(sse), "\n")
  cat("SST:", fmt(sst), "\n")
  cat("R-squared:", fmt(r2), "\n")
}

## Fixture A: R docs example
xA <- c(1, 3, 5, 8)
yA <- c(2, 4, 6, 8)
report_fixture("Fixture A", xA, yA)

## Fixture B: noisy data
xB <- 1:10
yB <- c(3.2, 1.8, 6.5, 4.9, 8.1, 6.0, 10.3, 8.7, 12.2, 9.5)
report_fixture("Fixture B", xB, yB)

## ---- Edge cases ----
cat("\n==== Edge case: lm with a single point (n = 1) ====\n")
x1 <- c(2)
y1 <- c(5)
regr1 <- tryCatch(lm(y1 ~ x1), error = function(e) e, warning = function(w) w)
print(regr1)
if (inherits(regr1, "lm")) {
  cat("coef:\n")
  print(coef(regr1))
  cat("fitted.values:\n")
  print(regr1$fitted.values)
  cat("residuals:\n")
  print(regr1$residuals)
  s <- tryCatch(summary(regr1), error = function(e) e, warning = function(w) w)
  print(s)
}

cat("\n==== Edge case: cor() when all x values are identical ====\n")
xc <- c(4, 4, 4, 4)
yc <- c(1, 2, 3, 4)
corc <- tryCatch(cor(xc, yc), error = function(e) e, warning = function(w) w)
print(corc)

regrc <- tryCatch(lm(yc ~ xc), error = function(e) e, warning = function(w) w)
cat("lm with constant x:\n")
print(regrc)

## ---------------------------------------------------------------------------
## Section 2 — constant-y edge case (varying x, all y identical)
## ---------------------------------------------------------------------------

cat("\n\n==== Edge case: constant y ====\n")

## Verify R's behavior for a constant-y regression edge case:
## varying x, all y identical.

fmt <- function(x) sprintf("%.17g", x)

x <- c(1, 2, 3, 4)
y <- c(7, 7, 7, 7)

regr <- lm(y ~ x)
cat("coef(regr):\n")
print(coef(regr))

co <- coef(regr)
cat("intercept:", fmt(co[["(Intercept)"]]), "\n")
cat("slope:", fmt(co[["x"]]), "\n")

cat("\ncor(x, y):\n")
corxy <- tryCatch(cor(x, y), warning = function(w) { print(w); cor(x, y) })
print(corxy)

cat("\nfitted.values:\n")
print(regr$fitted.values)
cat("fitted (full precision):\n")
print(fmt(regr$fitted.values))

cat("\nsummary(regr):\n")
s <- summary(regr)
print(s)
cat("\nsummary(regr)$r.squared:", fmt(s$r.squared), "\n")

cat("\nresiduals:\n")
print(regr$residuals)
