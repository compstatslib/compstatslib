# Conformance reference values for `optim(method = "BFGS")`.
#
# This package exports no optimizer: base R provides `optim()`, and a port to
# a language with no such standard library has to write one. This script is
# the bar that routine has to meet. The TypeScript port's `optim` is the first
# consumer.
#
# R's BFGS is Nash's algorithm 21 with Nash's line search and a relative
# tolerance on the function value. A port that uses another line search or
# another stopping rule reaches the same optimum by a different path, so it
# matches `par` and `value` to a stated tolerance and reports its own counts.
# The counts and the iterate path here are R's, recorded for reference rather
# than as a bar.
#
# Consumed by (TypeScript port, compstatslib-ts):
#   src/core/optim.test.ts
#
# Originating fixture document: optim-fixtures.md
# Verified under: R 4.5.3 (2026-03-11), arm64 macOS.
#
# Re-run with: Rscript conformance-fixtures/optim.R   (from the package root)
#
# Values print at %.17g so ports can pin bit-exact doubles. Editing this
# script invalidates the pinned values in every port. Add sections; do not
# change existing ones.

fmt <- function(x) if (is.na(x)) "NA" else sprintf("%.17g", x)
fmtv <- function(v) paste(sapply(v, fmt), collapse = ", ")

## Print one `optim()` return value: the fields a port reproduces (`par`,
## `value`, `convergence`, `message`) and the two counts R keeps.
report_optim <- function(label, result) {
  cat("\n---- ", label, " ----\n", sep = "")
  cat("par: ", fmtv(result$par), "\n", sep = "")
  cat("value: ", fmt(result$value), "\n", sep = "")
  cnt <- result$counts
  cat("counts function: ", if (is.na(cnt[["function"]])) "NA" else as.integer(cnt[["function"]]), "\n", sep = "")
  cat("counts gradient: ", if (is.na(cnt[["gradient"]])) "NA" else as.integer(cnt[["gradient"]]), "\n", sep = "")
  cat("convergence: ", as.integer(result$convergence), "\n", sep = "")
  cat("message: ", if (is.null(result$message)) "NULL" else result$message, "\n", sep = "")
}

## Errors and warnings print their condition message, so a port can follow
## the wording where it refuses the same input.
report_condition <- function(label, expr) {
  cat("\n---- ", label, " ----\n", sep = "")
  result <- tryCatch(
    withCallingHandlers(expr, warning = function(w) {
      cat("warning: ", conditionMessage(w), "\n", sep = "")
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      cat("error: ", conditionMessage(e), "\n", sep = "")
      NULL
    }
  )
  invisible(result)
}

## ===========================================================================
## Section 1 — Rosenbrock, the standard example from ?optim
## ===========================================================================
##
## The banana valley. The minimum is 0 at c(1, 1). The start c(-1.2, 1) is
## the one ?optim uses. Four calls: the analytic gradient, R's finite
## differences, an iteration cap, and a tighter relative tolerance.

cat("\n==== Section 1: Rosenbrock ====\n")

fr <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}

grr <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
    200 * (x2 - x1 * x1))
}

cat("start: ", fmtv(c(-1.2, 1)), "\n", sep = "")
cat("fr(start): ", fmt(fr(c(-1.2, 1))), "\n", sep = "")
cat("grr(start): ", fmtv(grr(c(-1.2, 1))), "\n", sep = "")
cat("exact minimizer: ", fmtv(c(1, 1)), "  exact minimum: ", fmt(0), "\n", sep = "")

## 1a. With the analytic gradient.
report_optim("1a optim(c(-1.2, 1), fr, grr, method = \"BFGS\")",
             optim(c(-1.2, 1), fr, grr, method = "BFGS"))

## 1b. Without a gradient. R takes central finite differences with a step of
## ndeps = 1e-3 per parameter, so the answer differs from 1a in the low digits
## and the function count is far higher.
report_optim("1b optim(c(-1.2, 1), fr, method = \"BFGS\") no gr",
             optim(c(-1.2, 1), fr, method = "BFGS"))
cat("1b default ndeps: ", fmt(1e-3), "\n", sep = "")

## 1c. Five iterations only. R stops on the cap and reports convergence 1.
report_optim("1c optim(c(-1.2, 1), fr, grr, method = \"BFGS\", maxit = 5)",
             optim(c(-1.2, 1), fr, grr, method = "BFGS", control = list(maxit = 5)))

## 1d. A tighter relative tolerance than the default 1e-8.
report_optim("1d optim(c(-1.2, 1), fr, grr, method = \"BFGS\", reltol = 1e-12)",
             optim(c(-1.2, 1), fr, grr, method = "BFGS", control = list(reltol = 1e-12)))

## ===========================================================================
## Section 2 — a convex quadratic
## ===========================================================================
##
## One minimum, reached in a few steps from any start. The cross term makes
## the Hessian non-diagonal, so a port that ignores it stops at the wrong
## point. The exact minimizer is c(1, -2) and the exact minimum is 0.

cat("\n==== Section 2: convex quadratic ====\n")

fq <- function(p) {
  2 * (p[1] - 1)^2 + (p[2] + 2)^2 + 0.5 * (p[1] - 1) * (p[2] + 2)
}

gq <- function(p) {
  c(4 * (p[1] - 1) + 0.5 * (p[2] + 2),
    2 * (p[2] + 2) + 0.5 * (p[1] - 1))
}

cat("start: ", fmtv(c(5, -3)), "\n", sep = "")
cat("fq(start): ", fmt(fq(c(5, -3))), "\n", sep = "")
cat("gq(start): ", fmtv(gq(c(5, -3))), "\n", sep = "")
cat("exact minimizer: ", fmtv(c(1, -2)), "  exact minimum: ", fmt(0), "\n", sep = "")

## 2a. With the analytic gradient.
report_optim("2a optim(c(5, -3), fq, gq, method = \"BFGS\")",
             optim(c(5, -3), fq, gq, method = "BFGS"))

## 2b. Without a gradient. Central differences are exact for a quadratic up to
## rounding, so this run matches 2a bit for bit. Rosenbrock in 1b does not.
report_optim("2b optim(c(5, -3), fq, method = \"BFGS\") no gr",
             optim(c(5, -3), fq, method = "BFGS"))

## ===========================================================================
## Section 3 — a two-parameter logistic maximum-likelihood fit
## ===========================================================================
##
## Ten observations, one predictor, an intercept. The objective is the
## negative log-likelihood, so its minimum is the maximum-likelihood fit.
## `glm()` reaches the same optimum by iteratively reweighted least squares.
## The two paths agree to about 5e-5 in `par`, not bit for bit, because they
## stop on different rules. The likelihood is flat near its maximum, so the
## two negative log-likelihoods agree to about 2e-9 while the coefficients
## differ in the fifth digit. A port pins the optim values and reads the glm
## coefficients as the answer both are near.

cat("\n==== Section 3: logistic maximum likelihood ====\n")

x <- c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)
y <- c(0, 0, 0, 1, 0, 1, 0, 1, 1, 1)

nll <- function(b) {
  eta <- b[1] + b[2] * x
  sum(log1p(exp(eta))) - sum(y * eta)
}

gnll <- function(b) {
  eta <- b[1] + b[2] * x
  p <- 1 / (1 + exp(-eta))
  c(sum(p - y), sum((p - y) * x))
}

cat("x: ", fmtv(x), "\n", sep = "")
cat("y: ", fmtv(y), "\n", sep = "")
cat("start: ", fmtv(c(0, 0)), "\n", sep = "")
cat("nll(start): ", fmt(nll(c(0, 0))), "\n", sep = "")
cat("gnll(start): ", fmtv(gnll(c(0, 0))), "\n", sep = "")

## 3a. With the analytic gradient.
report_optim("3a optim(c(0, 0), nll, gnll, method = \"BFGS\")",
             optim(c(0, 0), nll, gnll, method = "BFGS"))

## 3b. Without a gradient.
report_optim("3b optim(c(0, 0), nll, method = \"BFGS\") no gr",
             optim(c(0, 0), nll, method = "BFGS"))

## 3c. The same optimum through glm's IRLS, for reference.
fit <- glm(y ~ x, family = binomial)
cat("\n---- 3c coef(glm(y ~ x, family = binomial)) ----\n")
cat("coefficients: ", fmtv(coef(fit)), "\n", sep = "")
cat("names: ", paste(names(coef(fit)), collapse = ", "), "\n", sep = "")
cat("nll at glm coefficients: ", fmt(nll(unname(coef(fit)))), "\n", sep = "")
cat("deviance / 2: ", fmt(fit$deviance / 2), "\n", sep = "")

## ===========================================================================
## Section 4 — a start already at the stationary point
## ===========================================================================
##
## The gradient is zero at the start, so R stops at once. The counts and the
## convergence code are the point of this case.

cat("\n==== Section 4: stationary start ====\n")

cat("gq(c(1, -2)): ", fmtv(gq(c(1, -2))), "\n", sep = "")
report_optim("4a optim(c(1, -2), fq, method = \"BFGS\") no gr",
             optim(c(1, -2), fq, method = "BFGS"))
report_optim("4b optim(c(1, -2), fq, gq, method = \"BFGS\")",
             optim(c(1, -2), fq, gq, method = "BFGS"))

## ===========================================================================
## Section 5 — an unknown method
## ===========================================================================
##
## R matches the method name against its own list and refuses anything else.
## A port that offers BFGS alone quotes this wording. The quotation marks in
## the message are directional, because `useFancyQuotes` is TRUE by default.

cat("\n==== Section 5: unknown method ====\n")

report_condition("5a optim(c(1, 1), fr, method = \"Nope\")",
                 optim(c(1, 1), fr, method = "Nope"))
