# Conformance reference values for the logit family.
#
# Generates the expected values that language ports of compstatslib assert
# against for `plot_logit()` / `interactive_logit()`: `glm(family = binomial)`
# coefficients, fitted values, linear predictors, deviance, AIC and iteration
# counts, the perfect-separation doc example, the degenerate edge cases, and
# the `glm.fit` internals a ported IRLS must reproduce (mustart, working
# response, IRLS weights, convergence rule).
#
# Section 3 also produces the weighted least-squares reference values, since
# `glm.fit`'s per-iteration solve is `lm.wfit` on the working response. The
# rank-deficiency and pivoting fixtures for that solver live in ols.R.
#
# Consumed by (TypeScript port, compstatslib-ts):
#   src/core/logit.test.ts
#   src/core/ols.test.ts       (section 3's weighted/unweighted lm.wfit values)
#
# Originating fixture document: logit-fixtures.md
# Verified under: R 4.5.3 (2026-03-11)
#
# Re-run with: Rscript conformance-fixtures/logit.R   (from the package root)
#
# Values print at %.17g so ports can pin bit-exact doubles. Editing this
# script invalidates the pinned values in every port.

## ===========================================================================
## Section 1 — main fixture + doc example (with per-iteration trace)
## ===========================================================================

## Compute R-verified expected values for logit (glm binomial) test fixtures
## (Slice 3: logit). Computed with R 4.5.3 via Rscript.

fmt <- function(x) sprintf("%.17g", x)
fmtv <- function(v) sapply(v, fmt)

## Call fn(...) but capture any warning message(s) without stopping execution.
with_warnings <- function(fn) {
  msgs <- character(0)
  val <- withCallingHandlers(
    fn(),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = val, warnings = msgs)
}

md_table <- function(header, rows) {
  cat("| ", paste(header, collapse = " | "), " |\n", sep = "")
  cat("| ", paste(rep("---", length(header)), collapse = " | "), " |\n", sep = "")
  for (r in rows) {
    cat("| ", paste(r, collapse = " | "), " |\n", sep = "")
  }
  cat("\n")
}

predict_grid <- c(0, 5, 10, 12.5, 25, 37.5, 45, 50)

report_glm <- function(label, x, y, grid = predict_grid) {
  cat("\n==== ", label, " ====\n\n", sep = "")
  df <- data.frame(x = x, y = y)

  res <- with_warnings(function() glm(y ~ x, data = df, family = binomial))
  regr <- res$value
  warnings_fit <- res$warnings

  cat("Warnings during fit:\n")
  if (length(warnings_fit) == 0) {
    cat("- (none)\n")
  } else {
    for (w in unique(warnings_fit)) cat("- `", w, "`\n", sep = "")
  }
  cat("\n")

  co <- coef(regr)
  cat("coefficients (named):\n")
  print(co)
  cat("coefficients (unname, full precision):\n")
  cat("- intercept:", fmt(unname(co[1])), "\n")
  cat("- slope:", fmt(unname(co[2])), "\n\n")

  cat("fitted.values (full precision), in input order:\n\n")
  md_table(c("i", "x", "y", "fitted.values"), lapply(seq_along(x), function(i) {
    c(i, fmt(x[i]), fmt(y[i]), fmt(regr$fitted.values[[i]]))
  }))

  cat("linear.predictors (full precision), in input order:\n\n")
  md_table(c("i", "x", "linear.predictors"), lapply(seq_along(x), function(i) {
    c(i, fmt(x[i]), fmt(regr$linear.predictors[[i]]))
  }))

  cat("Deviance / AIC / df:\n\n")
  md_table(c("quantity", "value"), list(
    c("null.deviance", fmt(regr$null.deviance)),
    c("deviance", fmt(regr$deviance)),
    c("aic", fmt(regr$aic)),
    c("df.null", fmt(regr$df.null)),
    c("df.residual", fmt(regr$df.residual)),
    c("iter", fmt(regr$iter)),
    c("converged", as.character(regr$converged)),
    c("boundary", as.character(regr$boundary))
  ))

  s <- summary(regr)
  cat("summary(regr)$aic:", fmt(s$aic), "\n\n")

  cat("Coefficient table (Estimate, Std. Error, z value, Pr(>|z|)):\n\n")
  ct <- s$coefficients
  md_table(c("term", "Estimate", "Std. Error", "z value", "Pr(>|z|)"),
            lapply(seq_len(nrow(ct)), function(i) {
    c(rownames(ct)[i], fmt(ct[i, 1]), fmt(ct[i, 2]), fmt(ct[i, 3]), fmt(ct[i, 4]))
  }))

  cat("Predicted response (type = \"response\") at grid x:\n\n")
  newdata <- data.frame(x = grid)
  predres <- with_warnings(function() predict(regr, newdata, type = "response"))
  pred <- predres$value
  if (length(predres$warnings) > 0) {
    cat("Warnings during predict:\n")
    for (w in unique(predres$warnings)) cat("- `", w, "`\n", sep = "")
    cat("\n")
  }
  md_table(c("x", "predicted response"), lapply(seq_along(grid), function(i) {
    c(fmt(grid[i]), fmt(pred[[i]]))
  }))

  cat("plot_logit-style rounded stats strings:\n\n")
  cat("- `round(coef[1], 2)` =", as.character(round(co[[1]], 2)), "\n")
  cat("- `round(coef[2], 2)` =", as.character(round(co[[2]], 2)), "\n")
  cat("- `round(summary(regr)$aic, 2)` =", as.character(round(s$aic, 2)), "\n")

  invisible(regr)
}

cat("=== SECTION 1: MAIN FIXTURE (non-separated) ===\n")
x_main <- c(2, 8, 11, 15, 20, 24, 29, 33, 38, 44, 47, 50)
y_main <- c(0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1)
regr_main <- report_glm("Main fixture (non-separated)", x_main, y_main)

cat("\n\n=== SECTION 2: DOC EXAMPLE (perfect separation) ===\n")
x_sep <- c(-6, -3, 1, 3, 5, 8)
y_sep <- c(0, 0, 0, 1, 1, 1)
regr_sep <- report_glm("Doc example (perfect separation)", x_sep, y_sep, grid = c(-6, -3, 0, 1, 3, 5, 8))

cat("\n\n=== SECTION 2b: separated example, per-iteration trace ===\n")
df_sep <- data.frame(x = x_sep, y = y_sep)
trace_res <- with_warnings(function() {
  capture.output(
    glm(y ~ x, data = df_sep, family = binomial, control = glm.control(trace = TRUE)),
    type = "output"
  )
})
cat("Trace lines (captured stdout from control=glm.control(trace=TRUE)):\n\n")
cat("```text\n")
for (line in trace_res$value) cat(line, "\n")
cat("```\n\n")
cat("Warnings during traced fit:\n")
if (length(trace_res$warnings) == 0) cat("- (none)\n") else for (w in unique(trace_res$warnings)) cat("- `", w, "`\n", sep = "")

cat("\n\n=== SECTION 2c: main fixture, per-iteration trace ===\n")
df_main <- data.frame(x = x_main, y = y_main)
trace_main <- with_warnings(function() {
  capture.output(
    glm(y ~ x, data = df_main, family = binomial, control = glm.control(trace = TRUE)),
    type = "output"
  )
})
cat("Trace lines:\n\n")
cat("```text\n")
for (line in trace_main$value) cat(line, "\n")
cat("```\n\n")
cat("Warnings during traced fit:\n")
if (length(trace_main$warnings) == 0) cat("- (none)\n") else for (w in unique(trace_main$warnings)) cat("- `", w, "`\n", sep = "")

## ===========================================================================
## Section 2 — edge cases
## ===========================================================================

cat("\n\n########## EDGE CASES ##########\n")

## Edge-case behavior for glm(family = binomial) — Slice 3 (logit) fixtures.

fmt <- function(x) sprintf("%.17g", x)

with_warnings <- function(fn) {
  msgs <- character(0)
  val <- withCallingHandlers(
    fn(),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = val, warnings = msgs)
}

try_report <- function(label, expr_fn) {
  cat("\n---- ", label, " ----\n", sep = "")
  out <- tryCatch({
    with_warnings(expr_fn)
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    NULL
  })
  if (!is.null(out)) {
    if (length(out$warnings) > 0) {
      cat("Warnings:\n")
      for (w in unique(out$warnings)) cat("- `", w, "`\n", sep = "")
    } else {
      cat("Warnings: (none)\n")
    }
    cat("Value:\n")
    print(out$value)
  }
}

cat("=== EDGE CASE: 0 rows ===\n")
cat("plot_logit() short-circuits at `nrow(points) == 0` before calling glm ")
cat("(see R/logit_plot.R: `if (nrow(points) == 0) { plot_points_logit(NA, min_x, max_x); return() }`).\n")
cat("Still, record what glm() does when called directly on 0 rows:\n\n")
try_report("glm(y ~ x, data = data.frame(x = numeric(0), y = numeric(0)), family = binomial)", function() {
  glm(y ~ x, data = data.frame(x = numeric(0), y = numeric(0)), family = binomial)
})

cat("\n\n=== EDGE CASE: 1 point ===\n")
cat("plot_logit() short-circuits at `nrow(points) < 2` before fitting glm ")
cat("(after plotting the single point). Still, record what glm() alone does on 1 row:\n\n")
try_report("glm(y ~ x, data = data.frame(x = 10, y = 1), family = binomial)", function() {
  glm(y ~ x, data = data.frame(x = 10, y = 1), family = binomial)
})

cat("\n\n=== EDGE CASE: 2 points, one of each class (separated by construction) ===\n")
try_report("glm(y ~ x, data.frame(x=c(10,30), y=c(0,1)), family=binomial)", function() {
  regr <- glm(y ~ x, data = data.frame(x = c(10, 30), y = c(0, 1)), family = binomial)
  cat("coefficients:\n"); print(coef(regr))
  cat("coefficients full precision: intercept=", fmt(unname(coef(regr)[1])),
      " slope=", fmt(unname(coef(regr)[2])), "\n", sep = "")
  cat("fitted.values:\n"); print(fmt(regr$fitted.values))
  cat("deviance:", fmt(regr$deviance), " null.deviance:", fmt(regr$null.deviance), "\n")
  cat("aic:", fmt(regr$aic), "\n")
  cat("iter:", regr$iter, " converged:", regr$converged, " boundary:", regr$boundary, "\n")
  regr
})

cat("\n\n=== EDGE CASE: all y = 0 ===\n")
try_report("glm(y ~ x, data.frame(x=c(5,15,25,35), y=0), family=binomial)", function() {
  regr <- glm(y ~ x, data = data.frame(x = c(5, 15, 25, 35), y = 0), family = binomial)
  cat("coefficients:\n"); print(coef(regr))
  cat("fitted.values:\n"); print(fmt(regr$fitted.values))
  cat("deviance:", fmt(regr$deviance), " null.deviance:", fmt(regr$null.deviance), "\n")
  cat("aic:", fmt(regr$aic), "\n")
  cat("iter:", regr$iter, " converged:", regr$converged, " boundary:", regr$boundary, "\n")
  regr
})

cat("\n\n=== EDGE CASE: all y = 1 ===\n")
try_report("glm(y ~ x, data.frame(x=c(5,15,25,35), y=1), family=binomial)", function() {
  regr <- glm(y ~ x, data = data.frame(x = c(5, 15, 25, 35), y = 1), family = binomial)
  cat("coefficients:\n"); print(coef(regr))
  cat("fitted.values:\n"); print(fmt(regr$fitted.values))
  cat("deviance:", fmt(regr$deviance), " null.deviance:", fmt(regr$null.deviance), "\n")
  cat("aic:", fmt(regr$aic), "\n")
  cat("iter:", regr$iter, " converged:", regr$converged, " boundary:", regr$boundary, "\n")
  regr
})

cat("\n\n=== EDGE CASE: constant x ===\n")
try_report("glm(y ~ x, data.frame(x=20, y=c(0,1,0,1)), family=binomial)", function() {
  regr <- glm(y ~ x, data = data.frame(x = 20, y = c(0, 1, 0, 1)), family = binomial)
  cat("coefficients:\n"); print(coef(regr))
  cat("coefficients (raw, includes NA if aliased):\n")
  print(regr$coefficients)
  cat("fitted.values:\n"); print(fmt(regr$fitted.values))
  cat("deviance:", fmt(regr$deviance), " null.deviance:", fmt(regr$null.deviance), "\n")
  cat("aic:", fmt(regr$aic), "\n")
  cat("iter:", regr$iter, " converged:", regr$converged, " boundary:", regr$boundary, "\n")
  cat("rank of fit (via summary):\n")
  s <- summary(regr)
  print(s)
  regr
})

## ===========================================================================
## Section 3 — glm.fit internals demonstration + weighted OLS fixtures
## ===========================================================================

cat("\n\n########## GLM.FIT INTERNALS + WEIGHTED OLS ##########\n")

## glm.fit internals demonstration + weighted-OLS fixtures for core/ols.ts.

fmt <- function(x) sprintf("%.17g", x)

cat("=== glm.fit internals: mustart demonstration (main fixture) ===\n\n")
x_main <- c(2, 8, 11, 15, 20, 24, 29, 33, 38, 44, 47, 50)
y_main <- c(0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1)
w <- rep(1, length(y_main))
mustart <- (w * y_main + 0.5) / (w + 1)
cat("mustart = (weights * y + 0.5) / (weights + 1), weights = 1:\n")
cat("y:       ", paste(y_main, collapse = ", "), "\n")
cat("mustart: ", paste(fmt(mustart), collapse = ", "), "\n\n")

eta0 <- log(mustart / (1 - mustart))
cat("eta0 = logit(mustart) = log(mustart/(1-mustart)):\n")
cat(paste(fmt(eta0), collapse = ", "), "\n\n")
cat("Note: since mustart only takes two values (0.25 for y=0, 0.75 for y=1)\n")
cat("with weights=1, eta0 only takes two values too:\n")
cat("- y=0 -> mustart=0.25 -> eta0 =", fmt(log(0.25/0.75)), "\n")
cat("- y=1 -> mustart=0.75 -> eta0 =", fmt(log(0.75/0.25)), "\n\n")

cat("=== glm.fit internals: first-iteration IRLS quantities (main fixture) ===\n\n")
## Reproduce iteration 1 of glm.fit's IRLS loop by hand, starting from eta0/mustart.
mu <- mustart
eta <- eta0
mu_eta_val <- mu * (1 - mu)          # d(mu)/d(eta) for logit link
varmu <- mu * (1 - mu)               # binomial variance function
z <- eta + (y_main - mu) / mu_eta_val
wts <- sqrt(mu_eta_val^2 / varmu)    # = sqrt(mu*(1-mu)) since weights=1

cat("Working response z = eta + (y - mu) / mu.eta(eta):\n")
cat(paste(fmt(z), collapse = ", "), "\n\n")
cat("IRLS weights w = sqrt((weights * mu.eta(eta)^2) / variance(mu)):\n")
cat(paste(fmt(wts), collapse = ", "), "\n\n")

## Solve the weighted least squares step: lm.wfit on (1, x) with response z,
## weights = wts^2 (since w above is already sqrt-weights per glm.fit's own
## convention, feeding to lm.wfit needs the *squared* weights).
X <- cbind(1, x_main)
step1 <- lm.wfit(X, z, wts^2)
cat("Step-1 coefficients (should roughly match, but not equal, the final fit):\n")
print(step1$coefficients)
cat(fmt(step1$coefficients[1]), ",", fmt(step1$coefficients[2]), "\n\n")

cat("=== glm.control() defaults ===\n\n")
print(glm.control())

cat("\n=== binomial()$aic formula demonstration (main fixture, final fit) ===\n\n")
regr <- suppressWarnings(glm(y ~ x, data = data.frame(x = x_main, y = y_main), family = binomial))
mu_final <- regr$fitted.values
wt <- rep(1, length(y_main))
aic_manual <- -2 * sum(dbinom(round(1 * y_main), round(1), mu_final, log = TRUE)) + 2 * 2
cat("Manual AIC via binomial()$aic formula (-2*sum(dbinom(...,log=TRUE)) + 2*rank):\n")
cat(fmt(aic_manual), "\n")
cat("regr$aic (should match):", fmt(regr$aic), "\n\n")

cat("=== WEIGHTED OLS FIXTURES (core/ols.ts) ===\n\n")

cat("---- Unweighted: X = cbind(1, x), x = c(1,3,5,8), y = c(2,4,6,8) ----\n\n")
x_ols <- c(1, 3, 5, 8)
y_ols <- c(2, 4, 6, 8)
X_ols <- cbind(1, x_ols)
fit_uw <- lm.wfit(X_ols, y_ols, rep(1, 4))
cat("coefficients:\n")
print(fit_uw$coefficients)
cat("intercept:", fmt(fit_uw$coefficients[1]), " slope:", fmt(fit_uw$coefficients[2]), "\n")
cat("fitted.values:\n")
print(fmt(fit_uw$fitted.values))
cat("residuals:\n")
print(fmt(fit_uw$residuals))

cat("\n---- Weighted: same X, y; weights w = c(0.5, 2, 1, 4) ----\n\n")
w_ols <- c(0.5, 2, 1, 4)
fit_w <- lm.wfit(X_ols, y_ols, w_ols)
cat("coefficients:\n")
print(fit_w$coefficients)
cat("intercept:", fmt(fit_w$coefficients[1]), " slope:", fmt(fit_w$coefficients[2]), "\n")
cat("fitted.values:\n")
print(fmt(fit_w$fitted.values))
cat("residuals:\n")
print(fmt(fit_w$residuals))

## Cross-check against lm(..., weights=) which wraps lm.wfit the same way.
cat("\n---- Cross-check via lm(y ~ x, weights = w) ----\n\n")
lm_w <- lm(y_ols ~ x_ols, weights = w_ols)
print(coef(lm_w))
cat(fmt(coef(lm_w)[[1]]), ",", fmt(coef(lm_w)[[2]]), "\n")

## ===========================================================================
## Section 4 — edge cases at full precision, and the link functions R uses
##
## From the header of src/core/logit.test.ts in the TypeScript port: this ran
## for that task, to raise the edge cases above to full precision (the earlier
## sections print them through R's default 7-digit `print()`) and to pin the
## parts of `glm.fit` the fixture document only describes in prose. The
## step-1 solve goes through R's own `C_Cdqrls`, so it differs in the last two
## digits from Section 3's hand-built `mu * (1 - mu)` derivation — `mu.eta` is
## `exp(eta) / (1 + exp(eta))^2`, not `mu * (1 - mu)`.
## ===========================================================================

cat("\n\n########## EDGE CASES AT FULL PRECISION + LINK FUNCTIONS ##########\n\n")

fmt <- function(x) if (is.na(x)) "NA" else sprintf("%.17g", x)

## Link functions, read off binomial() itself rather than assumed.
fam <- binomial()
for (e in c(-40, -30.5, -30, 0, 30, 30.5, 40)) {
  cat(fmt(e), fmt(fam$linkinv(e)), fmt(fam$mu.eta(e)), "\n")
}

report <- function(label, x, y) {
  r <- suppressWarnings(glm(y ~ x, data = data.frame(x = x, y = y),
                            family = binomial))
  cat(label, fmt(coef(r)[1]), fmt(coef(r)[2]), fmt(r$deviance),
      fmt(r$null.deviance), fmt(r$aic), r$rank, r$iter, r$converged, "\n")
}
report("1 point", 10, 1)
report("2 separated", c(10, 30), c(0, 1))
report("all y = 0", c(5, 15, 25, 35), c(0, 0, 0, 0))
report("all y = 1", c(5, 15, 25, 35), c(1, 1, 1, 1))
report("constant x", c(20, 20, 20, 20), c(0, 1, 0, 1))
report("2 same class", c(10, 30), c(1, 1))

## One IRLS step, through R's own Cdqrls, from the binomial starting values.
x <- c(2, 8, 11, 15, 20, 24, 29, 33, 38, 44, 47, 50)
y <- c(0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1)
mu <- (y + 0.5) / 2
eta <- log(mu / (1 - mu))
mev <- fam$mu.eta(eta)
z <- eta + (y - mu) / mev
w <- sqrt(mev^2 / (mu * (1 - mu)))
print(sapply(.Call(stats:::C_Cdqrls, cbind(1, x) * w, z * w, 1e-11, FALSE)$coefficients, fmt))
## -1.9593006849236529, 0.07324488541770667
