# Conformance reference values for the t-test family.
#
# Generates the expected values that language ports of compstatslib assert
# against for `plot_t_test()` / `interactive_t_test()`: the central t
# distribution (dt/pt/qt), the non-central t distribution the alternative
# curve uses, and the quantities `plot_t_test()` itself derives (beta, power,
# fill quantiles, error-matrix cells, highlighted row).
#
# Section 3 sources this package's own `t_null_plot()` / `t_alt_lines()` and
# runs them on a null graphics device, so those values come from the same code
# path `plot_t_test()` runs — not a hand re-derivation of the formulas.
#
# Consumed by (TypeScript port, compstatslib-ts):
#   src/core/tdist.test.ts
#   src/core/ttest.test.ts
#
# Originating fixture document: tdist-fixtures.md
# Verified under: R 4.5.3 (2026-03-11)
#
# Re-run with: Rscript conformance-fixtures/tdist.R   (from the package root)
#
# Values print at %.17g so ports can pin bit-exact doubles. Editing this
# script invalidates the pinned values in every port.

## Compute R-verified expected values for t-distribution test fixtures
## (Slice 2: t-test). Central t, non-central t, and plot_t_test-derived
## quantities computed exactly as R/t_statistic_plot.R does.

fmt <- function(x) sprintf("%.17g", x)

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

cat("=== SECTION 1: CENTRAL T ===\n\n")

dfs <- c(1, 2, 5, 30, 99, 300)
xs <- c(-3, -1.5, 0, 0.5, 2, 4)
ps <- c(0.001, 0.025, 0.05, 0.5, 0.95, 0.975, 0.999)

for (df in dfs) {
  cat("#### df = ", df, "\n\n", sep = "")
  rows_dtpt <- lapply(xs, function(x) {
    c(fmt(x), fmt(dt(x, df = df)), fmt(pt(x, df = df)))
  })
  md_table(c("x", "dt(x, df)", "pt(x, df)"), rows_dtpt)

  rows_qt <- lapply(ps, function(p) {
    c(fmt(p), fmt(qt(p, df = df)))
  })
  md_table(c("p", "qt(p, df)"), rows_qt)
}

cat("\n=== SECTION 2: NON-CENTRAL T ===\n\n")

## Representative (df, ncp) combos covering the interactive slider ranges.
n500 <- 500
ncp500 <- 4 / (5 / sqrt(n500))
combos <- list(
  list(label = "df = 99, ncp = 1.25", df = 99, ncp = 1.25),
  list(label = "df = 99, ncp = 0.25", df = 99, ncp = 0.25),
  list(label = "df = 9, ncp = 2.5", df = 9, ncp = 2.5),
  list(label = "df = 499, ncp = 4/(5/sqrt(500)) (diff=4, sd=5, n=500)", df = n500 - 1, ncp = ncp500),
  list(label = "df = 4, ncp = 0.5", df = 4, ncp = 0.5)
)

x_nc <- c(-2, 0, 1, 1.66, 3)
p_nc <- c(0.025, 0.5, 0.975)

for (combo in combos) {
  df <- combo$df
  ncp <- combo$ncp
  cat("#### ", combo$label, "\n\n", sep = "")
  cat("ncp (full precision): ", fmt(ncp), "\n\n", sep = "")

  all_warnings <- character(0)

  rows_dtpt <- lapply(x_nc, function(x) {
    dres <- with_warnings(function() dt(x, df = df, ncp = ncp))
    pres <- with_warnings(function() pt(x, df = df, ncp = ncp))
    all_warnings <<- c(all_warnings, dres$warnings, pres$warnings)
    c(fmt(x), fmt(dres$value), fmt(pres$value))
  })
  md_table(c("x", "dt(x, df, ncp)", "pt(x, df, ncp)"), rows_dtpt)

  rows_qt <- lapply(p_nc, function(p) {
    qres <- with_warnings(function() qt(p, df = df, ncp = ncp))
    all_warnings <<- c(all_warnings, qres$warnings)
    c(fmt(p), fmt(qres$value))
  })
  md_table(c("p", "qt(p, df, ncp)"), rows_qt)

  if (length(all_warnings) > 0) {
    cat("Warnings emitted:\n\n")
    for (w in unique(all_warnings)) {
      cat("- `", w, "`\n", sep = "")
    }
    cat("\n")
  } else {
    cat("No warnings emitted for this combo.\n\n")
  }
}

cat("\n=== SECTION 3: plot_t_test derived quantities ===\n\n")

## Source the actual R package function definitions so the derived
## quantities are computed with the exact same code path as plot_t_test(),
## not a hand re-derivation.
source("R/t_statistic_plot.R")

## Graphics calls need an open device; use a null device so nothing is
## written to disk.
pdf(NULL)

param_sets <- list(
  list(label = "diff=0.5, sd=4, n=100, alpha=0.05 (defaults)", diff = 0.5, sd = 4, n = 100, alpha = 0.05),
  list(label = "diff=2, sd=2, n=10, alpha=0.01", diff = 2, sd = 2, n = 10, alpha = 0.01),
  list(label = "diff=0.1, sd=5, n=500, alpha=0.1", diff = 0.1, sd = 5, n = 500, alpha = 0.1),
  list(label = "diff=0, sd=4, n=2, alpha=0.05 (degenerate: ncp=0, n at slider minimum)", diff = 0, sd = 4, n = 2, alpha = 0.05)
)

for (ps_ in param_sets) {
  diff <- ps_$diff
  sd <- ps_$sd
  n <- ps_$n
  alpha <- ps_$alpha

  cat("#### ", ps_$label, "\n\n", sep = "")

  df <- n - 1
  t <- diff / (sd / sqrt(n))

  ## Call the actual package functions, exactly as plot_t_test() does,
  ## capturing any warnings (interactive_t_test() wraps this in
  ## suppressWarnings()).
  res <- with_warnings(function() {
    t_null_plot(df, alpha)
    t_alt_lines(df, t, alpha)
  })
  alt_stats <- res$value

  beta <- alt_stats[1]
  power <- alt_stats[2]
  alt_stats3 <- alt_stats[3]
  alt_stats4 <- alt_stats[4]

  crit <- qt(1 - alpha, df = df)
  nc_median <- qt(0.5, df = df, ncp = t)
  nc_median_dens <- dt(nc_median, df = df, ncp = t)
  fill_alt <- qt(c(beta, 0.999), df = df, ncp = t)
  fill_null <- qt(c(1 - alpha, 0.999), df = df)

  cat("df:", fmt(df), "\n")
  cat("t (= ncp of alt distribution):", fmt(t), "\n")
  cat("null rejection quantile qt(1-alpha, df):", fmt(crit), "\n")
  cat("beta = pt(qt(1-alpha, df), df, ncp=t) [alt_stats[1]]:", fmt(beta), "\n")
  cat("power = 1 - beta [alt_stats[2]]:", fmt(power), "\n")
  cat("t_alt_lines alt_stats[3] (= qt(beta, df, ncp=t), recovers crit value):", fmt(alt_stats3), "\n")
  cat("t_alt_lines alt_stats[4] (= qt(0.5, df, ncp=t), alt median):", fmt(alt_stats4), "\n")
  cat("noncentral median qt(0.5, df, ncp=t):", fmt(nc_median), "\n")
  cat("density at noncentral median dt(median, df, ncp=t):", fmt(nc_median_dens), "\n")
  cat("fill quantiles qt(c(beta, 0.999), df, ncp=t):", fmt(fill_alt[1]), ",", fmt(fill_alt[2]), "\n")
  cat("fill quantiles qt(c(1-alpha, 0.999), df) [null]:", fmt(fill_null[1]), ",", fmt(fill_null[2]), "\n")

  ## Error matrix cell values, per plot_error_matrix() / recttext() calls
  ## in t_statistic_plot.R:
  cell_type1 <- alpha
  cell_correct_reject <- round(power, 2)
  cell_correct_fail <- 1 - alpha
  cell_type2 <- round(beta, 2)

  cat("error matrix: Type I error cell (top-left, text=alpha):", fmt(cell_type1), "\n")
  cat("error matrix: Correct! cell (top-right, text=round(power,2)):", fmt(cell_correct_reject), "\n")
  cat("error matrix: Correct! cell (bottom-left, text=1-alpha):", fmt(cell_correct_fail), "\n")
  cat("error matrix: Type II error cell (bottom-right, text=round(beta,2)):", fmt(cell_type2), "\n")

  highlight_top <- alt_stats3 < alt_stats4
  cat("highlight test alt_stats[3] < alt_stats[4]:", highlight_top, "\n")
  if (highlight_top) {
    cat("Highlighted rectangle: TOP row (xl=-5.5, yb=0.25, xr=-2.5, yt=0.375) -- spans Type I error + Correct!(reject) cells\n")
  } else {
    cat("Highlighted rectangle: BOTTOM row (xl=-5.5, yb=0.125, xr=-2.5, yt=0.25) -- spans Correct!(fail-to-reject) + Type II error cells\n")
  }

  if (length(res$warnings) > 0) {
    cat("Warnings emitted by t_null_plot/t_alt_lines:\n")
    for (w in unique(res$warnings)) cat("  - ", w, "\n", sep = "")
  } else {
    cat("No warnings emitted.\n")
  }
  cat("\n")
}

dev.off()
