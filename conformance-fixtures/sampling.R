# Conformance reference values for the sampling and sample-CI families.
#
# Generates the expected values that language ports of compstatslib assert
# against for `plot_sampling()` / `interactive_sampling()` and
# `plot_sample_ci()`: R's `density()` (bandwidth, grid and all 512 ordinates),
# `hist()` cell edges and counts, the `pretty()` edges underneath them, `sd()`
# and `mean()`, and the confidence-interval arithmetic.
#
# Nothing about the *drawn* samples can be pinned — a seeded JavaScript
# generator does not reproduce R's Mersenne Twister, and the ports do not try.
# Everything here runs on fixed literal vectors instead.
#
# Consumed by (TypeScript port, compstatslib-ts):
#   src/core/kde.test.ts
#   src/core/histogram.test.ts
#   src/core/pretty.test.ts
#   src/core/arith.test.ts
#   src/core/sampling.test.ts
#
# Originating fixture document: sampling-fixtures.md (sections 1 to 4).
# Section 5 collects the cases that were computed in session while writing the
# TypeScript tests and were pinned there rather than in the document.
# Verified under: R 4.5.3 (2026-03-11)
#
# Re-run with: Rscript conformance-fixtures/sampling.R   (from the package root)
#
# Values print at %.17g so ports can pin bit-exact doubles. Editing this
# script invalidates the pinned values in every port.

## Compute R-verified expected values for sampling/sample-CI fixtures
## (Slice 4). density(), hist()/pretty(), sd(), and plot_sample_ci()
## arithmetic computed exactly as R 4.5.3's own algorithms and as
## R/sample_ci_plot.R does.

fmt <- function(x) sprintf("%.17g", x)
fmt_vec <- function(v) paste(vapply(v, fmt, character(1)), collapse = ", ")

cat("R.version.string: ", R.version.string, "\n\n", sep = "")

## ============================================================
cat("=== SECTION 1: density() ===\n\n")

cat("--- 1a. Fixture A (small) ---\n\n")
x_small <- c(2.1, 4.5, 4.7, 5, 5.5, 6.1, 7.3, 8.8, 9.9, 12.4)
bwA <- bw.nrd0(x_small)
dA <- density(x_small)
cat("bw.nrd0(x_small) = ", fmt(bwA), "\n", sep = "")
cat("d$x[1]   = ", fmt(dA$x[1]), "\n", sep = "")
cat("d$x[512] = ", fmt(dA$x[512]), "\n", sep = "")
cat("spacing (d$x[2]-d$x[1]) = ", fmt(dA$x[2] - dA$x[1]), "\n", sep = "")
cat("length(d$x) = ", length(dA$x), "\n", sep = "")
cat("sum(d$y) = ", fmt(sum(dA$y)), "\n", sep = "")
cat("max(d$y) = ", fmt(max(dA$y)), "\n", sep = "")
cat("which.max(d$y) = ", which.max(dA$y), "\n\n", sep = "")
cat("FULL d$y (512 values, one per line, index: value):\n")
for (i in seq_along(dA$y)) {
  cat(i, ": ", fmt(dA$y[i]), "\n", sep = "")
}
cat("\n")

cat("--- 1b. Fixture B (n=100) ---\n\n")
set.seed(42)
x_big <- rnorm(100, mean = 50, sd = 10)
cat("x_big (full precision literals):\n")
cat(fmt_vec(x_big), "\n\n")
bwB <- bw.nrd0(x_big)
dB <- density(x_big)
cat("bw.nrd0(x_big) = ", fmt(bwB), "\n", sep = "")
cat("d$x[1]   = ", fmt(dB$x[1]), "\n", sep = "")
cat("d$x[512] = ", fmt(dB$x[512]), "\n", sep = "")
idx <- c(1, 2, 3, 64, 128, 200, 256, 300, 384, 448, 510, 511, 512)
for (i in idx) {
  cat("d$y[", i, "] = ", fmt(dB$y[i]), "\n", sep = "")
}
cat("sum(d$y) = ", fmt(sum(dB$y)), "\n", sep = "")
cat("max(d$y) = ", fmt(max(dB$y)), "\n", sep = "")
cat("which.max(d$y) = ", which.max(dB$y), "\n\n", sep = "")

cat("--- 1c. Pooled-matrix behavior ---\n\n")
mat_vals <- c(1.0, 5.0, 2.0, 6.0, 3.0, 7.0)
m <- matrix(mat_vals, nrow = 2, ncol = 3)
cat("matrix (byrow default = FALSE, so as.vector(m) = column-major):\n")
print(m)
cat("as.vector(m) = ", fmt_vec(as.vector(m)), "\n\n", sep = "")
d_mat <- density(m)
d_vec <- density(as.vector(m))
cat("identical(density(m)$y, density(as.vector(m))$y): ",
    identical(d_mat$y, d_vec$y), "\n", sep = "")
cat("d_mat$bw = ", fmt(d_mat$bw), "\n", sep = "")
cat("d_mat$x[1] = ", fmt(d_mat$x[1]), ", d_mat$x[512] = ", fmt(d_mat$x[512]), "\n", sep = "")
cat("d_mat$y[1] = ", fmt(d_mat$y[1]), ", d_mat$y[256] = ", fmt(d_mat$y[256]), ", d_mat$y[512] = ", fmt(d_mat$y[512]), "\n\n", sep = "")

cat("--- 1e. bw.nrd0 fallback cases ---\n\n")
bw_const <- bw.nrd0(c(5, 5, 5, 5))
cat("bw.nrd0(c(5,5,5,5)) = ", fmt(bw_const), "\n", sep = "")
cat("  check: 0.9 * 5 * 4^(-0.2) = ", fmt(0.9 * 5 * 4^(-0.2)), "\n\n", sep = "")

bw_zero <- bw.nrd0(c(0, 0, 0, 0))
cat("bw.nrd0(c(0,0,0,0)) = ", fmt(bw_zero), "\n", sep = "")
cat("  check: 0.9 * 1 * 4^(-0.2) = ", fmt(0.9 * 1 * 4^(-0.2)), "\n\n", sep = "")

res_const <- tryCatch(density(c(5, 5, 5, 5)), error = function(e) e, warning = function(w) w)
cat("density(c(5,5,5,5)) result class: ", class(res_const)[1], "\n", sep = "")
cat("  bw = ", fmt(res_const$bw), "\n", sep = "")
cat("  x[1] = ", fmt(res_const$x[1]), ", x[512] = ", fmt(res_const$x[512]), "\n", sep = "")
cat("  y[1] = ", fmt(res_const$y[1]), ", max(y) = ", fmt(max(res_const$y)), ", which.max = ", which.max(res_const$y), "\n\n", sep = "")

cat("--- 1f. Edge pins ---\n\n")
d2 <- density(c(3, 9))
cat("density(c(3,9)): bw = ", fmt(d2$bw), ", from(x[1]) = ", fmt(d2$x[1]), ", to(x[512]) = ", fmt(d2$x[512]), "\n", sep = "")
cat("  y[1] = ", fmt(d2$y[1]), ", y[256] = ", fmt(d2$y[256]), ", y[512] = ", fmt(d2$y[512]), ", sum(y) = ", fmt(sum(d2$y)), "\n\n", sep = "")

res1 <- tryCatch(density(c(7)), error = function(e) e)
cat("density(c(7)) error message: '", conditionMessage(res1), "'\n\n", sep = "")

## ============================================================
cat("=== SECTION 2: hist() breaks / pretty() ===\n\n")

th7 <- c(49.8, 50.1, 50.4, 49.6, 50.0, 50.9, 49.2)
h7 <- hist(th7, plot = FALSE)
th30 <- round(seq(48.7, 52.1, length.out = 30), 3)
h30 <- hist(th30, plot = FALSE)
boundary_vec <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)
hb <- hist(boundary_vec, plot = FALSE)
hb2 <- hist(boundary_vec, breaks = boundary_vec, plot = FALSE,
            right = TRUE, include.lowest = TRUE)

## The document's script block builds these four histograms but prints them in
## its results section; the reporting below was added so the script itself
## emits every pinned value.
report_hist <- function(label, h) {
  cat(label, "\n", sep = "")
  cat("  breaks   = ", fmt_vec(h$breaks), "\n", sep = "")
  cat("  counts   = ", paste(h$counts, collapse = ", "), "\n", sep = "")
  cat("  mids     = ", fmt_vec(h$mids), "\n", sep = "")
  cat("  equidist = ", h$equidist, "\n", sep = "")
}

cat("nclass.Sturges(th7)  = ", nclass.Sturges(th7), "\n", sep = "")
cat("nclass.Sturges(th30) = ", nclass.Sturges(th30), "\n", sep = "")
cat("nclass.Sturges(boundary_vec) = ", nclass.Sturges(boundary_vec), "\n", sep = "")
cat("th30 = ", fmt_vec(th30), "\n\n", sep = "")
report_hist("hist(th7)", h7)
report_hist("hist(th30)", h30)
report_hist("hist(boundary_vec)", hb)
report_hist("hist(boundary_vec, breaks = boundary_vec)", hb2)
report_hist("hist(c(5,5,5,5))", hist(c(5, 5, 5, 5), plot = FALSE))
report_hist("hist(c(7))", hist(c(7), plot = FALSE))

cat("\npretty() calls the hist() fixtures feed, using hist()'s own",
    "min.n = 1 override:\n", sep = " ")
cat("  pretty(range(th7), n = 4, min.n = 1)  = ",
    fmt_vec(pretty(range(th7), n = nclass.Sturges(th7), min.n = 1)), "\n", sep = "")
cat("  pretty(range(th30), n = 6, min.n = 1) = ",
    fmt_vec(pretty(range(th30), n = nclass.Sturges(th30), min.n = 1)), "\n", sep = "")
cat("  pretty(c(0, 3), n = 4, min.n = 1)     = ",
    fmt_vec(pretty(range(boundary_vec), n = nclass.Sturges(boundary_vec), min.n = 1)), "\n", sep = "")

cat("\ngeneric pretty() pins (Section 2c of the document):\n")
cat("  pretty(c(0,1))            = ", fmt_vec(pretty(c(0, 1))), "\n", sep = "")
cat("  pretty(c(-5,50))          = ", fmt_vec(pretty(c(-5, 50))), "\n", sep = "")
cat("  pretty(c(0.001,0.0042), n=5) = ", fmt_vec(pretty(c(0.001, 0.0042), n = 5)), "\n", sep = "")
cat("  pretty(c(7,7))            = ", fmt_vec(pretty(c(7, 7))), "\n", sep = "")
cat("  pretty(c(0,100), n=10)    = ", fmt_vec(pretty(c(0, 100), n = 10)), "\n", sep = "")
cat("  pretty(c(1,9), n=5)       = ", fmt_vec(pretty(c(1, 9), n = 5)), "\n", sep = "")
cat("  pretty(c(-1,1), n=5)      = ", fmt_vec(pretty(c(-1, 1), n = 5)), "\n", sep = "")
cat("  pretty(c(0,0), n=5)       = ", fmt_vec(pretty(c(0, 0), n = 5)), "\n", sep = "")
cat("  pretty(c(3.5,3.9), n=5)   = ", fmt_vec(pretty(c(3.5, 3.9), n = 5)), "\n", sep = "")

cat("\nmin.n override check: pretty(c(0,1), n) with hist()'s min.n = 1 versus",
    "pretty.default's own min.n = n %/% 3:\n")
for (n in c(2, 3, 4, 5, 9, 15)) {
  a <- pretty(c(0, 1), n = n, min.n = 1)
  b <- pretty(c(0, 1), n = n)
  cat("  n = ", n, ": identical = ", identical(a, b), " breaks = ", fmt_vec(a), "\n", sep = "")
}

## ============================================================
cat("\n=== SECTION 3: sd() / mean() pins ===\n\n")

## The document's script block computes these in its results table; the three
## vectors and their statistics are spelled out here so the script prints them.
for (nm in c("x_small", "th7", "one_to_four")) {
  v <- switch(nm, x_small = x_small, th7 = th7, one_to_four = c(1, 2, 3, 4))
  cat(nm, ": mean = ", fmt(mean(v)), "  sd = ", fmt(sd(v)), "\n", sep = "")
}
cat("exact check sqrt(5/3) = ", fmt(sqrt(5 / 3)), "\n", sep = "")

## ============================================================
cat("\n=== SECTION 4: plot_sample_ci arithmetic ===\n\n")

samples <- matrix(c(
  48, 51, 50, 49,     # sample 1
  55, 57, 54, 56,     # sample 2 (should be "bad")
  50, 50, 51, 49,     # sample 3
  44, 46, 43, 45,     # sample 4 (should be "bad")
  49, 52, 48, 51      # sample 5
), nrow = 4)

pop_mean <- 50
sample_means  <- apply(samples, 2, FUN = mean)
sample_stdevs <- apply(samples, 2, FUN = sd)
sample_stderrs <- sample_stdevs / sqrt(4)
ci95_low  <- sample_means - sample_stderrs * 1.96
ci95_high <- sample_means + sample_stderrs * 1.96
ci99_low  <- sample_means - sample_stderrs * 2.58
ci99_high <- sample_means + sample_stderrs * 2.58
bad <- which(((ci95_low > pop_mean) | (ci95_high < pop_mean)) |
             ((ci99_low > pop_mean) | (ci99_high < pop_mean)))

## Reporting added so the script prints the values the document tabulates.
for (i in seq_len(ncol(samples))) {
  cat("sample ", i, ":\n", sep = "")
  cat("  mean    = ", fmt(sample_means[i]), "\n", sep = "")
  cat("  sd      = ", fmt(sample_stdevs[i]), "\n", sep = "")
  cat("  stderr  = ", fmt(sample_stderrs[i]), "\n", sep = "")
  cat("  ci95    = ", fmt(ci95_low[i]), ", ", fmt(ci95_high[i]), "\n", sep = "")
  cat("  ci99    = ", fmt(ci99_low[i]), ", ", fmt(ci99_high[i]), "\n", sep = "")
}
cat("bad = ", paste(bad, collapse = ", "), "\n", sep = "")

## ============================================================
## Section 5 — cases computed in session for the TypeScript port
##
## These were run while writing src/core/{kde,histogram,pretty,arith}.test.ts
## and pinned in those files rather than in the fixture document. They cover
## the branches the document's algorithm sketches could get wrong: the
## bandwidth fallback chain, an explicit bandwidth, non-finite input, cells
## either side of zero, and pretty()'s unit ladder, degenerate ranges, extreme
## magnitudes and min.n widening.
## ============================================================

cat("\n=== SECTION 5: cases computed in session ===\n\n")

cat("--- 5a. density() / bw.nrd0() ---\n\n")

report_density <- function(label, d) {
  cat(label, "\n", sep = "")
  cat("  bw        = ", fmt(d$bw), "\n", sep = "")
  cat("  n         = ", d$n, "\n", sep = "")
  cat("  x[1]      = ", fmt(d$x[1]), ", x[512] = ", fmt(d$x[512]), "\n", sep = "")
  cat("  y[1]      = ", fmt(d$y[1]), ", y[256] = ", fmt(d$y[256]), "\n", sep = "")
  cat("  max(y)    = ", fmt(max(d$y)), ", which.max = ", which.max(d$y), "\n", sep = "")
  cat("  sum(y)    = ", fmt(sum(d$y)), "\n", sep = "")
}

cat("bw.nrd0(c(0,5,5,5,10)) = ", fmt(bw.nrd0(c(0, 5, 5, 5, 10))),
    "   (sd = ", fmt(sd(c(0, 5, 5, 5, 10))), ", IQR/1.34 = ",
    fmt(IQR(c(0, 5, 5, 5, 10)) / 1.34), ")\n", sep = "")
cat("bw.nrd0(c(3,9))       = ", fmt(bw.nrd0(c(3, 9))), "\n", sep = "")
cat("bw.nrd0(c(1,5,2,6,3,7)) = ", fmt(bw.nrd0(c(1, 5, 2, 6, 3, 7))), "\n", sep = "")
cat("bw.nrd0(c(4)) error: '",
    conditionMessage(tryCatch(bw.nrd0(c(4)), error = function(e) e)), "'\n\n", sep = "")

report_density("density(c(0,5,5,5,10))", density(c(0, 5, 5, 5, 10)))
report_density("density(c(x_small, Inf))", density(c(x_small, Inf)))
report_density("density(c(7), bw = 2)", density(c(7), bw = 2))
report_density("density(x_small, bw = 0.5)", density(x_small, bw = 0.5))

for (call in c("density(numeric(0))", "density(c(1, NA, 3))",
               "density(x_small, bw = 0)", "density(x_small, bw = Inf)")) {
  msg <- tryCatch({ eval(parse(text = call)); "no error" },
                  error = function(e) conditionMessage(e))
  cat(call, " -> ", msg, "\n", sep = "")
}

cat("\n--- 5b. hist() ---\n\n")

report_hist("hist(c(-3.2,-1.1,0,2.5,-4.9,1.1,0.4,-2.2))",
            hist(c(-3.2, -1.1, 0, 2.5, -4.9, 1.1, 0.4, -2.2), plot = FALSE))
report_hist("hist(c(1,2))", hist(c(1, 2), plot = FALSE))
report_hist("hist(c(0,1000,500,250,750,100,900,333))",
            hist(c(0, 1000, 500, 250, 750, 100, 900, 333), plot = FALSE))

set.seed(11)
hist30 <- round(rnorm(30, 50, 2), 4)
cat("set.seed(11); round(rnorm(30, 50, 2), 4) = ", fmt_vec(hist30), "\n", sep = "")
report_hist("hist(that vector)", hist(hist30, plot = FALSE))

report_hist("hist(th30, breaks = 10)", hist(th30, breaks = 10, plot = FALSE))
report_hist("hist(th30, breaks = 3)", hist(th30, breaks = 3, plot = FALSE))
report_hist("hist(c(1,2,3,Inf))", hist(c(1, 2, 3, Inf), plot = FALSE))
report_hist("hist(c(1,2,3,NA))", hist(c(1, 2, 3, NA), plot = FALSE))
report_hist("hist(boundary_vec, breaks = c(3, 0, 1.5))",
            hist(boundary_vec, breaks = c(3, 0, 1.5), plot = FALSE))
report_hist("hist(c(1,2,3), breaks = 1)", hist(c(1, 2, 3), breaks = 1, plot = FALSE))

for (call in c("hist(numeric(0), plot = FALSE)",
               "hist(c(NaN), plot = FALSE)",
               "hist(c(1, 2, 9), breaks = c(0, 3), plot = FALSE)")) {
  msg <- tryCatch({ eval(parse(text = call)); "no error" },
                  error = function(e) conditionMessage(e))
  cat(call, " -> ", msg, "\n", sep = "")
}

cat("\nnclass.Sturges over sample sizes: ", sep = "")
for (k in c(1, 2, 7, 8, 9, 30, 100, 1000)) {
  cat("n=", k, ":", nclass.Sturges(rep(0, k)), "  ", sep = "")
}
cat("\nnclass.Sturges(numeric(0)) = ", nclass.Sturges(numeric(0)), "\n", sep = "")

cat("\n--- 5c. pretty() ---\n\n")

pretty_cases <- list(
  list(lo = 49.2, up = 50.9, n = 4, min.n = 1),
  list(lo = 48.7, up = 52.1, n = 6, min.n = 1),
  list(lo = 0, up = 3, n = 4, min.n = 1),
  list(lo = 0, up = 1),
  list(lo = -5, up = 50),
  list(lo = 0.001, up = 0.0042, n = 5),
  list(lo = 7, up = 7),
  list(lo = 0, up = 100, n = 10),
  list(lo = 1, up = 9, n = 5),
  list(lo = -1, up = 1, n = 5),
  list(lo = 0, up = 0),
  list(lo = 3.5, up = 3.9, n = 5),
  list(lo = -50, up = -5, n = 5, min.n = 1),
  list(lo = -100, up = -1, n = 7, min.n = 2),
  list(lo = -3, up = 7, n = 5, min.n = 1),
  list(lo = -0.7, up = 0.2, n = 5, min.n = 1),
  list(lo = -1e6, up = 1e6, n = 5, min.n = 1),
  list(lo = -7, up = -7, n = 5, min.n = 1),
  list(lo = 0.0001, up = 0.00013, n = 5, min.n = 1),
  list(lo = 1e-12, up = 3e-12, n = 5, min.n = 1),
  list(lo = 123456789, up = 123456790, n = 5, min.n = 1),
  list(lo = 2.0000001, up = 2.0000002, n = 5, min.n = 1),
  list(lo = 1 / 3, up = 2 / 3, n = 5, min.n = 1),
  list(lo = 0.1, up = 0.2, n = 3, min.n = 1),
  list(lo = 5, up = 5, n = 5, min.n = 1),
  list(lo = 5, up = 5, n = 5, min.n = 2),
  list(lo = -0.5, up = -0.5, n = 5, min.n = 1),
  list(lo = 1e-9, up = 1e-9, n = 5, min.n = 1),
  list(lo = 1e9, up = 1e9, n = 5, min.n = 1),
  list(lo = 0, up = 1.4),
  list(lo = 0, up = 1.5),
  list(lo = 0, up = 1.6),
  list(lo = 0, up = 2.4),
  list(lo = 0, up = 2.5),
  list(lo = 0, up = 2.6),
  list(lo = 0, up = 3.4),
  list(lo = 0, up = 3.5),
  list(lo = 0, up = 3.6),
  list(lo = 0, up = 6),
  list(lo = 0, up = 7),
  list(lo = 0, up = 7.5),
  list(lo = 0, up = 9),
  list(lo = 0, up = 11),
  list(lo = 0, up = 10, n = 1, min.n = 0),
  list(lo = 0, up = 10, n = 2, min.n = 0),
  list(lo = 0, up = 10, n = 3, min.n = 1),
  list(lo = 0, up = 10, n = 4, min.n = 1),
  list(lo = 0, up = 10, n = 7, min.n = 2),
  list(lo = 0, up = 10, n = 8, min.n = 2),
  list(lo = 0, up = 1, n = 1, min.n = 1),
  list(lo = 0, up = 1, n = 2, min.n = 1),
  list(lo = 0, up = 1, n = 3, min.n = 1),
  list(lo = 0, up = 0.4, n = 5, min.n = 4),
  list(lo = 0, up = 0.4, n = 5, min.n = 5),
  list(lo = 3, up = 4, n = 1, min.n = 1),
  list(lo = 0, up = 1, n = 0, min.n = 0),
  ## Keep this literal as `1.5e300`. R's own decimal parser reads the printed
  ## 17-digit form (1.500000000000001e300) three units in the last place away
  ## from the double it printed, so re-entering R's output here would change
  ## the case. JavaScript parses both forms correctly; the port's test file
  ## carries the 17-digit form for that reason.
  list(lo = 1e300, up = 1.5e300, n = 5, min.n = 1)
)

for (case in pretty_cases) {
  args <- list(c(case$lo, case$up))
  if (!is.null(case$n)) args$n <- case$n
  if (!is.null(case[["min.n"]])) args$min.n <- case[["min.n"]]
  label <- paste0("pretty(c(", fmt(case$lo), ", ", fmt(case$up), ")",
                  if (!is.null(case$n)) paste0(", n = ", case$n) else "",
                  if (!is.null(case[["min.n"]])) paste0(", min.n = ", case[["min.n"]]) else "",
                  ")")
  cat(label, " = ", fmt_vec(do.call(pretty, args)), "\n", sep = "")
}

cat("\n--- 5d. quantile(), type 7 ---\n\n")

quantile_cases <- list(
  list(v = x_small, p = c(0.25, 0.5, 0.75)),
  list(v = th7, p = c(0, 0.25, 0.5, 0.75, 1)),
  list(v = c(1, 2, 3, 4), p = c(0.25, 0.33, 0.75)),
  list(v = c(7), p = c(0.25)),
  list(v = c(5, 5, 5, 5), p = c(0.25))
)
for (case in quantile_cases) {
  q <- quantile(case$v, probs = case$p, type = 7, names = FALSE)
  cat("quantile(c(", fmt_vec(case$v), "), c(", fmt_vec(case$p), ")) = ",
      fmt_vec(q), "\n", sep = "")
}
