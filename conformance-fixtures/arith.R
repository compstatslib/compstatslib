# Conformance reference values for R's summary arithmetic on a double vector.
#
# This package exports no summary functions: base R provides `mean()`, `sd()`,
# `var()`, `median()` and `quantile()`, and the functions here call them. A
# port to a language with no such standard library has to write them, and this
# script is the bar those routines have to meet.
#
# The reason this script exists is that **R has more than one arithmetic mean**
# and which one a routine uses is a parity decision, not a style one:
#
#   * `mean.default()` on a double vector goes to C `do_mean`
#     (`src/main/summary.c`). It sums, divides, and then makes a second pass
#     that adds the mean of the residuals back:
#
#         for (i = 0; i < n; i++) s += x[i];
#         s /= n;
#         if (R_FINITE((double) s)) {
#             for (i = 0; i < n; i++) t += (x[i] - s);
#             s += t/n;
#         }
#
#   * `var()`, `sd()`, `cov()` and `cor()` go to `src/library/stats/src/cov.c`,
#     whose `MEAN` macro is the **same** two-pass computation. A port that
#     centers on the plain one-pass mean is wrong here too.
#
#   * `colMeans()` / `rowMeans()` go to `do_colsum` (`src/main/array.c`), which
#     is a single pass with no correction. `scale()` and therefore `prcomp()`
#     center on that one. This is the mean that differs.
#
# Sections 1 and 2 pin the first two; section 3 pins the third beside them on
# the same data, so a port can see the two answers disagree and check that it
# uses each one where R does.
#
# The accumulators in all three are `LDOUBLE`. The build these values come
# from reports `capabilities("long.double")` as FALSE, so `LDOUBLE` is
# `double` and R's arithmetic here is exactly the double-precision computation
# a port can reproduce bit for bit. On a build with 80-bit `long double` the
# corrections still apply but the last bits will differ; pin against this
# file, which prints the capability it ran under.
#
# Consumed by (TypeScript port, compstatslib-ts):
#   src/core/arith.test.ts   (sections 1, 2, 3, 4)
#
# Verified under: R 4.5.3 (2026-03-11), arm64 macOS with R's reference BLAS.
#
# Re-run with: Rscript conformance-fixtures/arith.R   (from the package root)
#
# Values print at %.17g so ports can pin bit-exact doubles. Editing this
# script invalidates the pinned values in every port. Add sections; do not
# change existing ones.

fmt <- function(x) {
  if (is.nan(x)) "NaN" else if (is.na(x)) "NA" else sprintf("%.17g", x)
}
fmtv <- function(v) paste(sapply(v, fmt), collapse = ", ")

## Print one "label: value" line, so the document stays greppable.
val <- function(label, expr) {
  x <- expr
  cat(label, ": ", fmt(x), "\n", sep = "")
  invisible(x)
}

## The vectors. Each is printed in full at %.17g so a port pins the same
## input, not a rounded copy of it. They are chosen to separate the two means:
## a long mixed-sign vector, where cancellation makes the correction largest;
## a vector of large values with small spread, the shape a port most often
## gets wrong; and a vector of small values, where the correction is tiny but
## still moves the last bit.
set.seed(2026)
vectors <- list(
  mixed = runif(97, -500, 500),
  offset = rnorm(150, 1e6, 3),
  small = runif(51, 0, 1e-3)
)

cat("======== R's summary arithmetic on a double vector ========\n")
cat("R version: ", R.version.string, "\n", sep = "")
cat("extended-precision long double available: ",
    capabilities("long.double"), "\n", sep = "")
cat("  FALSE means R's LDOUBLE accumulators are plain doubles here, so every\n")
cat("  value below is reproducible in double precision, bit for bit.\n\n")

cat("---- 0. The vectors ----\n")
for (name in names(vectors)) {
  cat("\n", name, " (n = ", length(vectors[[name]]), "):\n", sep = "")
  cat(fmtv(vectors[[name]]), "\n", sep = "")
}

cat("\n---- 1. mean(): R's `mean.default`, the corrected two-pass form ----\n")
cat("Beside each, the uncorrected `sum(x)/n` a naive port computes. Where the\n")
cat("two lines differ, a port pinning the naive one is not computing R's mean.\n")
for (name in names(vectors)) {
  x <- vectors[[name]]
  cat("\n", name, "\n", sep = "")
  val("  mean(x)", mean(x))
  val("  sum(x)/n [NOT R's mean]", sum(x) / length(x))
}

cat("\n---- 2. sd() and var(): cov.c, which corrects the mean the same way ----\n")
cat("Beside each, the same statistic centered on the uncorrected mean. R's\n")
cat("`sd()` is the corrected line: `cov.c`'s MEAN macro is `do_mean`'s body.\n")
for (name in names(vectors)) {
  x <- vectors[[name]]
  n <- length(x)
  plain <- sum(x) / n
  cat("\n", name, "\n", sep = "")
  val("  var(x)", var(x))
  val("  sd(x)", sd(x))
  val("  var centered on sum(x)/n [NOT R's var]", sum((x - plain)^2) / (n - 1))
  val("  sd centered on sum(x)/n [NOT R's sd]", sqrt(sum((x - plain)^2) / (n - 1)))
}

cat("\n---- 3. colMeans(): the uncorrected mean, and where R uses it ----\n")
cat("`colMeans` takes a single pass. `scale(center = TRUE)` and so `prcomp`\n")
cat("center on it, which is why a port may not route those through `mean()`.\n")
for (name in names(vectors)) {
  x <- vectors[[name]]
  cat("\n", name, "\n", sep = "")
  val("  colMeans(cbind(x))", colMeans(cbind(x)))
  val("  mean(x)", mean(x))
  val("  colMeans - mean", colMeans(cbind(x)) - mean(x))
  val("  attr(scale(cbind(x)), 'scaled:center')",
      attr(scale(cbind(x), scale = FALSE), "scaled:center"))
}

cat("\n---- 4. The correction on short and degenerate input ----\n")
cat("The guard in `do_mean` returns the first pass when it is not finite, so\n")
cat("an infinite or empty vector never reaches the second pass.\n\n")
val("mean(c(1, 3, 5, 8))", mean(c(1, 3, 5, 8)))
val("mean(7)", mean(7))
val("mean(numeric(0))", mean(numeric(0)))
val("mean(c(1, Inf))", mean(c(1, Inf)))
val("mean(c(Inf, -Inf))", mean(c(Inf, -Inf)))
val("mean(c(1, NaN))", mean(c(1, NaN)))
val("sd(c(1, 2, 3, 4))", sd(c(1, 2, 3, 4)))
val("sd(c(5, 5, 5, 5))", sd(c(5, 5, 5, 5)))
val("sd(7)", sd(7))

cat("\n---- 5. How often the two means differ ----\n")
cat("A sweep, so a port can see this is the common case and not a corner.\n\n")
set.seed(11)
n_trials <- 20000
differ <- 0
gaps <- numeric(0)
for (i in seq_len(n_trials)) {
  n <- sample(50:299, 1)
  x <- runif(n, -500, 500)
  corrected <- mean(x)
  plain <- sum(x) / n
  if (!identical(corrected, plain)) {
    differ <- differ + 1
    gaps <- c(gaps, abs(corrected - plain) /
                    (.Machine$double.eps * abs(corrected)))
  }
}
cat("vectors where mean(x) != sum(x)/n: ", differ, " of ", n_trials, "\n",
    sep = "")
cat("gap, in units of the last place of the mean:\n")
cat("  median: ", sprintf("%.1f", median(gaps)), "\n", sep = "")
cat("  90th percentile: ", sprintf("%.1f", quantile(gaps, 0.9)), "\n", sep = "")
cat("  worst: ", sprintf("%.1f", max(gaps)), "\n", sep = "")
cat("\nThe worst cases are not pathological inputs. A mixed-sign vector can\n")
cat("have a mean near zero, and there the absolute error the one-pass sum\n")
cat("carries is large measured against a small result. That is the case where\n")
cat("a caller comparing two means with a strict inequality flips.\n")
