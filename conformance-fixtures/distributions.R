# Conformance reference values for the chi-square and normal distributions.
#
# This package exports no distribution functions: base R provides `pchisq()`,
# `qchisq()`, `pnorm()` and `qnorm()`, and the functions here call them. A
# port to a language with no such standard library has to write them, and this
# script is the bar those routines have to meet. The TypeScript port is the
# first consumer, which needs the chi-square pair for fit measures and the
# normal quantile for every critical value it draws.
#
# Consumed by (TypeScript port, compstatslib-ts):
#   src/core/chisq.test.ts   (sections 1, 2, 3)
#   src/core/norm.test.ts    (section 4)
#
# Verified under: R 4.5.3 (2026-03-11), arm64 macOS with R's reference BLAS.
#
# Re-run with: Rscript conformance-fixtures/distributions.R   (from the
# package root)
#
# Values print at %.17g so ports can pin bit-exact doubles. Editing this
# script invalidates the pinned values in every port. Add sections; do not
# change existing ones.

fmt <- function(x) {
  if (is.nan(x)) "NaN" else if (is.na(x)) "NA" else sprintf("%.17g", x)
}
fmtv <- function(v) paste(sapply(v, fmt), collapse = ", ")

## Print one "label: value" line, so the document stays greppable. A call
## that warns prints its warning first, on its own line, so a port sees where
## R gives up precision rather than where it stops.
val <- function(label, expr) {
  x <- withCallingHandlers(
    expr,
    warning = function(w) {
      cat("warning: ", conditionMessage(w), "\n", sep = "")
      invokeRestart("muffleWarning")
    }
  )
  cat(label, ": ", fmt(x), "\n", sep = "")
  invisible(x)
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

## The invalid-argument cases below warn and return NaN rather than stop, so
## the value is worth printing beside the warning.
report_condition_value <- function(label, expr) {
  result <- report_condition(label, expr)
  if (is.null(result)) {
    cat("value: the call stopped\n")
  } else {
    cat("value: ", fmt(result), "\n", sep = "")
  }
  invisible(result)
}

## Short labels for the grids. The exact doubles print once per section
## through fmtv, so a label such as "1e-8" stays readable while the pinned
## value keeps all seventeen digits.
x_grid <- c(1e-8, 0.001, 0.1, 0.5, 1, 2, 5, 10, 30, 60, 100, 200, 250, 300, 500, 1000)
x_labels <- c(
  "1e-8", "0.001", "0.1", "0.5", "1", "2", "5", "10",
  "30", "60", "100", "200", "250", "300", "500", "1000"
)

p_grid <- c(
  1e-300, 1e-100, 1e-16, 1e-8, 0.001, 0.01, 0.025, 0.05, 0.1, 0.25,
  0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 0.999, 1 - 1e-8, 1 - 1e-12, 1 - 1e-16
)
p_labels <- c(
  "1e-300", "1e-100", "1e-16", "1e-8", "0.001", "0.01", "0.025", "0.05",
  "0.1", "0.25", "0.5", "0.75", "0.9", "0.95", "0.975", "0.99", "0.999",
  "1-1e-8", "1-1e-12", "1-1e-16"
)

dfs <- c(1, 2, 5, 30, 200)

## ===========================================================================
## Section 1 — pchisq(): the central chi-square distribution function
## ===========================================================================
##
## R computes the central case with the regularized lower incomplete gamma
## function at shape df/2 and rate 1/2. Both tails are pinned, because the
## upper tail is where a port that subtracts from one loses every digit. The
## grid runs from 1e-8 to 1000 against degrees of freedom from 1 to 200, so
## each df meets the far left tail, the body and the far right tail.
##
## Consumed by (TypeScript port): src/core/chisq.test.ts

cat("\n==== Section 1: pchisq() central ====\n")
cat("\nx grid (exact doubles): ", fmtv(x_grid), "\n", sep = "")

for (df in dfs) {
  cat("\n---- 1a pchisq central, df = ", df, " ----\n", sep = "")
  for (i in seq_along(x_grid)) {
    x <- x_grid[i]
    lab <- x_labels[i]
    val(paste0("pchisq(", lab, ", df = ", df, ")"), pchisq(x, df = df))
    val(
      paste0("pchisq(", lab, ", df = ", df, ", lower.tail = FALSE)"),
      pchisq(x, df = df, lower.tail = FALSE)
    )
  }
}

## 1b. The boundaries. A negative quantile gives 0 with no warning, because a
## chi-square variate is never negative. df = 0 is a point mass at 0, so the
## distribution function is 1 everywhere above 0.
cat("\n---- 1b boundaries ----\n")
val("pchisq(0, df = 5)", pchisq(0, df = 5))
val("pchisq(0, df = 5, lower.tail = FALSE)", pchisq(0, df = 5, lower.tail = FALSE))
val("pchisq(-1, df = 5)", pchisq(-1, df = 5))
val("pchisq(-1, df = 5, lower.tail = FALSE)", pchisq(-1, df = 5, lower.tail = FALSE))
val("pchisq(Inf, df = 5)", pchisq(Inf, df = 5))
val("pchisq(Inf, df = 5, lower.tail = FALSE)", pchisq(Inf, df = 5, lower.tail = FALSE))
val("pchisq(1, df = 0)", pchisq(1, df = 0))
val("pchisq(1, df = 0, lower.tail = FALSE)", pchisq(1, df = 0, lower.tail = FALSE))
val("pchisq(0, df = 0)", pchisq(0, df = 0))

## 1c. Invalid arguments. R warns and returns NaN, and it propagates NaN
## without a warning.
report_condition_value("1c pchisq(1, df = -1)", pchisq(1, df = -1))
report_condition_value("1c pchisq(NaN, df = 5)", pchisq(NaN, df = 5))

## ===========================================================================
## Section 2 — qchisq(): the central chi-square quantile function
## ===========================================================================
##
## R reaches qchisq through qgamma, which takes the Wilson-Hilferty starting
## value of AS 91 and then polishes it by Newton steps on the log density.
## The probability grid runs from 1e-300 to one minus 1e-16 so a port meets
## both ends of the range, where a starting value alone is not enough.
##
## Consumed by (TypeScript port): src/core/chisq.test.ts

cat("\n==== Section 2: qchisq() central ====\n")
cat("\np grid (exact doubles): ", fmtv(p_grid), "\n", sep = "")

for (df in dfs) {
  cat("\n---- 2a qchisq central, df = ", df, " ----\n", sep = "")
  for (i in seq_along(p_grid)) {
    p <- p_grid[i]
    lab <- p_labels[i]
    val(paste0("qchisq(", lab, ", df = ", df, ")"), qchisq(p, df = df))
    val(
      paste0("qchisq(", lab, ", df = ", df, ", lower.tail = FALSE)"),
      qchisq(p, df = df, lower.tail = FALSE)
    )
  }
}

## 2b. The boundaries of the probability range.
cat("\n---- 2b boundaries ----\n")
val("qchisq(0, df = 5)", qchisq(0, df = 5))
val("qchisq(1, df = 5)", qchisq(1, df = 5))
val("qchisq(0, df = 5, lower.tail = FALSE)", qchisq(0, df = 5, lower.tail = FALSE))
val("qchisq(1, df = 5, lower.tail = FALSE)", qchisq(1, df = 5, lower.tail = FALSE))

## 2c. Probabilities outside [0, 1] and a negative df. R warns and gives NaN.
report_condition_value("2c qchisq(1.5, df = 3)", qchisq(1.5, df = 3))
report_condition_value("2c qchisq(-0.1, df = 3)", qchisq(-0.1, df = 3))
report_condition_value("2c qchisq(0.5, df = -1)", qchisq(0.5, df = -1))

## ===========================================================================
## Section 3 — pchisq(x, df, ncp): the noncentral chi-square
## ===========================================================================
##
## R has no user-facing pnchisq. `pchisq(q, df, ncp)` dispatches to
## nmath/pnchisq.c as soon as ncp is given, the same way `pt` dispatches to
## pnt. The sum is a Poisson-weighted mixture of central terms, so a port
## needs R's convergence rule as well as its formula.
##
## The RMSEA block is the shape a fit measure asks for: a fixed chi-square at
## a fixed df, walked across the noncentrality parameter, which is the
## function a confidence bound does its root search on.
##
## The ncp = 0 block prints the noncentral call and the central call side by
## side, so a port can see whether R's noncentral path returns the central
## value bit for bit at the boundary.
##
## Consumed by (TypeScript port): src/core/chisq.test.ts

cat("\n==== Section 3: pchisq() noncentral ====\n")

x_nc <- c(0.5, 2, 5, 10, 30, 60, 100, 200, 300, 500)
x_nc_labels <- c("0.5", "2", "5", "10", "30", "60", "100", "200", "300", "500")
ncps <- c(0.5, 5, 50)

for (df in c(5, 30, 200)) {
  for (ncp in ncps) {
    cat("\n---- 3a pchisq noncentral, df = ", df, ", ncp = ", ncp, " ----\n", sep = "")
    for (i in seq_along(x_nc)) {
      x <- x_nc[i]
      lab <- x_nc_labels[i]
      val(
        paste0("pchisq(", lab, ", df = ", df, ", ncp = ", ncp, ")"),
        pchisq(x, df = df, ncp = ncp)
      )
      val(
        paste0("pchisq(", lab, ", df = ", df, ", ncp = ", ncp, ", lower.tail = FALSE)"),
        pchisq(x, df = df, ncp = ncp, lower.tail = FALSE)
      )
    }
  }
}

## 3b. The RMSEA shape: chi-square 300 on 200 degrees of freedom, walked
## across the noncentrality parameter a confidence bound searches over.
cat("\n---- 3b RMSEA shape, x = 300, df = 200, ncp walked ----\n")
for (ncp in c(0, 1, 10, 50, 100, 150, 200, 300, 400)) {
  val(
    paste0("pchisq(300, df = 200, ncp = ", ncp, ")"),
    pchisq(300, df = 200, ncp = ncp)
  )
  val(
    paste0("pchisq(300, df = 200, ncp = ", ncp, ", lower.tail = FALSE)"),
    pchisq(300, df = 200, ncp = ncp, lower.tail = FALSE)
  )
}

## 3c. The boundary at ncp = 0, against the central call at the same point.
cat("\n---- 3c ncp = 0 against the central call ----\n")
boundary <- list(c(5, 5), c(30, 30), c(300, 200))
for (pair in boundary) {
  x <- pair[1]
  df <- pair[2]
  val(paste0("pchisq(", x, ", df = ", df, ", ncp = 0)"), pchisq(x, df = df, ncp = 0))
  val(paste0("pchisq(", x, ", df = ", df, ")"), pchisq(x, df = df))
  val(
    paste0("pchisq(", x, ", df = ", df, ", ncp = 0, lower.tail = FALSE)"),
    pchisq(x, df = df, ncp = 0, lower.tail = FALSE)
  )
  val(
    paste0("pchisq(", x, ", df = ", df, ", lower.tail = FALSE)"),
    pchisq(x, df = df, lower.tail = FALSE)
  )
}

## 3d. The large-ncp path, where the Poisson weights start far from term
## zero and a naive sum from the left loses the mass.
cat("\n---- 3d large ncp ----\n")
large <- list(c(1500, 200, 1000), c(50, 5, 80), c(200, 30, 100))
for (case in large) {
  x <- case[1]
  df <- case[2]
  ncp <- case[3]
  val(
    paste0("pchisq(", x, ", df = ", df, ", ncp = ", ncp, ")"),
    pchisq(x, df = df, ncp = ncp)
  )
  val(
    paste0("pchisq(", x, ", df = ", df, ", ncp = ", ncp, ", lower.tail = FALSE)"),
    pchisq(x, df = df, ncp = ncp, lower.tail = FALSE)
  )
}

## 3e. A negative noncentrality parameter.
report_condition_value("3e pchisq(5, df = 5, ncp = -1)", pchisq(5, df = 5, ncp = -1))

## ===========================================================================
## Section 4 — pnorm() and qnorm()
## ===========================================================================
##
## `qnorm` is Wichura's AS 241, which splits the range into three regions:
## the central region where the absolute value of p minus one half is at most
## 0.425, the near tail where r is at most 5, and the far tail beyond it. Each
## region has its own pair of polynomials, so a port that meets only the
## middle passes half the grid. The probability grid below crosses all three
## boundaries in both directions.
##
## The z grid for `pnorm` reaches 40, past the point where the lower tail
## underflows to zero, so a port sees which side keeps its digits.
##
## Consumed by (TypeScript port): src/core/norm.test.ts

cat("\n==== Section 4: pnorm() and qnorm() ====\n")

z_grid <- c(
  -40, -38, -37.5, -30, -20, -10, -8, -5, -3, -2, -1.5, -1, -0.5, -0.1, 0,
  0.1, 0.5, 1, 1.5, 2, 3, 5, 8, 10, 20, 30, 37.5, 38, 40
)

cat("\n---- 4a pnorm ----\n")
for (z in z_grid) {
  val(paste0("pnorm(", z, ")"), pnorm(z))
  val(paste0("pnorm(", z, ", lower.tail = FALSE)"), pnorm(z, lower.tail = FALSE))
}

q_grid <- c(
  1e-300, 1e-200, 1e-100, 1e-50, 1e-20, 1e-16, 1e-10, 1e-8, 1e-5, 1e-3,
  0.01, 0.025, 0.05, 0.075, 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9, 0.925,
  0.95, 0.975, 0.99, 1 - 1e-5, 1 - 1e-8, 1 - 1e-10, 1 - 1e-16
)
q_labels <- c(
  "1e-300", "1e-200", "1e-100", "1e-50", "1e-20", "1e-16", "1e-10", "1e-8",
  "1e-5", "1e-3", "0.01", "0.025", "0.05", "0.075", "0.1", "0.25", "0.4",
  "0.5", "0.6", "0.75", "0.9", "0.925", "0.95", "0.975", "0.99", "1-1e-5",
  "1-1e-8", "1-1e-10", "1-1e-16"
)

cat("\n---- 4b qnorm ----\n")
cat("p grid (exact doubles): ", fmtv(q_grid), "\n", sep = "")
for (i in seq_along(q_grid)) {
  p <- q_grid[i]
  lab <- q_labels[i]
  val(paste0("qnorm(", lab, ")"), qnorm(p))
  val(paste0("qnorm(", lab, ", lower.tail = FALSE)"), qnorm(p, lower.tail = FALSE))
}

## 4c. The boundaries of the probability range.
cat("\n---- 4c boundaries ----\n")
val("qnorm(0)", qnorm(0))
val("qnorm(1)", qnorm(1))
val("qnorm(0, lower.tail = FALSE)", qnorm(0, lower.tail = FALSE))
val("qnorm(1, lower.tail = FALSE)", qnorm(1, lower.tail = FALSE))

## 4d. Probabilities outside [0, 1], and NaN in.
report_condition_value("4d qnorm(1.5)", qnorm(1.5))
report_condition_value("4d qnorm(-1)", qnorm(-1))
report_condition_value("4d qnorm(NaN)", qnorm(NaN))

## 4e. The location-scale form. R shifts and scales rather than re-deriving,
## so a port that writes qnorm(p) * sd + mean matches it.
cat("\n---- 4e location and scale, mean = 10, sd = 2 ----\n")
for (p in c(0.025, 0.5, 0.975)) {
  val(paste0("qnorm(", p, ", mean = 10, sd = 2)"), qnorm(p, mean = 10, sd = 2))
}
for (z in c(6, 10, 13.5)) {
  val(paste0("pnorm(", z, ", mean = 10, sd = 2)"), pnorm(z, mean = 10, sd = 2))
}

## ===========================================================================
## Section 5 — qnorm() below the reach of AS 241
## ===========================================================================
##
## Wichura's AS 241 covers a smaller tail down to about 2.5e-317, which is
## r = sqrt(-log(p)) of 27. A double goes further: the subnormals run to
## 4.9406564584124654e-324, where r reaches 27.284. Section 4's grid stops at
## 1e-300 (r = 26.28), so nothing there crosses the boundary.
##
## R does not stop at 27. `qnorm.c` closes the gap with an asymptotic
## expansion of Maechler (2022), which solves x^2 = 2s - log(2 pi x^2) for
## s = -log(p), refined by one further term for each step inward. The
## thresholds below (r < 36000, 840, 109, 55) choose how many terms to take.
## Every probability in this section is an ordinary argument a caller can
## pass, so a port that implements only the published AS 241 returns a wrong
## number here rather than an error.
##
## Consumed by (TypeScript port): src/core/norm.test.ts

cat("\n==== Section 5: qnorm() below the reach of AS 241 ====\n")

## 5a. Either side of the r = 27 boundary. The first two still fall to AS 241's
## third region; the rest reach the asymptotic branch.
sub_grid <- c(1e-310, 2.4e-317, 1e-318, 1e-320, 1e-322, 5e-324)

cat("\n---- 5a p grid (exact doubles) ----\n")
cat("p grid: ", fmtv(sub_grid), "\n", sep = "")
cat("r = sqrt(-log(p)): ", fmtv(sqrt(-log(sub_grid))), "\n", sep = "")

cat("\n---- 5b qnorm on the subnormal tail ----\n")
for (p in sub_grid) {
  val(paste0("qnorm(", fmt(p), ")"), qnorm(p))
}

## 5c. The upper tail of the same probabilities, which R reflects.
cat("\n---- 5c upper tail ----\n")
for (p in sub_grid) {
  val(paste0("qnorm(", fmt(p), ", lower.tail = FALSE)"), qnorm(p, lower.tail = FALSE))
}

## 5d. The round trip. pnorm of the quantile returns the probability to
## within the resolution a subnormal still carries.
cat("\n---- 5d pnorm(qnorm(p)) round trip ----\n")
for (p in sub_grid) {
  val(paste0("pnorm(qnorm(", fmt(p), "))"), pnorm(qnorm(p)))
}
