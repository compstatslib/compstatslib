# Conformance reference values for the linear-algebra primitives.
#
# This package exports no linear algebra: base R provides `matrix()`, `t()`,
# `%*%`, `crossprod()`, `cbind()`, `solve()`, `qr()` and `model.matrix()`, and
# the functions here call them. A port to a language with no such standard
# library has to write them, and this script is the bar those routines have
# to meet. The TypeScript port's `@compstats/core/linalg` entry is the first
# consumer.
#
# Consumed by (TypeScript port, compstatslib-ts):
#   src/core/linalg/matrix.test.ts   (section 1)
#   src/core/linalg/ops.test.ts      (section 1)
#   src/core/linalg/vector.test.ts   (section 1)
#
# Verified under: R 4.5.3 (2026-03-11), arm64 macOS with R's reference BLAS.
#
# Re-run with: Rscript conformance-fixtures/linalg.R   (from the package root)
#
# Values print at %.17g so ports can pin bit-exact doubles. Editing this
# script invalidates the pinned values in every port. Add sections; do not
# change existing ones.

fmt <- function(x) if (is.na(x)) "NA" else sprintf("%.17g", x)
fmtv <- function(v) paste(sapply(v, fmt), collapse = ", ")

## Print a matrix as R stores it: column-major, one line, plus its dim and
## dimnames. A port that reads `as.vector(m)` back in column-major order gets
## the same doubles in the same positions.
report <- function(label, m) {
  cat("\n---- ", label, " ----\n", sep = "")
  if (is.matrix(m)) {
    cat("dim: ", nrow(m), " x ", ncol(m), "\n", sep = "")
    dn <- dimnames(m)
    cat("rownames: ", if (is.null(dn[[1]])) "NULL" else paste(dn[[1]], collapse = ", "), "\n", sep = "")
    cat("colnames: ", if (is.null(dn[[2]])) "NULL" else paste(dn[[2]], collapse = ", "), "\n", sep = "")
    cat("column-major: ", fmtv(as.vector(m)), "\n", sep = "")
  } else {
    cat("vector: ", fmtv(m), "\n", sep = "")
    if (!is.null(names(m))) cat("names: ", paste(names(m), collapse = ", "), "\n", sep = "")
  }
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
## Section 1 — construction, transpose, product, binding, diagonal, vectors
## ===========================================================================

cat("\n==== Section 1: construction and elementary operations ====\n")

## 1a. Fill order. R fills column by column unless byrow = TRUE.
A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
report("1a matrix(1:6, nrow = 3)", A)
report("1a matrix(1:6, nrow = 3, byrow = TRUE)", matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE))
report("1a matrix(1:6, ncol = 2) infers nrow", matrix(c(1, 2, 3, 4, 5, 6), ncol = 2))
report("1a t(A)", t(A))

## 1b. Product. A is 3 x 2; B is 2 x 4. Entries are exact binary fractions, so
## the products and sums are exact and a port can pin them bit for bit.
B <- matrix(c(0.5, -1, 2, 3, 1.5, 0, -2.5, 4), nrow = 2)
report("1b B", B)
report("1b A %*% B", A %*% B)
report("1b crossprod(A) = t(A) %*% A", crossprod(A))
report("1b tcrossprod(A) = A %*% t(A)", tcrossprod(A))
y <- c(1.25, -0.5, 2)
report("1b crossprod(A, y)", crossprod(A, y))
report("1b A %*% c(2, -0.5) (vector as column)", A %*% c(2, -0.5))

## 1c. dimnames travel. A product keeps the row names of the left factor and
## the column names of the right; a transpose swaps them; binding stacks them.
X <- matrix(c(1, 2, 3, 4), nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b")))
Y <- matrix(c(1, 0, 2, -1, 0.5, 3), nrow = 2, dimnames = list(NULL, c("u", "v", "w")))
report("1c X", X)
report("1c t(X)", t(X))
report("1c X %*% Y", X %*% Y)
report("1c cbind(X, c(9, 8)) — unnamed column gets \"\"", cbind(X, c(9, 8)))
report("1c cbind(X, X)", cbind(X, X))
report("1c rbind(X, c(7, 6))", rbind(X, c(7, 6)))
report("1c rbind(A, t(B[, 1:3]))", rbind(A, t(B[, 1:3])))
report("1c cbind(c(1, 2), c(3, 4)) — no names at all", cbind(c(1, 2), c(3, 4)))

## 1d. diag() in its three forms.
report("1d diag(c(1, 2, 3))", diag(c(1, 2, 3)))
report("1d diag(3) identity", diag(3))
report("1d diag(A) of a 3 x 2", diag(A))
report("1d diag(X) keeps no names", diag(X))

## 1e. Vector arithmetic. R's operators are functions over vectors; a scalar
## recycles. Entries are exact binary fractions.
a <- c(1.5, -2, 3.25, 0.5)
b <- c(2, 4, -1, 0.25)
report("1e a + 1", a + 1)
report("1e a - b", a - b)
report("1e a * b", a * b)
report("1e 2 * a", 2 * a)
report("1e a^2", a^2)
report("1e a / b", a / b)
cat("\n---- 1e dot, norms, cosine ----\n")
cat("sum(a * b): ", fmt(sum(a * b)), "\n", sep = "")
cat("sqrt(sum(a^2)): ", fmt(sqrt(sum(a^2))), "\n", sep = "")
cat("sqrt(sum(b^2)): ", fmt(sqrt(sum(b^2))), "\n", sep = "")
cat("cosine: ", fmt(sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))), "\n", sep = "")
cat("cosine(a, a): ", fmt(sum(a * a) / (sqrt(sum(a^2)) * sqrt(sum(a^2)))), "\n", sep = "")
cat("cosine(c(1, 0), c(0, 1)): ", fmt(0 / 1), "\n", sep = "")
## A norm a spread-based implementation cannot take: sqrt(2e5) exactly.
cat("sqrt(sum(rep(1, 200000)^2)): ", fmt(sqrt(sum(rep(1, 200000)^2))), "\n", sep = "")

## 1f. What R refuses, and what it only warns about. The port refuses in
## every case below: a warning that recycles a mismatched length would let a
## typo silently fit the wrong model.
report_condition("1f A %*% A non-conformable", A %*% A)
report_condition("1f crossprod(A, B) non-conformable", crossprod(A, B))
report_condition("1f cbind rows differ", cbind(matrix(1:4, 2), matrix(1:6, 3)))
report_condition("1f rbind columns differ", rbind(matrix(1:4, 2), matrix(1:6, 2)))
report_condition("1f matrix(1:5, nrow = 2) — R warns and recycles", matrix(1:5, nrow = 2))
report_condition("1f a + c(1, 2, 3) — R warns and recycles", a + c(1, 2, 3))
report_condition("1f cbind(X, c(9, 8, 7)) — R warns and recycles", cbind(X, c(9, 8, 7)))
