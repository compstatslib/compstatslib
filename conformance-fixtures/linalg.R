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

## ===========================================================================
## Section 2 — qr(): the compact factorization lm.fit() runs on
## ===========================================================================
##
## `qr(x, LAPACK = FALSE)` is LINPACK's dqrdc2, the Householder QR with the
## limited column pivoting that lets lm.fit() report an aliased coefficient
## as NA. The port already reproduces it inside its least-squares solver;
## this section pins the factorization itself — the compact `qr` matrix,
## `qraux`, `pivot` and `rank` — and the readers qr.Q, qr.R, qr.coef,
## qr.fitted and qr.resid, so the port can expose them by name.
##
## Consumed by (TypeScript port): src/core/linalg/qr.test.ts

cat("\n==== Section 2: qr() ====\n")

report_qr <- function(label, x, y = NULL, tol = 1e-07) {
  q <- qr(x, tol = tol)
  cat("\n---- ", label, " ----\n", sep = "")
  cat("rank: ", q$rank, "\n", sep = "")
  cat("pivot: ", paste(q$pivot, collapse = ", "), "\n", sep = "")
  cat("qraux: ", fmtv(q$qraux), "\n", sep = "")
  cat("qr dim: ", nrow(q$qr), " x ", ncol(q$qr), "\n", sep = "")
  cat("qr column-major: ", fmtv(as.vector(q$qr)), "\n", sep = "")
  if (!is.null(y)) {
    cat("coef: ", fmtv(qr.coef(q, y)), "\n", sep = "")
    cat("fitted: ", fmtv(qr.fitted(q, y)), "\n", sep = "")
    cat("resid: ", fmtv(qr.resid(q, y)), "\n", sep = "")
  }
  invisible(q)
}

x <- c(1, 3, 5, 8)
y <- c(2, 4, 6, 8)

## 2a. Full rank, n > p: the ordinary regression design.
q1 <- report_qr("2a qr(cbind(1, x)), y", cbind(1, x), y)
report("2a qr.Q", qr.Q(q1))
report("2a qr.R", qr.R(q1))
report("2a qr.Q %*% qr.R recovers X", qr.Q(q1) %*% qr.R(q1))
report("2a crossprod(qr.Q) is I", crossprod(qr.Q(q1)))

## 2b. Rank deficient: a duplicated column is pivoted to the end and its
## coefficient is NA, in the ORIGINAL column order.
report_qr("2b qr(cbind(1, 1, x)), y — duplicate column", cbind(1, 1, x), y)

## 2c. Wide: more columns than rows. The last row is never reflected.
report_qr("2c qr(matrix(c(1, 10), nrow = 1)), y = 3", matrix(c(1, 10), nrow = 1), 3)

## 2d. Square, full rank.
S <- matrix(c(2, 1, -1, 1, 3, 2, 1, -1, 4), nrow = 3)
q4 <- report_qr("2d qr(S) 3 x 3, y = c(1, 2, 3)", S, c(1, 2, 3))
report("2d qr.Q", qr.Q(q4))
report("2d qr.R", qr.R(q4))
cat("2d solve(S, c(1, 2, 3)) agrees: ", fmtv(solve(S, c(1, 2, 3))), "\n", sep = "")

## 2e. The moderation-shaped design from ols.R, with dimnames on the design so
## the coefficient names travel.
z <- c(2, 5, 1, 9, 4, 6)
xx <- c(1, 2, 3, 4, 5, 6)
yy <- c(3.1, 4.4, 2.2, 9.9, 5.5, 7.7)
M <- cbind("(Intercept)" = 1, xx = xx, z = z, "xx:z" = xx * z)
q5 <- report_qr("2e qr(M) moderation shaped, yy", M, yy)
cat("2e names(qr.coef): ", paste(names(qr.coef(q5, yy)), collapse = ", "), "\n", sep = "")
cat("2e colnames(qr$qr): ", paste(colnames(q5$qr), collapse = ", "), "\n", sep = "")
report("2e qr.R keeps column names", qr.R(q5))

## 2f. Near collinear at both tolerances, as ols.R does for lm.fit.
X_near <- cbind(1, c(1, 1, 1, 1 + 1e-8))
report_qr("2f qr(X_near), tol = 1e-7", X_near, y, tol = 1e-07)
report_qr("2f qr(X_near), tol = 1e-11", X_near, y, tol = 1e-11)

## 2g. An all-zero column: its norm is 0, R substitutes 1 for the comparison
## and aliases it.
report_qr("2g qr(cbind(x, 0)), y — zero column", cbind(x, 0), y)

## 2h. Errors.
report_condition("2h qr.coef with y of the wrong length", qr.coef(q1, c(1, 2, 3)))
report_condition("2h qr() of a 0-row matrix", qr(matrix(numeric(0), nrow = 0, ncol = 2)))

## ---------------------------------------------------------------------------
## 1g. Added after review of the port's first slice: how a bare vector
## conforms in a product, silent recycling in matrix(), zero extents, diag()
## on a length-1 vector, and the vector wording of cbind()/rbind().
## ---------------------------------------------------------------------------

cat("\n==== Section 1g: vector promotion, recycling, zero extents ====\n")

## A vector on either side of %*% takes whichever shape conforms: as a row
## when its length matches the rows of the right factor, as a column when the
## right factor has one row (the outer product); mirrored on the right.
report("1g c(1, 2) %*% B — row", c(1, 2) %*% B)
report("1g c(1, 2, 3) %*% A — row", c(1, 2, 3) %*% A)
report("1g c(1, 2, 3) %*% matrix(1:4, nrow = 1) — column, outer", c(1, 2, 3) %*% matrix(1:4, nrow = 1))
report("1g matrix(1:3, 3) %*% c(10, 20) — row, outer", matrix(1:3, 3) %*% c(10, 20))
report("1g matrix(2) %*% 1:3 — row", matrix(2) %*% 1:3)
report("1g 1:3 %*% 1:3 — inner product", 1:3 %*% 1:3)
report("1g 2 %*% 1:3 — scalar times row", 2 %*% 1:3)
report_condition("1g c(1, 2) %*% A", c(1, 2) %*% A)
report_condition("1g 1:3 %*% 1:2", 1:3 %*% 1:2)
report_condition("1g 1:3 %*% 2", 1:3 %*% 2)

## crossprod takes a vector as a column when that conforms, else as a row;
## tcrossprod takes it as a row when that conforms, else as a column.
report("1g crossprod(1:3)", crossprod(1:3))
report("1g crossprod(c(1, 2, 3), A)", crossprod(c(1, 2, 3), A))
report("1g crossprod(X, Y)", crossprod(X, Y))
report("1g tcrossprod(1:3)", tcrossprod(1:3))
report("1g tcrossprod(matrix(1:3, 1), 1:3)", tcrossprod(matrix(1:3, 1), 1:3))
report("1g tcrossprod(matrix(1:3, 3), 1:3)", tcrossprod(matrix(1:3, 3), 1:3))
report("1g tcrossprod(1:2, A)", tcrossprod(1:2, A))
report_condition("1g crossprod(c(1, 2), A)", crossprod(c(1, 2), A))
report_condition("1g crossprod(A, c(1, 2))", crossprod(A, c(1, 2)))
report_condition("1g tcrossprod(A, 1:2)", tcrossprod(A, 1:2))

## matrix() recycles a scalar, and a sub-multiple, with no warning at all.
## The port recycles the scalar only.
report_condition("1g matrix(0, 3, 3) — no warning", report("1g matrix(0, 3, 3)", matrix(0, 3, 3)))
report_condition("1g matrix(7, nrow = 2) — no warning", report("1g matrix(7, nrow = 2)", matrix(7, nrow = 2)))
report_condition("1g matrix(1:3, 3, 2) — no warning", report("1g matrix(1:3, 3, 2)", matrix(1:3, 3, 2)))

## Zero extents are legal.
cat("\n---- 1g zero extents ----\n")
cat("dim(matrix(numeric(0), nrow = 0)): ", paste(dim(matrix(numeric(0), nrow = 0)), collapse = " x "), "\n", sep = "")
cat("dim(matrix(numeric(0), nrow = 0, ncol = 3)): ", paste(dim(matrix(numeric(0), nrow = 0, ncol = 3)), collapse = " x "), "\n", sep = "")
cat("dim(matrix(numeric(0), ncol = 0)): ", paste(dim(matrix(numeric(0), ncol = 0)), collapse = " x "), "\n", sep = "")
cat("dim(diag(0)): ", paste(dim(diag(0)), collapse = " x "), "\n", sep = "")
report_condition("1g matrix(1:2, nrow = 0)", matrix(1:2, nrow = 0))

## diag() reads a length-1 vector as a count and truncates a fraction. The
## port dispatches on type instead: diag([5]) is 1 x 1, and diag(2.5) refuses.
report("1g diag(c(5)) — R's gotcha", diag(c(5)))
report("1g diag(2.5) — truncates", diag(2.5))
report("1g diag() of a matrix with matching names", diag(matrix(1:4, 2, dimnames = list(c("r1", "r2"), c("r1", "r2")))))

## The vector wording of a cbind/rbind mismatch.
report_condition("1g cbind(matrix(1:4, 2), 1:3)", cbind(matrix(1:4, 2), 1:3))
report_condition("1g rbind(matrix(1:4, 2), 1:3)", rbind(matrix(1:4, 2), 1:3))

## 1h. The full vector-conformity rules of crossprod() and tcrossprod(),
## probed on the shapes that tell the candidate rules apart. Read from
## these: in crossprod a vector x is always a column, and a vector y is a
## column when its length matches the rows of x, else a row; in tcrossprod a
## vector x is a row when its length matches the columns of a matrix y, else
## a column, and a vector y is a row only when x has one row; two vectors
## are both columns.
cat("\n==== Section 1h: crossprod/tcrossprod vector conformity ====\n")
probe <- function(label, expr) report_condition(label, report(label, expr))
probe("1h tcrossprod(matrix(2), 1:3)", tcrossprod(matrix(2), 1:3))
probe("1h tcrossprod(matrix(1:2, 2), 1:2)", tcrossprod(matrix(1:2, 2), 1:2))
probe("1h tcrossprod(matrix(1:2, 1), 1:2)", tcrossprod(matrix(1:2, 1), 1:2))
probe("1h tcrossprod(1:2, matrix(1:2, 2))", tcrossprod(1:2, matrix(1:2, 2)))
probe("1h tcrossprod(1:2, matrix(1:2, 1))", tcrossprod(1:2, matrix(1:2, 1)))
probe("1h tcrossprod(1:3, matrix(1:6, 2))", tcrossprod(1:3, matrix(1:6, 2)))
probe("1h tcrossprod(1:2, matrix(1:6, 2))", tcrossprod(1:2, matrix(1:6, 2)))
probe("1h tcrossprod(matrix(1:6, 3), 1:2)", tcrossprod(matrix(1:6, 3), 1:2))
probe("1h tcrossprod(matrix(1:6, 2), 1:3)", tcrossprod(matrix(1:6, 2), 1:3))
probe("1h tcrossprod(1:3, 1:2)", tcrossprod(1:3, 1:2))
probe("1h tcrossprod(2, 1:3)", tcrossprod(2, 1:3))
probe("1h crossprod(matrix(1:3, 1), 1:3)", crossprod(matrix(1:3, 1), 1:3))
probe("1h crossprod(1:3, matrix(1:3, 1))", crossprod(1:3, matrix(1:3, 1)))
probe("1h crossprod(matrix(1:6, 2), 1:2)", crossprod(matrix(1:6, 2), 1:2))
probe("1h crossprod(1:2, matrix(1:6, 2))", crossprod(1:2, matrix(1:6, 2)))
probe("1h crossprod(1:3, 1:2)", crossprod(1:3, 1:2))
probe("1h crossprod(2, 1:3)", crossprod(2, 1:3))

## ===========================================================================
## Section 3 — LU: solve(), det(), rcond() for n x n
## ===========================================================================
##
## R's solve() is LAPACK dgesv (dgetrf with partial pivoting, then dgetrs),
## det() is dgetrf followed by a sum of logarithms of the diagonal, and
## rcond() is dgecon's estimate. Matrix::lu() exposes the compact dgetrf
## factorization (L below the unit diagonal, U on and above) and the pivot
## vector, which lets a port pin the factorization itself.
##
## Consumed by (TypeScript port): src/core/linalg/lu.test.ts

cat("\n==== Section 3: LU, solve, det, rcond ====\n")
suppressMessages(library(Matrix))
cat("LAPACK: ", La_version(), "\n", sep = "")

report_lu <- function(label, A, b = NULL, B = NULL) {
  cat("\n---- ", label, " ----\n", sep = "")
  cat("A column-major: ", fmtv(as.vector(A)), "\n", sep = "")
  cat("dim: ", nrow(A), " x ", ncol(A), "\n", sep = "")
  l <- tryCatch(Matrix::lu(A), error = function(e) NULL)
  if (!is.null(l)) {
    cat("lu compact: ", fmtv(l@x), "\n", sep = "")
    cat("lu perm (1-based ipiv): ", paste(l@perm, collapse = ", "), "\n", sep = "")
  }
  cat("det: ", fmt(det(A)), "\n", sep = "")
  d <- determinant(A)
  cat("determinant modulus (log): ", fmt(d$modulus), "  sign: ", d$sign, "\n", sep = "")
  cat("norm(A, \"O\"): ", fmt(norm(A, "O")), "\n", sep = "")
  inv <- tryCatch(solve(A), error = function(e) { cat("solve error: ", conditionMessage(e), "\n", sep = ""); NULL })
  if (!is.null(inv)) {
    cat("solve(A) column-major: ", fmtv(as.vector(inv)), "\n", sep = "")
    dn <- dimnames(inv)
    if (!is.null(dn)) cat("solve(A) rownames: ", paste(dn[[1]], collapse = ", "), "  colnames: ", paste(dn[[2]], collapse = ", "), "\n", sep = "")
    cat("rcond(A, \"O\"): ", fmt(rcond(A, norm = "O")), "\n", sep = "")
    cat("exact 1/(norm1(A) norm1(inv)): ", fmt(1 / (norm(A, "O") * norm(inv, "O"))), "\n", sep = "")
    if (!is.null(b)) {
      s <- solve(A, b)
      cat("solve(A, b): ", fmtv(s), "\n", sep = "")
      if (!is.null(names(s))) cat("solve(A, b) names: ", paste(names(s), collapse = ", "), "\n", sep = "")
    }
    if (!is.null(B)) {
      s <- solve(A, B)
      cat("solve(A, B) column-major: ", fmtv(as.vector(s)), "\n", sep = "")
      dn <- dimnames(s)
      if (!is.null(dn)) cat("solve(A, B) rownames: ", paste(dn[[1]], collapse = ", "), "  colnames: ", paste(dn[[2]], collapse = ", "), "\n", sep = "")
    }
  }
  invisible(NULL)
}

S <- matrix(c(2, 1, -1, 1, 3, 2, 1, -1, 4), nrow = 3)
report_lu("3a S, no interchange", S, b = c(1, 2, 3), B = matrix(c(1, 0, 0, 2, 1, 1), nrow = 3))

P <- matrix(c(0, 1, 2, 3, 1, 0, 1, 4, 2), nrow = 3)
report_lu("3b P, zero leading entry forces an interchange", P, b = c(1, 2, 3))

M4 <- matrix(c(1.3, -0.7, 0.4, 1.9, 2.2, 0.5, -1.1, 0.8, -0.3, 1.6, 2.4, -0.9, 0.7, -1.2, 0.6, 1.5), nrow = 4)
report_lu("3c M4, non-integer entries", M4, b = c(1, -1, 0.5, 2))

X <- matrix(c(1, 2, 3, 4), nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b")))
report_lu("3d X with dimnames", X, b = c(1, 2), B = matrix(c(1, 0, 0, 1), nrow = 2, dimnames = list(NULL, c("p", "q"))))

report_lu("3e 1 x 1", matrix(4), b = 2)

Z <- matrix(c(1, 2, 3, 2, 4, 6, 1, 0, 1), nrow = 3)
report_lu("3f Z, exactly singular (column 2 = 2 * column 1)", Z)

Z2 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3)
report_lu("3g Z2 = 1:9, singular in exact arithmetic", Z2)

N <- matrix(c(1, 1, 1, 1, 1, 1 + 1e-15, 1, 1 + 1e-15, 1), nrow = 3)
report_lu("3h N, near singular", N)

report_lu("3i the default matrix-inverse fixture F1 (1, 2, 2, 1)", matrix(c(1, 2, 2, 1), nrow = 2))
report_lu("3j F4a (-2, -1.6, -1.5, -1.2)", matrix(c(-2, -1.6, -1.5, -1.2), nrow = 2))

report_condition("3k solve of a non-square matrix", solve(matrix(1:6, nrow = 2)))
report_condition("3k solve(A, b) with b of the wrong length", solve(S, c(1, 2)))
report_condition("3k det of a non-square matrix", det(matrix(1:6, nrow = 2)))

## ===========================================================================
## Section 4 — model.matrix() and lm()
## ===========================================================================
##
## The bridge from a data frame to a design: model.matrix() names its
## columns as lm() names its coefficients — (Intercept), the main effects in
## formula order, then the interactions — and drops incomplete rows the way
## model.frame() does. lm() is qr() on that matrix; summary.lm() reads R²,
## adjusted R², sigma and the standard errors off the fit.
##
## Consumed by (TypeScript port): src/core/linalg/modelMatrix.test.ts,
##                                src/core/linalg/lm.test.ts

cat("\n==== Section 4: model.matrix() and lm() ====\n")
load("data/moderation_data.rda")

report_mm <- function(label, formula, data) {
  mm <- model.matrix(formula, data)
  cat("\n---- ", label, " ----\n", sep = "")
  cat("dim: ", nrow(mm), " x ", ncol(mm), "\n", sep = "")
  cat("colnames: ", paste(colnames(mm), collapse = ", "), "\n", sep = "")
  cat("assign: ", paste(attr(mm, "assign"), collapse = ", "), "\n", sep = "")
  cat("row 1: ", fmtv(mm[1, ]), "\n", sep = "")
  if (nrow(mm) > 1) cat("row n: ", fmtv(mm[nrow(mm), ]), "\n", sep = "")
  cat("colSums: ", fmtv(colSums(mm)), "\n", sep = "")
  invisible(mm)
}

report_lm <- function(label, formula, data, ...) {
  fit <- lm(formula, data, ...)
  s <- summary(fit)
  cat("\n---- ", label, " ----\n", sep = "")
  cat("coef names: ", paste(names(coef(fit)), collapse = ", "), "\n", sep = "")
  cat("coef: ", fmtv(coef(fit)), "\n", sep = "")
  cat("rank: ", fit$rank, "  df.residual: ", fit$df.residual, "\n", sep = "")
  cat("fitted[1:5]: ", fmtv(head(fitted(fit), 5)), "\n", sep = "")
  cat("residuals[1:5]: ", fmtv(head(residuals(fit), 5)), "\n", sep = "")
  cat("length(fitted): ", length(fitted(fit)), "\n", sep = "")
  cat("r.squared: ", fmt(s$r.squared), "  adj.r.squared: ", fmt(s$adj.r.squared), "  sigma: ", fmt(s$sigma), "\n", sep = "")
  cm <- coef(s)
  cat("summary rows: ", paste(rownames(cm), collapse = ", "), "\n", sep = "")
  cat("std.error: ", fmtv(cm[, 2]), "\n", sep = "")
  cat("t value: ", fmtv(cm[, 3]), "\n", sep = "")
  cat("p value: ", fmtv(cm[, 4]), "\n", sep = "")
  if (!is.null(s$fstatistic)) cat("fstatistic: ", fmtv(s$fstatistic), "\n", sep = "")
  invisible(fit)
}

## 4a. The moderation model with a control: interactions come after every
## main effect, w included.
report_mm("4a model.matrix(y ~ x * z + w)", y ~ x * z + w, moderation_data)
report_lm("4a lm(y ~ x * z + w)", y ~ x * z + w, moderation_data)

## 4b. Shapes of the term list.
report_mm("4b model.matrix(y ~ x + z - 1) no intercept", y ~ x + z - 1, moderation_data)
report_mm("4b model.matrix(y ~ x:z) interaction only", y ~ x:z, moderation_data)
report_mm("4b model.matrix(y ~ x:z + z) — main effect after its interaction", y ~ x:z + z, moderation_data)
report_mm("4b model.matrix(y ~ x * z * w) three-way", y ~ x * z * w, moderation_data)
report_lm("4b lm(y ~ x + z - 1)", y ~ x + z - 1, moderation_data)
report_lm("4b lm(y ~ 1) intercept only", y ~ 1, moderation_data)

## 4c. An aliased column: x2 = 2 x. Coefficient NA, rank 2.
aliased <- data.frame(y = moderation_data$y, x = moderation_data$x, x2 = 2 * moderation_data$x)
report_mm("4c model.matrix(y ~ x + x2)", y ~ x + x2, aliased)
report_lm("4c lm(y ~ x + x2) aliased", y ~ x + x2, aliased)

## 4d. Missing values: model.frame drops the row; na.exclude pads the fit.
holed <- moderation_data
holed$y[3] <- NA
holed$x[5] <- NA
report_mm("4d model.matrix with rows 3 and 5 incomplete", y ~ x * z, holed)
report_lm("4d lm(y ~ x * z) na.omit", y ~ x * z, holed)
fit_ex <- lm(y ~ x * z, holed, na.action = na.exclude)
cat("\n---- 4d na.exclude padding ----\n")
cat("fitted[1:6]: ", fmtv(head(fitted(fit_ex), 6)), "\n", sep = "")
cat("residuals[1:6]: ", fmtv(head(residuals(fit_ex), 6)), "\n", sep = "")
cat("length(fitted): ", length(fitted(fit_ex)), "\n", sep = "")

## 4e. Agreement with the simple regression of regression.R: y ~ x.
report_lm("4e lm(y ~ x)", y ~ x, moderation_data)

## 4f. Errors.
report_condition("4f a term naming an absent column", model.matrix(y ~ x + nope, moderation_data))
report_condition("4f lm with every row incomplete", lm(y ~ x, data.frame(y = c(NA, NA), x = c(1, 2))))

## ===========================================================================
## Section 5 — cov(), cor(), eigen(symmetric = TRUE), prcomp()
## ===========================================================================
##
## The covariance and correlation matrices of a frame, the symmetric
## eigendecomposition R reaches through LAPACK's dsyevr, and prcomp(), which
## R computes through the SVD of the centered data. Eigenvector signs are
## LAPACK's; a port compares them up to sign.
##
## Consumed by (TypeScript port): src/core/linalg/eigen.test.ts,
##                                src/core/linalg/prcomp.test.ts

cat("\n==== Section 5: cov, cor, eigen, prcomp ====\n")
load("data/pca_degenerate.rda")

md <- as.matrix(moderation_data)
report("5a cov(moderation_data)", cov(md))
report("5a cor(moderation_data)", cor(md))
cat("5a colMeans: ", fmtv(colMeans(md)), "\n", sep = "")

report_eigen <- function(label, S) {
  e <- eigen(S, symmetric = TRUE)
  cat("\n---- ", label, " ----\n", sep = "")
  cat("values: ", fmtv(e$values), "\n", sep = "")
  cat("vectors column-major: ", fmtv(as.vector(e$vectors)), "\n", sep = "")
  cat("check |S v - lambda v| max: ", fmt(max(abs(S %*% e$vectors - e$vectors %*% diag(e$values, nrow = length(e$values))))), "\n", sep = "")
  invisible(e)
}
report_eigen("5b eigen(cov(moderation_data))", cov(md))
report_eigen("5b eigen 2 x 2 [[2, 1], [1, 2]]", matrix(c(2, 1, 1, 2), 2))
report_eigen("5b eigen diagonal, unsorted input", diag(c(1, 3, 2)))
report_eigen("5b eigen 1 x 1", matrix(5))
report_eigen("5b eigen of a rank-1 matrix", tcrossprod(c(1, 2, 3)))
report_eigen("5b eigen with a repeated eigenvalue", diag(2))
report_condition("5b eigen(symmetric = TRUE) on a non-symmetric matrix silently uses the lower triangle",
                 report("5b lower triangle of [[1, 9], [2, 1]]", eigen(matrix(c(1, 2, 9, 1), 2), symmetric = TRUE)$values))

report_prcomp <- function(label, x, ...) {
  p <- prcomp(x, ...)
  cat("\n---- ", label, " ----\n", sep = "")
  cat("sdev: ", fmtv(p$sdev), "\n", sep = "")
  cat("center: ", fmtv(p$center), "\n", sep = "")
  cat("scale: ", if (isFALSE(p$scale)) "FALSE" else fmtv(p$scale), "\n", sep = "")
  cat("rotation dim: ", nrow(p$rotation), " x ", ncol(p$rotation), "\n", sep = "")
  cat("rotation rownames: ", paste(rownames(p$rotation), collapse = ", "), "  colnames: ", paste(colnames(p$rotation), collapse = ", "), "\n", sep = "")
  cat("rotation column-major: ", fmtv(as.vector(p$rotation)), "\n", sep = "")
  cat("x[1:3, ] column-major: ", fmtv(as.vector(p$x[1:min(3, nrow(p$x)), , drop = FALSE])), "\n", sep = "")
  cat("x dim: ", nrow(p$x), " x ", ncol(p$x), "\n", sep = "")
  invisible(p)
}
report_prcomp("5c prcomp(moderation_data)", md)
report_prcomp("5c prcomp(moderation_data, scale. = TRUE)", md, scale. = TRUE)
report_prcomp("5c prcomp(pca_degenerate)", as.matrix(pca_degenerate))
report_prcomp("5c prcomp(pca_degenerate, center = FALSE)", as.matrix(pca_degenerate), center = FALSE)

## 5d. cov()/cor() on vectors and with a missing value.
cat("\n---- 5d cov and cor of two vectors ----\n")
cat("cov(x, z): ", fmt(cov(moderation_data$x, moderation_data$z)), "\n", sep = "")
cat("cor(x, z): ", fmt(cor(moderation_data$x, moderation_data$z)), "\n", sep = "")
cat("var(x): ", fmt(var(moderation_data$x)), "\n", sep = "")
report_condition("5d cov with an NA", cov(c(1, 2, NA), c(2, 4, 6)))
report_condition("5d cor of a constant", cor(c(1, 1, 1), c(1, 2, 3)))
report_condition("5d cov of one observation", cov(matrix(c(1, 2), 1)))
