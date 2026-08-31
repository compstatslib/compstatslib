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
#   src/core/linalg/ops.test.ts      (sections 1, 9)
#   src/core/linalg/vector.test.ts   (sections 1, 9)
#   src/core/linalg/cov.test.ts      (sections 5, 6)
#   src/core/linalg/scale.test.ts    (section 6)
#   src/core/linalg/chol.test.ts     (section 7)
#   src/core/linalg/lm.test.ts       (sections 4, 8)
#
# Verified under: R 4.5.3 (2026-03-11), arm64 macOS with R's reference BLAS.
# That build contracts each multiply-add into one rounding. The TypeScript
# port's default arithmetic does the same, so its factorizations pin these
# values bit for bit; its `{ fma: false }` option rounds twice and lands a few
# units in the last place away.
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

## ---------------------------------------------------------------------------
## 2i and 3l. Added after review of the port's QR and LU slices: row names
## through qr(), a matrix right-hand side for the qr readers, tol = 0, the
## other norm types, rcond() of a non-square matrix, the 0-diml errors, a
## negative zero through solve(), and %g at the exponents that trip a naive
## formatter.
## ---------------------------------------------------------------------------

cat("\n==== Section 2i: qr() names, matrix y, tol = 0 ====\n")
nm <- matrix(c(1, 2, 3, 4, 5, 6), 3, 2, dimnames = list(c("r1", "r2", "r3"), c("a", "b")))
q_nm <- qr(nm)
cat("dimnames(qr(nm)$qr): ", paste(dimnames(q_nm$qr)[[1]], collapse = ", "), " | ", paste(dimnames(q_nm$qr)[[2]], collapse = ", "), "\n", sep = "")
cat("dimnames(qr.R(qr(nm))): ", paste(dimnames(qr.R(q_nm))[[1]], collapse = ", "), " | ", paste(dimnames(qr.R(q_nm))[[2]], collapse = ", "), "\n", sep = "")
cat("dimnames(qr.Q(qr(nm))) is NULL: ", is.null(dimnames(qr.Q(q_nm))), "\n", sep = "")
wide <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3, dimnames = list(c("r1", "r2"), c("a", "b", "c")))
cat("dimnames(qr.R(qr(wide))): ", paste(dimnames(qr.R(qr(wide)))[[1]], collapse = ", "), " | ", paste(dimnames(qr.R(qr(wide)))[[2]], collapse = ", "), "\n", sep = "")
Y2 <- cbind(y1 = c(1, 2, 3), y2 = c(2, 3, 5))
report("2i qr.coef(qr(nm), Y2)", qr.coef(q_nm, Y2))
report("2i qr.fitted(qr(nm), Y2)", qr.fitted(q_nm, Y2))
report("2i qr.resid(qr(nm), Y2)", qr.resid(q_nm, Y2))
report("2i qr.qty(qr(nm), Y2)", qr.qty(q_nm, Y2))
report("2i qr.qy(qr(nm), Y2)", qr.qy(q_nm, Y2))
report("2i qr.coef of the duplicate-column design with a matrix y", qr.coef(qr(cbind(1, 1, c(1, 3, 5, 8))), cbind(y1 = c(2, 4, 6, 8), y2 = c(1, 1, 2, 3))))
cat("2i qr(cbind(1, 1, x), tol = 0)$rank: ", qr(cbind(1, 1, c(1, 3, 5, 8)), tol = 0)$rank, "\n", sep = "")
report_condition("2i qr() of a matrix with NaN", qr(matrix(c(1, NaN, 3, 4), 2)))
report_condition("2i qr.coef with a matrix y of the wrong rows", qr.coef(q_nm, matrix(1:4, 2)))

cat("\n==== Section 3l: norms, rcond of non-square, 0-diml, negative zero, %g ====\n")
S <- matrix(c(2, 1, -1, 1, 3, 2, 1, -1, 4), nrow = 3)
for (type in c("O", "I", "F", "M")) cat("norm(S, \"", type, "\"): ", fmt(norm(S, type)), "\n", sep = "")
cat("norm(S, \"f\") lowercase: ", fmt(norm(S, "f")), "\n", sep = "")
report_condition("3l norm with a bad type", norm(S, "X"))
cat("rcond(matrix(1:6, 2)): ", fmt(rcond(matrix(1:6, 2))), "\n", sep = "")
cat("rcond(matrix(1:6, 3)): ", fmt(rcond(matrix(1:6, 3))), "\n", sep = "")
cat("rcond(qr.R(qr(matrix(1:6, 3)))): ", fmt(rcond(qr.R(qr(matrix(1:6, 3))))), "\n", sep = "")
cat("exact rcond of that R factor: ", fmt(1 / (norm(qr.R(qr(matrix(1:6, 3))), "O") * norm(solve(qr.R(qr(matrix(1:6, 3)))), "O"))), "\n", sep = "")
cat("rcond(diag(0)): ", fmt(rcond(diag(0))), "  det(diag(0)): ", fmt(det(diag(0))), "\n", sep = "")
report_condition("3l solve of a 0 x 0 matrix", solve(matrix(numeric(0), 0, 0)))
report_condition("3l solve with a 0-column right-hand side", solve(diag(2), matrix(numeric(0), 2, 0)))
cat("3l 1 / solve(diag(2), c(-0, 1)): ", fmtv(1 / solve(diag(2), c(-0, 1))), "\n", sep = "")
cat("3l solve(matrix(1:4, 2), tol = -1) runs: ", fmtv(solve(matrix(c(1, 2, 3, 4), 2), tol = -1)), "\n", sep = "")
report_condition("3l %g at e-06", solve(matrix(c(1, 1, 1, 1 + 2e-5), 2), tol = 1e-3))
report_condition("3l %g at e-05", solve(matrix(c(1, 0, 0, 1.234e-5), 2), tol = 1e-3))
report_condition("3l %g at the e-04 boundary", solve(matrix(c(1, 0, 0, 9.9999999e-5), 2), tol = 1e-3))
report_condition("3l %g at e-04", solve(matrix(c(1, 0, 0, 1e-4), 2), tol = 1e-3))
report_condition("3l %g at e-07", solve(matrix(c(1, 0, 0, 1e-7), 2), tol = 1e-3))
report_condition("3l %g exactly 1e-05", solve(matrix(c(1, 0, 0, 1e-5), 2), tol = 1e-3))
report_condition("3l F5 (1, 1, 1, 1) exactly singular", solve(matrix(c(1, 1, 1, 1), 2)))
cat("3l det(F5): ", fmt(det(matrix(c(1, 1, 1, 1), 2))), "\n", sep = "")
report_condition("3l solve with an Inf entry", report("3l solve([[Inf, 1], [1, 1]])", solve(matrix(c(Inf, 1, 1, 1), 2))))
report_condition("3l rcond with a NaN entry", rcond(matrix(c(1, 1, NaN, 1), 2)))

## ---------------------------------------------------------------------------
## 4g and 5e. Added after review of the port's lm and eigen slices: a
## saturated fit with no residual degrees of freedom, a fit with no
## intercept, and the inputs eigen() and prcomp() refuse.
## ---------------------------------------------------------------------------

cat("\n==== Section 4g: lm at the edges of the residual degrees of freedom ====\n")
sat <- data.frame(y = c(1, 3, 2), x = c(1, 2, 3), z = c(2, 1, 4))
fs <- lm(y ~ x + z, sat)
ss <- summary(fs)
report("4g saturated lm(y ~ x + z) coefficients", coef(fs))
cat("4g rank: ", fs$rank, "  df.residual: ", fs$df.residual, "\n", sep = "")
cat("4g sigma: ", fmt(ss$sigma), "  r.squared: ", fmt(ss$r.squared), "  adj.r.squared: ", fmt(ss$adj.r.squared), "\n", sep = "")
cat("4g std errors: ", fmtv(coef(ss)[, "Std. Error"]), "\n", sep = "")
cat("4g fstatistic: ", fmtv(ss$fstatistic), "\n", sep = "")
report("4g saturated residuals", residuals(fs))

noint <- lm(y ~ x - 1, moderation_data)
sn <- summary(noint)
report("4g lm(y ~ x - 1) coefficients", coef(noint))
cat("4g no-intercept r.squared: ", fmt(sn$r.squared), "  adj: ", fmt(sn$adj.r.squared), "  sigma: ", fmt(sn$sigma), "\n", sep = "")
cat("4g no-intercept fstatistic: ", fmtv(sn$fstatistic), "\n", sep = "")

onlyint <- lm(y ~ 1, moderation_data)
so <- summary(onlyint)
report("4g lm(y ~ 1) coefficients", coef(onlyint))
cat("4g intercept-only r.squared: ", fmt(so$r.squared), "  adj: ", fmt(so$adj.r.squared), "  sigma: ", fmt(so$sigma), "  fstatistic is NULL: ", is.null(so$fstatistic), "\n", sep = "")

cat("\n==== Section 5e: eigen() and prcomp() refusals, prcomp row names ====\n")
report_condition("5e eigen with an NA entry", eigen(matrix(c(NA, 1, 1, 1), 2), symmetric = TRUE))
report_condition("5e eigen with an Inf entry", eigen(matrix(c(Inf, 1, 1, 1), 2), symmetric = TRUE))
report_condition("5e eigen of a 0 x 0 matrix", eigen(matrix(numeric(0), 0, 0), symmetric = TRUE))
report_condition("5e prcomp with an NA entry", prcomp(matrix(c(NA, 1, 2, 3, 4, 5), 3, 2)))
report_condition("5e prcomp of a constant column with scale. = TRUE", prcomp(matrix(c(1, 1, 1, 1, 2, 3), 3, 2), scale. = TRUE))

named_rows <- matrix(c(1, 2, 3, 4, 5, 6, 2, 1, 4, 3, 6, 5), 6, 2,
                     dimnames = list(c("o1", "o2", "o3", "o4", "o5", "o6"), c("a", "b")))
pn <- prcomp(named_rows)
cat("5e prcomp x rownames: ", paste(rownames(pn$x), collapse = ", "), "\n", sep = "")
cat("5e prcomp x colnames: ", paste(colnames(pn$x), collapse = ", "), "\n", sep = "")
cat("5e prcomp rotation rownames: ", paste(rownames(pn$rotation), collapse = ", "), "\n", sep = "")
cat("5e prcomp center names: ", paste(names(pn$center), collapse = ", "), "\n", sep = "")
report("5e prcomp(named_rows) sdev", pn$sdev)
report("5e prcomp(named_rows) rotation", pn$rotation)
report("5e prcomp(named_rows) x", pn$x)

## ---------------------------------------------------------------------------
## 4h. The moderation demo's own model, y ~ x * z, fitted through lm(). The
## port's `moderationSurface` reads its fit from the same routine, so these
## are the values it has to reproduce. R's residuals are dqrsl's and its
## fitted values are y minus them, which is why the two do not satisfy
## `residuals == y - fitted` exactly.
## ---------------------------------------------------------------------------

cat("\n==== Section 4h: lm(y ~ x * z, moderation_data) ====\n")
m1 <- lm(y ~ x * z, moderation_data)
report("4h coefficients", coef(m1))
cat("4h fitted[1:5]: ", fmtv(head(fitted(m1), 5)), "\n", sep = "")
cat("4h residuals[1:5]: ", fmtv(head(residuals(m1), 5)), "\n", sep = "")
cat("4h rows where residuals(m1) != y - fitted(m1): ",
    sum(residuals(m1) != moderation_data$y - fitted(m1)), " of ", nrow(moderation_data), "\n", sep = "")
cat("4h max |residuals(m1) - (y - fitted(m1))|: ",
    fmt(max(abs(residuals(m1) - (moderation_data$y - fitted(m1))))), "\n", sep = "")

## ===========================================================================
## Section 6 — cov(x, y) and cor(x, y) over two matrices, and scale()
## ===========================================================================
##
## cov() and cor() take a second matrix. They then return the cross
## covariance or the cross correlation of every column of x against every
## column of y. The result carries the column names of x as its row names
## and the column names of y as its column names. A vector counts as one
## column, so a vector against a two-column matrix gives a 1 x 2 result.
##
## scale() centers each column, then divides it. It reports the values it
## used in the attributes "scaled:center" and "scaled:scale". An attribute is
## absent when its argument was FALSE. R prints NaN for a column of zero
## variance, and fmt() above prints NaN as NA, so each scale case also lists
## the column-major indices of its NaN entries.
##
## Consumed by (TypeScript port): src/core/linalg/cov.test.ts,
##                                src/core/linalg/scale.test.ts

cat("\n==== Section 6: two-matrix cov and cor, scale ====\n")

md <- as.matrix(moderation_data)

## 6a. Two matrices from the bundled data. Rows come from x, columns from y.
report("6a cov(md[, c(\"x\", \"z\")], md[, c(\"y\", \"w\")])",
       cov(md[, c("x", "z")], md[, c("y", "w")]))
report("6a cor(md[, c(\"x\", \"z\")], md[, c(\"y\", \"w\")])",
       cor(md[, c("x", "z")], md[, c("y", "w")]))

## 6b. A vector on either side. R treats it as a single unnamed column, so
## the result is 1 x 2 one way and 2 x 1 the other.
report("6b cor(md[, \"x\"], md[, c(\"y\", \"w\")])", cor(md[, "x"], md[, c("y", "w")]))
report("6b cor(md[, c(\"y\", \"w\")], md[, \"x\"])", cor(md[, c("y", "w")], md[, "x"]))
report("6b cov(md[, \"x\"], md[, c(\"y\", \"w\")])", cov(md[, "x"], md[, c("y", "w")]))

## 6c. What R refuses, and what it only warns about. A row-count mismatch is
## an error. A constant column is a warning, and its row of the result is NA.
report_condition("6c cor(matrix(1:6, 3), matrix(1:4, 2)) row counts differ",
                 cor(matrix(1:6, 3), matrix(1:4, 2)))
report_condition("6c cor with a constant column in x",
                 report("6c cor(cbind(a = c(1, 1, 1), b = 1:3), cbind(c = 1:3, d = 3:1))",
                        cor(cbind(a = c(1, 1, 1), b = 1:3), cbind(c = 1:3, d = 3:1))))
cat("6c the constant row holds NA, not NaN: ",
    all(!is.nan(suppressWarnings(cor(cbind(a = c(1, 1, 1), b = 1:3), cbind(c = 1:3, d = 3:1))))),
    "\n", sep = "")

## 6d. Exact binary fractions, 5 x 2 against 5 x 3, so a port can pin the
## bits. R computes the covariance in two passes with a refined mean.
E1 <- matrix(c(0.5, -1.25, 2, 0.75, -0.5,
               1.5, 0.25, -2, 3.25, 0.5), nrow = 5,
             dimnames = list(NULL, c("e1", "e2")))
E2 <- matrix(c(-0.25, 1, 0.5, -1.5, 2.25,
               4, -0.75, 1.25, 0.5, -2,
               0.125, 0.375, -0.625, 1.75, 0.25), nrow = 5,
             dimnames = list(NULL, c("f1", "f2", "f3")))
report("6d E1", E1)
report("6d E2", E2)
report("6d cov(E1, E2)", cov(E1, E2))
report("6d cor(E1, E2)", cor(E1, E2))

## 6e. scale() on a small exact matrix. Column c is constant, so the default
## call divides by a standard deviation of zero and gives NaN.
m5 <- matrix(c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10, 1, 1, 1, 1, 1), 5,
             dimnames = list(NULL, c("a", "b", "c")))
report("6e m5", m5)

report_scale <- function(label, ...) {
  s <- scale(...)
  report(label, s)
  ctr <- attr(s, "scaled:center")
  scl <- attr(s, "scaled:scale")
  cat("scaled:center: ", if (is.null(ctr)) "absent" else fmtv(ctr), "\n", sep = "")
  cat("scaled:center names: ",
      if (is.null(ctr) || is.null(names(ctr))) "NULL" else paste(names(ctr), collapse = ", "),
      "\n", sep = "")
  cat("scaled:scale: ", if (is.null(scl)) "absent" else fmtv(scl), "\n", sep = "")
  cat("scaled:scale names: ",
      if (is.null(scl) || is.null(names(scl))) "NULL" else paste(names(scl), collapse = ", "),
      "\n", sep = "")
  nan_at <- which(is.nan(s))
  cat("NaN at column-major indices: ",
      if (length(nan_at) == 0) "none" else paste(nan_at, collapse = ", "), "\n", sep = "")
  invisible(s)
}

## The defaults center by the mean and divide by the standard deviation.
## R gives no warning for the constant column.
report_condition("6e scale(m5) defaults, warning if any",
                 report_scale("6e scale(m5) defaults", m5))

## With center = FALSE R divides by the root mean square, sqrt(sum(x^2) /
## (n - 1)), not by the standard deviation. Column c is then finite.
report_scale("6e scale(m5, center = FALSE)", m5, center = FALSE)

## With scale = FALSE R only centers, and reports no "scaled:scale".
report_scale("6e scale(m5, scale = FALSE)", m5, scale = FALSE)

## Explicit numeric vectors. R subtracts and divides by the values given and
## reports them back unchanged.
report_scale("6e scale(m5, center = c(1, 2, 3), scale = c(2, 2, 2))",
             m5, center = c(1, 2, 3), scale = c(2, 2, 2))
report_scale("6e scale(m5, center = TRUE, scale = c(1, 2, 4))",
             m5, center = TRUE, scale = c(1, 2, 4))

## 6f. scale() of the bundled data, both attributes in full and the first
## three rows of the result.
smd <- scale(md)
cat("\n---- 6f scale(moderation_data) defaults ----\n")
cat("dim: ", nrow(smd), " x ", ncol(smd), "\n", sep = "")
cat("colnames: ", paste(colnames(smd), collapse = ", "), "\n", sep = "")
cat("scaled:center: ", fmtv(attr(smd, "scaled:center")), "\n", sep = "")
cat("scaled:center names: ", paste(names(attr(smd, "scaled:center")), collapse = ", "), "\n", sep = "")
cat("scaled:scale: ", fmtv(attr(smd, "scaled:scale")), "\n", sep = "")
cat("scaled:scale names: ", paste(names(attr(smd, "scaled:scale")), collapse = ", "), "\n", sep = "")
cat("rows 1:3 column-major: ", fmtv(as.vector(smd[1:3, ])), "\n", sep = "")

## 6g. A center or a scale vector of the wrong length.
report_condition("6g scale(m5, center = c(1, 2))", scale(m5, center = c(1, 2)))
report_condition("6g scale(m5, scale = c(1, 2))", scale(m5, scale = c(1, 2)))

## ===========================================================================
## Section 7 — chol() and chol2inv()
## ===========================================================================
##
## chol() is LAPACK dpotrf. R returns the UPPER factor U, so A equals
## t(U) %*% U. chol2inv() is dpotri, which inverts A from that factor and
## costs less than a general LU inverse. chol() reads only the upper
## triangle of its argument and never looks at the lower one.
##
## Consumed by (TypeScript port): src/core/linalg/chol.test.ts

cat("\n==== Section 7: chol and chol2inv ====\n")

report_chol <- function(label, A) {
  U <- chol(A)
  report(paste0(label, " — chol(A), the upper factor U"), U)
  cat("max |t(U) %*% U - A|: ", fmt(max(abs(crossprod(U) - A))), "\n", sep = "")
  Ainv <- chol2inv(U)
  report(paste0(label, " — chol2inv(chol(A))"), Ainv)
  cat("max |A %*% chol2inv(chol(A)) - I|: ",
      fmt(max(abs(A %*% Ainv - diag(nrow(A))))), "\n", sep = "")
  invisible(U)
}

## 7a. A small positive definite matrix. solve() prints beside chol2inv() so
## a port can see how far the two inverses sit apart.
S3 <- matrix(c(4, 2, 2, 2, 5, 3, 2, 3, 6), 3)
report("7a S3", S3)
report_chol("7a S3", S3)
report("7a solve(S3), the LU inverse", solve(S3))
cat("7a max |chol2inv(chol(S3)) - solve(S3)|: ",
    fmt(max(abs(chol2inv(chol(S3)) - solve(S3)))), "\n", sep = "")

## 7b. The correlation matrix of the bundled data. Its dimnames travel into
## the factor.
R4 <- cor(md)
report("7b cor(moderation_data)", R4)
report_chol("7b cor(moderation_data)", R4)

## 7c. dimnames. chol() keeps both, and chol2inv() drops them.
D2 <- matrix(c(4, 2, 2, 3), 2, dimnames = list(c("p", "q"), c("p", "q")))
UD <- chol(D2)
report("7c chol(D2) with dimnames", UD)
report("7c chol2inv(chol(D2))", chol2inv(UD))

## 7d. Only the upper triangle is read. The 99 in the lower slot never
## reaches the factorization, so the factor equals the factor of the
## symmetric matrix that holds 2 there.
report("7d chol(matrix(c(4, 99, 2, 3), 2))", chol(matrix(c(4, 99, 2, 3), 2)))
report("7d chol(matrix(c(4, 2, 2, 3), 2)) for comparison", chol(matrix(c(4, 2, 2, 3), 2)))
cat("7d the two factors are identical: ",
    identical(chol(matrix(c(4, 99, 2, 3), 2)), chol(matrix(c(4, 2, 2, 3), 2))), "\n", sep = "")

## 7e. A 1 x 1.
report("7e chol(matrix(9))", chol(matrix(9)))
report("7e chol2inv(chol(matrix(9)))", chol2inv(chol(matrix(9))))

## 7f. What chol() and chol2inv() refuse.
report_condition("7f chol(matrix(c(1, 2, 2, 1), 2)) not positive definite",
                 chol(matrix(c(1, 2, 2, 1), 2)))
report_condition("7f chol(matrix(1:6, 3)) non-square", chol(matrix(1:6, 3)))
report_condition("7f chol(matrix(c(NA, 1, 1, 2), 2)) an NA", chol(matrix(c(NA, 1, 1, 2), 2)))

## chol2inv() does not refuse a tall matrix. Its `size` argument defaults to
## the column count, so it reads the leading 2 x 2 of the upper triangle and
## inverts that. Only a wide matrix, where size would exceed the row count,
## is an error. The port refuses both shapes.
report_condition("7f chol2inv(matrix(1:6, 3)) is 3 x 2 and R computes it",
                 report("7f chol2inv(matrix(1:6, 3))", chol2inv(matrix(1:6, 3))))
report_condition("7f chol2inv(matrix(1:6, 2)) is 2 x 3", chol2inv(matrix(1:6, 2)))
report_condition("7f chol2inv(matrix(c(NA, 1, 1, 2), 2)) an NA",
                 report("7f chol2inv with an NA", chol2inv(matrix(c(NA, 1, 1, 2), 2))))

## ===========================================================================
## Section 8 — predict.lm() over new data
## ===========================================================================
##
## predict.lm() rebuilds the design over the new frame with the terms of the
## fit, then multiplies it by the coefficients. A row that holds an NA gives
## NA. A column the terms need and the frame lacks is an error. A fit whose
## design was rank deficient still predicts, and R warns about it.
##
## Consumed by (TypeScript port): src/core/linalg/lm.test.ts

cat("\n==== Section 8: predict.lm ====\n")

nd <- data.frame(x = c(-1, 0, 0.5, 2, 1.25),
                 z = c(0.5, -0.25, 1, -1, 0),
                 w = c(2, 1, -0.5, 0.75, -1.5))
cat("\n---- 8 newdata nd ----\n")
cat("x: ", fmtv(nd$x), "\n", sep = "")
cat("z: ", fmtv(nd$z), "\n", sep = "")
cat("w: ", fmtv(nd$w), "\n", sep = "")

## 8a. The moderation model with a control, the fit of section 4a.
f4a <- lm(y ~ x * z + w, moderation_data)
report("8a coef(lm(y ~ x * z + w))", coef(f4a))
report("8a predict(lm(y ~ x * z + w), nd)", predict(f4a, nd))

## 8b. The no-intercept fit of section 4b.
f4g <- lm(y ~ x + z - 1, moderation_data)
report("8b coef(lm(y ~ x + z - 1))", coef(f4g))
report("8b predict(lm(y ~ x + z - 1), nd)", predict(f4g, nd))

## 8c. The moderation demo's own model, the fit of section 4h.
f4h <- lm(y ~ x * z, moderation_data)
report("8c coef(lm(y ~ x * z))", coef(f4h))
report("8c predict(lm(y ~ x * z), nd)", predict(f4h, nd))

## 8d. An NA in row 3. R returns NA for that row and keeps the row name.
nd_na <- nd
nd_na$x[3] <- NA
report_condition("8d predict with an NA in row 3 of x, warning if any",
                 report("8d predict(f4a, nd_na)", predict(f4a, nd_na)))

## 8e. A frame that lacks a column the terms need.
report_condition("8e predict(f4a, nd[, c(\"x\", \"z\")]) drops w",
                 predict(f4a, nd[, c("x", "z")]))

## 8f. The aliased fit of section 4c, whose x2 column is 2 * x. Its second
## slope is NA, and that coefficient adds nothing to a prediction.
##
## R 4.3 and later take `rankdeficient = "warnif"` by default. R then warns
## only when a row of the new design leaves the column space of the fitted
## design. A new frame that keeps x2 = 2 * x stays inside it, so R predicts
## in silence. A new frame that breaks the relation is doubtful, and R warns
## and marks the rows in an attribute. Both frames give the same numbers,
## because the aliased column never enters the product.
aliased <- data.frame(y = moderation_data$y, x = moderation_data$x, x2 = 2 * moderation_data$x)
f4c <- lm(y ~ x + x2, aliased)
report("8f coef(lm(y ~ x + x2)) aliased", coef(f4c))
nd_alias <- data.frame(x = nd$x, x2 = 2 * nd$x)
report_condition("8f predict on the aliased fit, x2 = 2 * x, no warning",
                 report("8f predict(f4c, nd_alias)", predict(f4c, nd_alias)))
nd_doubt <- data.frame(x = nd$x, x2 = c(-2, 0, 1, 3, 2.5))
report_condition("8f predict on the aliased fit, x2 free of x, R warns",
                 report("8f predict(f4c, nd_doubt)", predict(f4c, nd_doubt)))
cat("8f non-estim attribute of that prediction: ",
    paste(attr(suppressWarnings(predict(f4c, nd_doubt)), "non-estim"), collapse = ", "), "\n", sep = "")

## 8g. predict() with no newdata does NOT return fitted() bit for bit. R
## builds the design again and multiplies it by the coefficients, while
## fitted() holds what dqrsl returned during the fit. The two agree to about
## 1e-14 and differ in nearly every row.
report("8g predict(f4h)[1:5]", head(predict(f4h), 5))
report("8g fitted(f4h)[1:5]", head(fitted(f4h), 5))
cat("8g rows where predict(f4h) != fitted(f4h): ",
    sum(predict(f4h) != fitted(f4h)), " of ", nrow(moderation_data), "\n", sep = "")
cat("8g max |predict(f4h) - fitted(f4h)|: ",
    fmt(max(abs(predict(f4h) - fitted(f4h)))), "\n", sep = "")

## ===========================================================================
## Section 9 — elementwise arithmetic and outer()
## ===========================================================================
##
## R gives `+`, `-`, `*` and `/` on two matrices of the same extents. Each
## operator works entry by entry. `*` is the elementwise product and not the
## matrix product, which is `%*%`. A number on either side applies to every
## entry. R keeps the dimnames of the first operand when that operand has
## any, and takes the dimnames of the second operand otherwise.
##
## R recycles a vector against a matrix in silence when the length of the
## vector divides the entry count. R warns only when the length does not
## divide it. The port refuses every vector beside a matrix, so these cases
## record a stated narrowing and not a behavior to copy.
##
## outer(x, y) multiplies every entry of x by every entry of y. The result
## has length(x) rows and length(y) columns, and it equals tcrossprod(x, y).
## R names the rows and the columns of the result from the names of the two
## vectors. The port's vectors carry no names, so its outer() returns a
## matrix with no dimnames.
##
## Consumed by (TypeScript port): src/core/linalg/vector.test.ts,
##                                src/core/linalg/ops.test.ts

cat("\n==== Section 9: elementwise arithmetic and outer ====\n")

## 9a. Two matrices of exact binary fractions. Every entry of the result is
## one operation on one pair of doubles, so a port can pin the bits.
A9 <- matrix(c(1, 2, 3, 4, 5, 6), 3)
B9 <- matrix(c(0.5, -1, 2, 0.25, 4, -8), 3)
report("9a A", A9)
report("9a B", B9)
report("9a A + B", A9 + B9)
report("9a A - B", A9 - B9)
report("9a A * B", A9 * B9)
report("9a A / B", A9 / B9)

## 9b. A number on either side. The number reaches every entry. The order of
## the two operands matters for the operators that do not commute.
report("9b A + 2", A9 + 2)
report("9b A * 0.5", A9 * 0.5)
report("9b A / 4", A9 / 4)
report("9b 2 - A", 2 - A9)

## 9c. Dimnames. The first operand that carries dimnames gives them to the
## result. A number carries none, so the matrix keeps its own.
N1 <- A9
dimnames(N1) <- list(c("r1", "r2", "r3"), c("a", "b"))
N2 <- B9
dimnames(N2) <- list(c("p", "q", "s"), c("u", "v"))
report("9c N1", N1)
report("9c N2", N2)
report("9c N1 + N2 keeps the names of N1", N1 + N2)
report("9c N1 + B takes the names of N1", N1 + B9)
report("9c A + N2 takes the names of N2", A9 + N2)
report("9c N1 + 2 keeps the names of N1", N1 + 2)

## 9d. Two matrices of different extents.
report_condition("9d matrix(1:6, 3) + matrix(1:6, 2)",
                 matrix(1:6, 3) + matrix(1:6, 2))

## 9e. A vector beside a matrix. R recycles the vector down the column-major
## order and says nothing when the length divides the entry count. R warns
## when the length does not divide it, and it still returns a result. The
## port refuses all three of these calls.
report("9e matrix(1:6, 3) + 1:3", matrix(1:6, 3) + 1:3)
report("9e matrix(1:6, 3) + 1:2", matrix(1:6, 3) + 1:2)
report_condition("9e matrix(1:6, 3) + 1:4, a length that does not divide 6",
                 report("9e matrix(1:6, 3) + 1:4", matrix(1:6, 3) + 1:4))

## 9f. outer() of two unnamed vectors, and its agreement with tcrossprod().
report("9f outer(1:3, c(0.5, 2))", outer(1:3, c(0.5, 2)))
report("9f outer(c(-1, 2), c(3, 4, 5))", outer(c(-1, 2), c(3, 4, 5)))
cat("9f identical(outer(1:3, c(0.5, 2)), tcrossprod(1:3, c(0.5, 2))): ",
    identical(outer(1:3, c(0.5, 2)), tcrossprod(1:3, c(0.5, 2))), "\n", sep = "")
cat("9f identical(outer(c(-1, 2), c(3, 4, 5)), tcrossprod(c(-1, 2), c(3, 4, 5))): ",
    identical(outer(c(-1, 2), c(3, 4, 5)), tcrossprod(c(-1, 2), c(3, 4, 5))), "\n", sep = "")

## 9g. outer() of two named vectors. R takes the row names from x and the
## column names from y. The port's Vector carries no names, so its outer()
## returns no dimnames and a caller who wants them sets them afterward.
a9 <- c(x = 1, y = 2)
b9 <- c(u = 3, v = 4, w = 5)
report("9g outer(c(x = 1, y = 2), c(u = 3, v = 4, w = 5))", outer(a9, b9))
