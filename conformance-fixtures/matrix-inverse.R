# Conformance reference values for the matrix-inverse family.
#
# Generates the expected values that language ports of compstatslib assert
# against for `plot_matrix_inverse()` / `interactive_matrix_inverse()`:
# `det()`, `solve()` and `rcond()` over the slider-reachable matrices, R's two
# distinct singularity failures and where the boundary between them sits, the
# three fill/border colours, and the drawing order — including the fact that a
# singular matrix draws nothing at all, because `solve()` throws before
# `plot()` is ever called.
#
# Consumed by (TypeScript port, compstatslib-ts):
#   src/core/matrix.test.ts
#   src/plot/matrixInverse.test.ts
#   src/interactive/matrixInverse.test.ts
#
# Originating fixture document: matrix-inverse-fixtures.md; section 1's driver
# is the script from the header of src/core/matrix.test.ts, which prints every
# field the document tabulates.
# Verified under: R 4.5.3 (2026-03-11), arm64 macOS with R's reference BLAS.
#
# Re-run with: Rscript conformance-fixtures/matrix-inverse.R  (from the package root)
#
# Values print at %.17g so ports can pin bit-exact doubles. Editing this
# script invalidates the pinned values in every port.
#
# Two numbers here depend on the BLAS: R's `det()` factors the matrix and
# exponentiates a sum of logarithms (F1's determinant is -2.9999999999999996,
# not -3), and the rank-one update in the factorization is contracted into a
# fused multiply-add on this build. A build without that contraction differs
# in the last two digits of F4a and F4c.

## ===========================================================================
## Section 1 — determinant, inverse, condition number, product
## ===========================================================================

options(digits = 17)
fmt <- function(x) sprintf("%.17g", x)
mk <- function(x1, y1, x2, y2) matrix(c(x1, y1, x2, y2), nrow = 2)
report <- function(label, x1, y1, x2, y2) {
  A <- mk(x1, y1, x2, y2)
  ## The matrix itself and the det(solve(A)) / 1/det(A) pair are printed here
  ## too; the fixture document records both and the test-file script did not.
  cat(label, " A =\n", sep = "")
  print(A, digits = 17)
  cat(label, " det=", fmt(det(A)), "\n", sep = "")
  tryCatch({
    s <- solve(A)
    cat("  solve [1,1],[1,2],[2,1],[2,2] = ",
        paste(fmt(c(s[1,1], s[1,2], s[2,1], s[2,2])), collapse = ", "), "\n",
        "  rcond = ", fmt(rcond(A, norm = "O")), "\n", sep = "")
    print(A %*% s, digits = 17)
    cat("  det(solve(A)) = ", fmt(det(s)), "  1/det(A) = ", fmt(1 / det(A)), "\n", sep = "")
  }, error = function(e) cat("  ERROR: ", conditionMessage(e), "\n", sep = ""))
}
report("F1",  1,    2,    2,    1)
report("F2",  1.3, -0.7,  0.4,  1.9)
report("F3", -1.5,  0.8,  0.6,  1.3)
report("F4a", -2,  -1.6, -1.5, -1.2)
report("F4b", -2,   -2,   -2,   -2)
report("F4c",  2,   1.9,  1.9,  1.8)
report("F5",   1,    1,    1,    1)
report("F6",   1,    0,    0,    1)
report("F7",   0,    1,    1,    0)

## Two more grid hits of F4a's shape (determinant exactly +/- one ulp of 1.0),
## and the inverse determinant of the default matrix, which the document
## records alongside 1/det(A) because the two land on adjacent doubles.
report("F4a'", -2, -1.6,  1.5,  1.2)
report("F4a''", -2, -1.5, -1.6, -1.2)

## F3 was chosen by hand because two suggested triples turned out to have
## positive determinants. The search log, for the record.
for (cand in list(c(-1.2, 0.5, 1.7, -1.4), c(-1.5, 0.8, 0.6, 1.3), c(0.9, 1.6, -1.1, 0.3))) {
  cat("candidate x1=", fmt(cand[1]), " y1=", fmt(cand[2]), " x2=", fmt(cand[3]),
      " y2=", fmt(cand[4]), " -> det=", fmt(det(mk(cand[1], cand[2], cand[3], cand[4]))),
      "\n", sep = "")
}

## ===========================================================================
## Section 2 — where R draws the singularity line
##
## A = [[1, 1], [1, 1 + eps]], so the real determinant is exactly eps.
## ===========================================================================

cat("\n=== SECTION 2: singularity threshold ===\n\n")
cat("solve.default's default tol = .Machine$double.eps = ",
    fmt(.Machine$double.eps), "\n\n", sep = "")

sweep_eps <- function(eps) {
  A <- matrix(c(1,1,1,1+eps), nrow=2)
  d <- det(A)
  r <- tryCatch(list(ok=TRUE, val=solve(A)),
                error=function(e) list(ok=FALSE, msg=conditionMessage(e)))
  ## Reporting added: the document quotes this function's printed output but
  ## abbreviated the printing itself to a comment.
  cat("eps=", fmt(eps), " fp_det=", fmt(d), " -> ", sep = "")
  if (r$ok) {
    cat("SUCCESS, solve()[1,1]=", fmt(r$val[1,1]), "\n", sep = "")
  } else {
    cat("ERROR: ", r$msg, "\n", sep = "")
  }
}

for (eps in c(0.1, 0.01, 0.001, 0.0001, 1e-08, 1e-12, 1e-14, 1e-15,
              1e-16, .Machine$double.eps, .Machine$double.eps / 2,
              2 * .Machine$double.eps, 5e-16, 2.5e-16,
              1e-17, 1e-18, 1e-300, 0)) {
  sweep_eps(eps)
}

cat("\n--- rcond closed form versus R's own rcond() ---\n\n")

norm1 <- function(M) max(colSums(abs(M)))
check_rcond <- function(label, A) {
  Ainv_manual <- matrix(c(A[2,2],-A[2,1],-A[1,2],A[1,1]), nrow=2, byrow=TRUE) / det(A)
  manual_rcond <- 1 / (norm1(A) * norm1(Ainv_manual))
  cat(label, "\n  manual 1/(norm1(A)*norm1(Ainv)) = ", fmt(manual_rcond),
      "\n  rcond(A, norm=\"O\")              = ", fmt(rcond(A, norm = "O")),
      "\n  error-message form (%g)          = ", sprintf("%g", rcond(A, norm = "O")),
      "\n", sep = "")
}
check_rcond("eps = .Machine$double.eps",
            matrix(c(1, 1, 1, 1 + .Machine$double.eps), nrow = 2))
check_rcond("eps = 4.4408920985006262e-16",
            matrix(c(1, 1, 1, 1 + 4.4408920985006262e-16), nrow = 2))
check_rcond("F4c (2, 1.9, 1.9, 1.8), not near-singular", mk(2, 1.9, 1.9, 1.8))
check_rcond("F4a (-2, -1.6, -1.5, -1.2)", mk(-2, -1.6, -1.5, -1.2))
check_rcond("four equal entries at -3.7", mk(-3.7, -3.7, -3.7, -3.7))
check_rcond("four equal entries at 1e-5", mk(1e-5, 1e-5, 1e-5, 1e-5))

cat("\n--- 2c: identical columns at several magnitudes ---\n\n")

for (v in c(1, 1e-5, 1e10, 0, -3.7)) {
  A <- matrix(c(v, v, v, v), nrow=2)  # identical columns at various magnitudes
  ## Reporting added, as above.
  msg <- tryCatch({ solve(A); "SUCCESS" }, error = function(e) conditionMessage(e))
  cat("v=", fmt(v), " det=", fmt(det(A)), " -> ", msg, "\n", sep = "")
}

cat("\n--- an infinite entry: R answers where its own rcond() refuses ---\n\n")
Ainf <- mk(Inf, 1, 1, 1)
cat("det = ", fmt(det(Ainf)), "\n", sep = "")
cat("solve() -> ",
    tryCatch(paste(fmt(as.vector(solve(Ainf))), collapse = ", "),
             error = function(e) conditionMessage(e)), "\n", sep = "")
cat("rcond() -> ",
    tryCatch(fmt(rcond(Ainf, norm = "O")),
             error = function(e) conditionMessage(e)), "\n", sep = "")

## ===========================================================================
## Section 3 — plot geometry
##
## Pinned by tracing the real graphics primitives on a null device, not by
## reading documentation.
## ===========================================================================

cat("\n=== SECTION 3: plot geometry ===\n\n")

cat("rgb(1,0,0,0.1) = ", rgb(1,0,0,0.1), "\n", sep = "")
cat("rgb(0,0,1,0.1) = ", rgb(0,0,1,0.1), "\n", sep = "")
cat("rgb(0,0,0,0.3) = ", rgb(0,0,0,0.3), "\n", sep = "")
print(col2rgb(rgb(1,0,0,0.1), alpha=TRUE))
print(col2rgb(rgb(0,0,1,0.1), alpha=TRUE))
print(col2rgb(rgb(0,0,0,0.3), alpha=TRUE))

cat("\narrows() defaults:\n")
print(formals(graphics::arrows)[c("length", "angle", "code", "col", "lty", "lwd")])

cat("\n--- xlab / ylab of plot(NA, xlim, ylim, frame.plot = FALSE) ---\n")
pdf(NULL)
trace(graphics::title, tracer = quote(cat("TRACE title() called with xlab=",
  deparse(xlab), " ylab=", deparse(ylab), "\n")), print=FALSE)
plot(NA, xlim=c(-3,3), ylim=c(-3,3), frame.plot = FALSE)
untrace(graphics::title)

cat("\n--- draw order: solve() before plot(), and nothing at all when singular ---\n")
source("R/matrix_inverse_plot.R")
trace(graphics::polygon, tracer = quote(cat("POLYGON x=", paste(sprintf("%.17g", x), collapse=","),
  " y=", paste(sprintf("%.17g", y), collapse=","), " col=", deparse(col), " border=", deparse(border), "\n")), print=FALSE)
trace(graphics::arrows, tracer = quote(cat("ARROWS x0=", x0, " y0=", y0, " x1=", x1, " y1=", y1, "\n")), print=FALSE)
trace(graphics::plot.default, tracer = quote(cat("PLOT.DEFAULT called: xlim=", deparse(xlim),
  " ylim=", deparse(ylim), " frame.plot=", deparse(frame.plot), "\n")), print=FALSE)

cat("=== Calling plot_matrix_inverse(1,2,2,1) (F1) ===\n")
plot_matrix_inverse(1, 2, 2, 1)   # F1
cat("\n=== Calling plot_matrix_inverse for singular (should error before any drawing) ===\n")
cat("ERRORED: ", tryCatch({ plot_matrix_inverse(1, 1, 1, 1); "no error" },
                          error = function(e) conditionMessage(e)), "\n", sep = "")

untrace(graphics::polygon); untrace(graphics::arrows); untrace(graphics::plot.default)
dev.off()

## ===========================================================================
## Section 4 — out-of-range slider values
##
## Needs shiny, which is a hard dependency of the package but not of Rscript.
## ===========================================================================

cat("\n=== SECTION 4: slider value out of range ===\n\n")

if (!requireNamespace("shiny", quietly = TRUE)) {
  message("shiny is not installed; skipping the slider-value section.")
} else {
  tag <- withCallingHandlers(
    shiny::sliderInput("x1", "x1", min = -2, max = 2, value = -5, step = 0.1),
    warning = function(w) { cat("WARNING:", conditionMessage(w), "\n"); invokeRestart("muffleWarning") }
  )
  print(tag)
  tag_high <- withCallingHandlers(
    shiny::sliderInput("x1", "x1", min = -2, max = 2, value = 5, step = 0.1),
    warning = function(w) { cat("WARNING:", conditionMessage(w), "\n"); invokeRestart("muffleWarning") }
  )
  print(tag_high)
  cat("\nshiny:::validate_slider_value, the source of those warnings:\n")
  print(shiny:::validate_slider_value)
  cat("\nThe rendered widget clamps: ion.rangeSlider's validate() sets",
      "o.from = o.min when o.from < o.min and o.from = o.max when it is above,",
      "so the slider a user sees starts at the nearest bound.\n")
}
