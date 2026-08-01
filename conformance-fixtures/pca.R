# Conformance reference values for the PCA family.
#
# Generates the expected values that language ports of compstatslib assert
# against for `plot_pca()` / `interactive_pca()`: `prcomp()`'s standard
# deviations, rotation, centre and scores, the `vec` matrix `plot_pca()` builds
# from them, and the arrow endpoints it draws — over the bundled
# `pca_degenerate` dataset, a general eight-point set, and the degenerate
# collinear / identical / constant-column cases. The dataset dump doubles as
# the export the ports embed.
#
# Rotation column signs are LAPACK's, not R's, and `?prcomp` says outright
# that they may differ between builds. A port that computes its own
# eigendecomposition should assert up to a whole-column sign flip.
#
# Consumed by (TypeScript port, compstatslib-ts):
#   src/core/pca.test.ts
#   src/data/pcaDegenerate.ts   (the dataset dump below)
#
# Originating fixture document: pca-fixtures.md
# Verified under: R 4.5.3 (2026-03-11)
#
# Re-run with: Rscript conformance-fixtures/pca.R   (from the package root)
#
# Values print at %.17g (plus %a hex floats for the dataset) so ports can pin
# bit-exact doubles. Editing this script invalidates the pinned values in
# every port.

options(digits = 17)
fmt <- function(x) sprintf("%.17g", x)
hexfmt <- function(x) sprintf("%a", x)

dump_vec <- function(label, v) {
  cat(label, ": [", paste(fmt(v), collapse=", "), "]\n", sep="")
}
dump_hex <- function(label, v) {
  cat(label, " (hex): [", paste(hexfmt(v), collapse=", "), "]\n", sep="")
}
dump_mat <- function(label, m) {
  cat(label, " dim=", nrow(m), "x", ncol(m), " rownames=", paste(rownames(m), collapse=","),
      " colnames=", paste(colnames(m), collapse=","), "\n", sep="")
  for (i in seq_len(nrow(m))) {
    cat("  row ", i, " (", rownames(m)[i], "): [", paste(fmt(m[i,]), collapse=", "), "]\n", sep="")
  }
}

pca_fixture <- function(points, meancenter = TRUE, label = "") {
  cat("=====", label, "=====\n")
  cat("nrow:", nrow(points), "\n")
  if (nrow(points) == 0) {
    cat("plot_pca: nrow==0 -> plot(NA,...) branch, returns invisible(NULL). No prcomp call.\n")
    return(invisible(NULL))
  }
  if (meancenter && nrow(points) > 1) {
    mc_diff <- sapply(points, mean)
    mc_points <- sweep(points, 2, mc_diff)
  } else {
    mc_diff <- c(x = 0, y = 0)
    mc_points <- points
  }
  dump_vec("mc_diff$x,y", c(mc_diff["x"], mc_diff["y"]))
  cat("mc_points:\n")
  print(mc_points, digits=17)

  if (nrow(points) < 3) {
    cat("plot_pca: nrow<3 -> no prcomp call (guard). Points-only branch.\n")
    return(invisible(NULL))
  }

  pca <- prcomp(mc_points, scale. = FALSE)
  cat("--- prcomp result ---\n")
  dump_vec("sdev", pca$sdev)
  dump_hex("sdev", pca$sdev)
  cat("center:", paste(fmt(pca$center), collapse=", "), "\n")
  cat("scale:", pca$scale, "\n")
  dump_mat("rotation", pca$rotation)
  cat("x (scores):\n")
  for (i in seq_len(nrow(pca$x))) {
    cat("  row", i, ": [", paste(fmt(pca$x[i,]), collapse=", "), "]\n")
  }

  egvec <- pca$rotation[, c("PC1", "PC2")]
  sv <- pca$sdev[1:2]
  vec <- egvec %*% diag(sv)
  rownames(vec) <- c("x", "y")
  colnames(vec) <- c("PC1", "PC2")
  dump_mat("vec", vec)

  from_pc1 <- c(x = -vec["x","PC1"] + mc_diff["x"], y = -vec["y","PC1"] + mc_diff["y"])
  to_pc1   <- c(x =  vec["x","PC1"] + mc_diff["x"], y =  vec["y","PC1"] + mc_diff["y"])
  from_pc2 <- c(x = -vec["x","PC2"] + mc_diff["x"], y = -vec["y","PC2"] + mc_diff["y"])
  to_pc2   <- c(x =  vec["x","PC2"] + mc_diff["x"], y =  vec["y","PC2"] + mc_diff["y"])
  cat("arrow PC1 from: [", paste(fmt(from_pc1), collapse=", "), "]\n")
  cat("arrow PC1 to:   [", paste(fmt(to_pc1), collapse=", "), "]\n")
  cat("arrow PC2 from: [", paste(fmt(from_pc2), collapse=", "), "]\n")
  cat("arrow PC2 to:   [", paste(fmt(to_pc2), collapse=", "), "]\n")

  invisible(pca)
}

cat("################ F1: pca_degenerate ################\n")
load("data/pca_degenerate.rda")
cat("Full dataset dump (17 sig digits):\n")
for (i in seq_len(nrow(pca_degenerate))) {
  cat("  row", i, ": x=", fmt(pca_degenerate$x[i]), " y=", fmt(pca_degenerate$y[i]),
      "  (hex x=", hexfmt(pca_degenerate$x[i]), " y=", hexfmt(pca_degenerate$y[i]), ")\n", sep="")
}
pca_fixture(pca_degenerate, meancenter = TRUE, label = "F1 pca_degenerate meancenter=TRUE")

cat("\n################ F2: general 8-point set ################\n")
f2 <- data.frame(x = c(-30, -18, -7, 2, 9, 17, 26, 38),
                  y = c(-25, -11, -9, 3, 2, 15, 18, 30))
pca_fixture(f2, meancenter = TRUE, label = "F2 general 8pt meancenter=TRUE")

cat("\n################ F3: collinear 3 points ################\n")
f3 <- data.frame(x = c(0, 10, 20), y = c(0, 5, 10))
pca_fixture(f3, meancenter = TRUE, label = "F3 collinear meancenter=TRUE")

cat("\n################ F4: 3 identical points ################\n")
f4 <- data.frame(x = c(5,5,5), y = c(5,5,5))
pca_fixture(f4, meancenter = TRUE, label = "F4 identical meancenter=TRUE")

cat("\n################ F5: constant-x set ################\n")
f5 <- data.frame(x = c(3,3,3,3), y = c(-8,-1,4,12))
pca_fixture(f5, meancenter = TRUE, label = "F5 constant-x meancenter=TRUE")

cat("\n################ F6: n=2 ################\n")
f6 <- data.frame(x = c(1,4), y = c(2,6))
cat("Directly test prcomp on n=2 (bypassing plot_pca's guard), meancentered manually:\n")
mc_diff6 <- sapply(f6, mean)
mc6 <- sweep(f6, 2, mc_diff6)
cat("mc_points:\n"); print(mc6, digits=17)
pca6 <- prcomp(mc6, scale.=FALSE)
dump_vec("sdev", pca6$sdev)
cat("center:", paste(fmt(pca6$center), collapse=", "), "\n")
dump_mat("rotation", pca6$rotation)
cat("x (scores):\n"); print(pca6$x, digits=17)
cat("plot_pca's own guard: nrow(points) < 3 --> no prcomp call at all for n=2 through plot_pca.\n")

cat("\n################ F7: F2 with meancenter=FALSE ################\n")
pca_fixture(f2, meancenter = FALSE, label = "F7 F2 meancenter=FALSE")

cat("\n################ F8: 1 point and 0 points ################\n")
f8a <- data.frame(x = 7, y = -3)
pca_fixture(f8a, meancenter = TRUE, label = "F8a one point (7,-3)")
f8b <- data.frame(x = numeric(0), y = numeric(0))
pca_fixture(f8b, meancenter = TRUE, label = "F8b zero points")

## ---------------------------------------------------------------------------
## Degenerate cases raise no condition
##
## `options(warn = 2)` promotes warnings to errors, so a clean run proves the
## absence of even a suppressed warning for the collinear, identical and
## constant-column inputs.
## ---------------------------------------------------------------------------

cat("\n################ warn = 2 re-run of F3 / F4 / F5 ################\n")

options(warn = 2)
f4 <- data.frame(x = c(5,5,5), y = c(5,5,5))
mc <- sweep(f4, 2, sapply(f4, mean))
res <- tryCatch(prcomp(mc, scale.=FALSE),
                 warning=function(w) paste("WARN:", conditionMessage(w)),
                 error=function(e) paste("ERR:", conditionMessage(e)))
print(res)

f3 <- data.frame(x = c(0, 10, 20), y = c(0, 5, 10))
mc3 <- sweep(f3, 2, sapply(f3, mean))
res3 <- tryCatch(prcomp(mc3, scale.=FALSE),
                  warning=function(w) paste("WARN:", conditionMessage(w)),
                  error=function(e) paste("ERR:", conditionMessage(e)))
print(class(res3))

f5 <- data.frame(x = c(3,3,3,3), y=c(-8,-1,4,12))
mc5 <- sweep(f5, 2, sapply(f5, mean))
res5 <- tryCatch(prcomp(mc5, scale.=FALSE),
                  warning=function(w) paste("WARN:", conditionMessage(w)),
                  error=function(e) paste("ERR:", conditionMessage(e)))
print(class(res5))
cat("no errors/warnings thrown for degenerate/collinear/constant cases with scale.=FALSE (default tol=NULL)\n")

options(warn = 0)

## ---------------------------------------------------------------------------
## prcomp() below plot_pca()'s guard
##
## From the header of src/core/pca.test.ts in the TypeScript port, which ran
## these while deciding what a core `principalComponents()` should do with one
## row and with none. `plot_pca()` never reaches either call.
## ---------------------------------------------------------------------------

cat("\n################ prcomp() called directly on 1 and 0 rows ################\n")

pca1 <- prcomp(data.frame(x = 7, y = -3))
dump_vec("n = 1: sdev", pca1$sdev)
dump_mat("n = 1: rotation", pca1$rotation)
dump_vec("n = 1: center", pca1$center)
cat("n = 1: scores: [", paste(fmt(pca1$x), collapse=", "), "]\n", sep="")

cat("n = 0: ",
    tryCatch({ prcomp(data.frame(x = numeric(0), y = numeric(0))); "no error" },
             error = function(e) conditionMessage(e)), "\n", sep = "")
