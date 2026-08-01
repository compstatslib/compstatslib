# Conformance reference values for the moderation-surface and scatter3d families.
#
# Generates the expected values that language ports of compstatslib assert
# against for `plot_moderation_3d()` / `interactive_moderation_3d()` and
# `plot_scatter3d()` / `interactive_scatter3d()`: checksums of the bundled
# `moderation_data`, the three `lm()` fits, the 15 x 15 prediction grid and its
# `zlim`, `hold_value()`'s rules, `plot_scatter3d()`'s column selection and
# validation messages, and the structure of the plotly object it builds.
#
# Section 6 writes the tab-separated `%.17g` export of `moderation_data`.
# `src/data/moderationData.ts` in the TypeScript port was generated from that
# output and must never be regenerated with different values: the data was
# drawn under R's own RNG with `set.seed(42)`, so a JavaScript regeneration
# would silently change every default demo.
#
# Consumed by (TypeScript port, compstatslib-ts):
#   src/core/moderation.test.ts
#   src/core/frame.test.ts
#   src/data/moderationData.ts
#   src/plot/moderation3d.test.ts
#   src/plot/scatter3d.test.ts
#
# Originating fixture document: moderation-fixtures.md
# Verified under: R 4.5.3 (2026-03-11)
#
# Re-run with: Rscript conformance-fixtures/moderation.R   (from the package root)
#
# Values print at %.17g so ports can pin bit-exact doubles. Editing this
# script invalidates the pinned values in every port.
#
# Do not verify the exported TSV by reading it back with R's `read.table()`.
# R's decimal parser re-reads 120 of the 800 printed values one unit in the
# last place away from the doubles it printed. JavaScript engines parse all
# 800 back bit-exactly, which is what the export is for.

## ===========================================================================
## Section 1 — dataset checksums
## ===========================================================================

load("data/moderation_data.rda")
options(digits = 17)
fmt <- function(x) sprintf("%.17g", x)

for (col in c("y", "x", "z", "w")) {
  v <- moderation_data[[col]]
  cat("column:", col, "\n")
  cat("  n   =", length(v), "\n")
  cat("  sum =", fmt(sum(v)), "\n")
  cat("  mean=", fmt(mean(v)), "\n")
  cat("  sd  =", fmt(sd(v)), "\n")
  cat("  min =", fmt(min(v)), "\n")
  cat("  max =", fmt(max(v)), "\n")
}

cat("\nfirst row:\n")
row1 <- moderation_data[1, ]
cat(sprintf("y=%s x=%s z=%s w=%s\n", fmt(row1$y), fmt(row1$x), fmt(row1$z), fmt(row1$w)))

cat("\nlast row (200):\n")
row200 <- moderation_data[200, ]
cat(sprintf("y=%s x=%s z=%s w=%s\n", fmt(row200$y), fmt(row200$x), fmt(row200$z), fmt(row200$w)))

## ===========================================================================
## Section 2 — lm fits
##
## M1 is the default `plot_moderation_3d()` model; M2 drops the interaction;
## M3 adds `w` as a control, the shape the multi-predictor doc example uses
## with iv = "x", mod = "z".
## ===========================================================================

cat("\n=== SECTION 2: lm fits ===\n\n")

report_model <- function(label, formula) {
  cat("=====", label, "=====\n")
  fit <- lm(formula, data = moderation_data)
  s <- summary(fit)
  cat("coefficients (name = value):\n")
  cn <- names(coef(fit))
  for (nm in cn) {
    cat(sprintf("  %s = %s\n", nm, fmt(coef(fit)[[nm]])))
  }
  cat("residual standard error =", fmt(s$sigma), "\n")
  cat("df (residual) =", s$df[2], "\n")
  cat("df (model, excl intercept) =", s$df[1] - 1, "\n")
  cat("R-squared =", fmt(s$r.squared), "\n")
  cat("adj R-squared =", fmt(s$adj.r.squared), "\n")
  cat("model.matrix colnames:", paste(colnames(model.matrix(fit)), collapse=", "), "\n")
  fit
}

m1 <- report_model("M1 y ~ x * z", y ~ x * z)
fv1 <- fitted(m1)   # all 200 values dumped below

m2 <- report_model("M2 y ~ x + z", y ~ x + z)
fv2 <- fitted(m2)   # rows 1,2,50,100,200 dumped below

m3 <- report_model("M3 y ~ x + z + w + x:z", y ~ x + z + w + x:z)
fv3 <- fitted(m3)   # rows 1,2,50,100,200 dumped below

## The dumps the three comments above refer to. The document prints them in
## its results section; the loops were added so this script emits them.
cat("\nM1 fitted values, all 200:\n")
for (i in seq_along(fv1)) cat(i, ": ", fmt(fv1[[i]]), "\n", sep = "")

cat("\nM2 fitted values at rows 1, 2, 50, 100, 200:\n")
for (i in c(1, 2, 50, 100, 200)) cat(i, ": ", fmt(fv2[[i]]), "\n", sep = "")

cat("\nM3 fitted values at rows 1, 2, 50, 100, 200:\n")
for (i in c(1, 2, 50, 100, 200)) cat(i, ": ", fmt(fv3[[i]]), "\n", sep = "")

## ===========================================================================
## Section 3 — the prediction grid (the surface)
## ===========================================================================

cat("\n=== SECTION 3: prediction grid ===\n\n")

x <- moderation_data$x
z <- moderation_data$z
w <- moderation_data$w

seq_x <- seq(min(x), max(x), length.out = 15)
seq_z <- seq(min(z), max(z), length.out = 15)

grid_xz <- setNames(expand.grid(seq_x, seq_z), c("x", "z"))

m1 <- lm(y ~ x * z, data = moderation_data)
grid1 <- grid_xz
grid1$y <- predict(m1, grid1)
zlim1 <- range(c(moderation_data$y, grid1$y))

m2 <- lm(y ~ x + z, data = moderation_data)
grid2 <- grid_xz
grid2$y <- predict(m2, grid2)
zlim2 <- range(c(moderation_data$y, grid2$y))

m3 <- lm(y ~ x + z + w + x:z, data = moderation_data)
hold_w <- mean(w, na.rm = TRUE)
grid3 <- grid_xz
grid3$w <- hold_w
grid3$y <- predict(m3, grid3)
zlim3 <- range(c(moderation_data$y, grid3$y))

## Reporting added, as in section 2.
cat("min(x) = ", fmt(min(x)), "\nmax(x) = ", fmt(max(x)),
    "\nmin(z) = ", fmt(min(z)), "\nmax(z) = ", fmt(max(z)), "\n\n", sep = "")

cat("seq_x, 15 values:\n")
for (i in seq_along(seq_x)) cat(i, ": ", fmt(seq_x[i]), "\n", sep = "")
cat("\nseq_z, 15 values:\n")
for (i in seq_along(seq_z)) cat(i, ": ", fmt(seq_z[i]), "\n", sep = "")

cat("\nexpand.grid order, first 5 rows and row 16 (the IV varies fastest):\n")
for (i in c(1, 2, 3, 4, 5, 16)) {
  cat(i, ": x=", fmt(grid1$x[i]), " z=", fmt(grid1$z[i]), "\n", sep = "")
}

cat("\nM1 grid predictions, all 225:\n")
for (i in seq_len(nrow(grid1))) {
  cat(i, ": x=", fmt(grid1$x[i]), " z=", fmt(grid1$z[i]), " y=", fmt(grid1$y[i]), "\n", sep = "")
}
cat("\nzlim M1 = ", fmt(zlim1[1]), " ", fmt(zlim1[2]), "\n", sep = "")
cat("data range y = ", fmt(range(moderation_data$y)[1]), " ",
    fmt(range(moderation_data$y)[2]), "\n", sep = "")
cat("grid range y = ", fmt(range(grid1$y)[1]), " ", fmt(range(grid1$y)[2]), "\n", sep = "")

cat("\nM2 grid predictions at rows 1, 2, 15, 16, 113, 225:\n")
for (i in c(1, 2, 15, 16, 113, 225)) {
  cat(i, ": x=", fmt(grid2$x[i]), " z=", fmt(grid2$z[i]), " y=", fmt(grid2$y[i]), "\n", sep = "")
}
cat("zlim M2 = ", fmt(zlim2[1]), " ", fmt(zlim2[2]), "\n", sep = "")
cat("grid range y = ", fmt(range(grid2$y)[1]), " ", fmt(range(grid2$y)[2]), "\n", sep = "")

cat("\nM3 grid predictions at rows 1, 2, 15, 16, 113, 225 (w held at its mean):\n")
for (i in c(1, 2, 15, 16, 113, 225)) {
  cat(i, ": x=", fmt(grid3$x[i]), " z=", fmt(grid3$z[i]), " w=", fmt(grid3$w[i]),
      " y=", fmt(grid3$y[i]), "\n", sep = "")
}
cat("zlim M3 = ", fmt(zlim3[1]), " ", fmt(zlim3[2]), "\n", sep = "")
cat("grid range y = ", fmt(range(grid3$y)[1]), " ", fmt(range(grid3$y)[2]), "\n", sep = "")
cat("hold value w = mean(w) = ", fmt(hold_w), "\n", sep = "")

## Hand-summing grid row 1 of M3 from the coefficients, to confirm predict()
## used mean(w) as the hold value.
cf <- coef(m3)
manual_row1 <- cf[["(Intercept)"]] + cf[["x"]]*grid3$x[1] + cf[["z"]]*grid3$z[1] +
  cf[["w"]]*hold_w + cf[["x:z"]]*grid3$x[1]*grid3$z[1]
cat("manual row1 M3 prediction =", fmt(manual_row1), "\n")
cat("predict() row1 M3          =", fmt(grid3$y[1]), "\n")
cat("difference =", fmt(manual_row1 - grid3$y[1]), "\n")

## ===========================================================================
## Section 4 — hold_value() rules
## ===========================================================================

cat("\n=== SECTION 4: hold_value() ===\n\n")

source("R/moderation_3d_plot.R", local = TRUE)
options(digits = 17)
fmt <- function(x) sprintf("%.17g", x)

v <- c(1, 2, 3, 4, NA, 6)
cat("numeric hold_value =", fmt(hold_value(v)), "\n")   # mean(x, na.rm=TRUE)

f <- factor(c("b", "a", "c", "a"), levels = c("b", "a", "c"))
r <- hold_value(f)
cat("factor hold_value =", as.character(r), " levels=", paste(levels(r), collapse=","), "\n")

ch <- c("banana", "apple", "cherry")
cat("character hold_value =", hold_value(ch), "\n")   # sort(unique(x))[1]

lg <- c(TRUE, TRUE, FALSE)
cat("logical hold_value =", hold_value(lg), "\n")   # always FALSE

d <- as.Date(c("2020-01-01", "2020-06-15"))
res <- tryCatch(hold_value(d), error = function(e) conditionMessage(e))
cat("Date error =", res, "\n")

## ===========================================================================
## Section 5 — scatter3d column selection and validation
##
## Needs plotly, which is a hard dependency of the package but not of Rscript.
## ===========================================================================

cat("\n=== SECTION 5: scatter3d ===\n\n")

if (!requireNamespace("plotly", quietly = TRUE)) {
  message("plotly is not installed; skipping the scatter3d section.")
} else {

source("R/scatter3d_helpers.R", local = TRUE)
source("R/scatter3d_plot.R", local = TRUE)

num_cols <- scatter3d_numeric_cols(moderation_data)
cat("numeric columns:", paste(num_cols, collapse=", "), "\n")

msgs <- character(0)
p <- withCallingHandlers(
  plot_scatter3d(),
  message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
)
cat("captured message:", msgs, "\n")

cat("\n--- validation error messages ---\n\n")

try_err <- function(expr) tryCatch(expr, error = function(e) conditionMessage(e))

df_factor <- data.frame(a = 1:5, b = factor(letters[1:5]), c = 6:10, d = 11:15)
cat(try_err(plot_scatter3d(df_factor, x = "b", y = "a", z = "c")), "\n")

cat(try_err(plot_scatter3d(moderation_data, x = "nope", y = "x", z = "z")), "\n")

cat(try_err(plot_scatter3d(moderation_data, aspect = c(1, 2))), "\n")

cat(try_err(plot_scatter3d(moderation_data, aspect = c(1, -1, 1))), "\n")

cat(try_err(plot_scatter3d(moderation_data, opacity = 0)), "\n")

cat(try_err(plot_scatter3d(moderation_data, opacity = 1.5)), "\n")

cat(try_err(plot_scatter3d(moderation_data, size = 0)), "\n")

cat(try_err(plot_scatter3d(moderation_data, camera = "foo")), "\n")

df_2num <- data.frame(a = 1:5, b = factor(letters[1:5]), c = 6:10)
cat(try_err(plot_scatter3d(df_2num)), "\n")

cat("\n--- validation runs before plot_ly is ever called ---\n\n")

call_count <- 0
trace("plot_ly", where = asNamespace("plotly"),
      tracer = quote(call_count <<- call_count + 1), print = FALSE)

reset_and_report <- function(label, expr) {
  call_count <<- 0
  res <- tryCatch({ expr; "no error" }, error = function(e) conditionMessage(e))
  cat(sprintf("%s -> plot_ly call_count = %d ; result: %s\n", label, call_count, res))
}

reset_and_report("aspect wrong length", plot_scatter3d(moderation_data, aspect = c(1,2)))
reset_and_report("opacity 0", plot_scatter3d(moderation_data, opacity = 0))
reset_and_report("missing column", plot_scatter3d(moderation_data, x = "nope"))
reset_and_report("valid call (no error)", plot_scatter3d())
untrace("plot_ly", where = asNamespace("plotly"))

cat("\n--- the built plotly object, no arguments ---\n\n")

suppressMessages({ p <- plot_scatter3d() })
built <- plotly::plotly_build(p)$x

tr <- built$data[[1]]
cat("type:", tr$type, "\n")
cat("mode:", tr$mode, "\n")
cat("marker$opacity:", tr$marker$opacity, "\n")
cat("marker$size:", tr$marker$size, "\n")

scene <- built$layout$scene
str(scene)
cat("layout$uirevision:", built$layout$uirevision, "\n")
cat("length(x):", length(tr$x), " length(y):", length(tr$y), " length(z):", length(tr$z), "\n")
cat("names(built$layout):", paste(names(built$layout), collapse = ", "), "\n")
cat("marker$color:", tr$marker$color, "\n")

}

## ===========================================================================
## Section 6 — the dataset export
##
## Tab-separated, header y/x/z/w, every value %.17g. This is the file the
## TypeScript port's src/data/moderationData.ts was generated from. Writes to
## a temporary file and prints the first and last rows plus a checksum, so a
## re-run proves the export unchanged without dumping 800 numbers twice.
## ===========================================================================

cat("\n=== SECTION 6: moderation_data export ===\n\n")

export_path <- file.path(tempdir(), "moderation-data.tsv")
con <- file(export_path, "w")
cat("y\tx\tz\tw\n", file = con, sep = "")
for (i in seq_len(nrow(moderation_data))) {
  cat(fmt(moderation_data$y[i]), "\t", fmt(moderation_data$x[i]), "\t",
      fmt(moderation_data$z[i]), "\t", fmt(moderation_data$w[i]), "\n",
      file = con, sep = "")
}
close(con)

exported <- readLines(export_path)
cat("wrote ", export_path, ": ", length(exported), " lines\n", sep = "")
cat("header: ", exported[1], "\n", sep = "")
cat("row 1:  ", exported[2], "\n", sep = "")
cat("row 200:", exported[201], "\n", sep = "")
cat("md5:    ", tools::md5sum(export_path)[[1]], "\n", sep = "")

## ===========================================================================
## Section 7 — extra cases computed in session for the TypeScript port
##
## From the header of src/core/moderation.test.ts: a degenerate seq() and a
## rank-deficient design, to pin what an aliased coefficient does to the
## surface.
## ===========================================================================

cat("\n=== SECTION 7: cases computed in session ===\n\n")

cat("seq(3, 3, length.out = 15) = ", paste(fmt(seq(3, 3, length.out = 15)), collapse = ", "), "\n", sep = "")

d <- data.frame(y = c(3,5,4,8,10,9), x = c(1,2,3,4,5,6), z = c(2,1,4,3,6,5))
d$dup <- d$x
cf_dup <- coef(lm(y ~ x + z + dup + x:z, data = d))     # dup aliases to NA
for (nm in names(cf_dup)) {
  cat("  ", nm, " = ", if (is.na(cf_dup[[nm]])) "NA" else fmt(cf_dup[[nm]]), "\n", sep = "")
}
