# Conformance fixtures

These scripts generate the reference values a language port must reproduce to
be a conforming port of `compstatslib`. R is the specification: when a port
computes a regression, a non-central t density, a kernel density estimate or a
matrix inverse, the number it must produce is the number R produces here. The
scripts are the canonical generators — a port conforms to their output, not to
a copy of it.

The first consumer is the TypeScript port, whose test suite pins these values.
Each script names the test files that read it.

## Running one

From the package root:

```sh
Rscript conformance-fixtures/regression.R
```

Every script prints to standard output and writes nothing but the temporary
dataset export in `moderation.R`. Some sections source files from `R/` or load
a dataset from `data/`, so the working directory must be the package root.

## The scripts

| Script | Covers |
| --- | --- |
| `regression.R` | `plot_regression()`: coefficients, sums of squares, degenerate fits |
| `tdist.R` | `plot_t_test()`: central and non-central t, power, error matrix |
| `logit.R` | `plot_logit()`: `glm(binomial)` fits, separation, `glm.fit` internals |
| `ols.R` | `lm.fit()` / `lm.wfit()`: rank deficiency, pivoting, weights |
| `sampling.R` | `plot_sampling()` and `plot_sample_ci()`: `density()`, `hist()`, `pretty()`, confidence intervals |
| `pca.R` | `plot_pca()`: `prcomp()` and the arrow geometry, plus the `pca_degenerate` dump |
| `matrix-inverse.R` | `plot_matrix_inverse()`: `solve()`, both singularity failures, colours, draw order |
| `moderation.R` | `plot_moderation_3d()` and `plot_scatter3d()`: fits, prediction grid, validation, the `moderation_data` export |

`ols.R` is not a function family. It covers the solver that the regression
line, every IRLS step of the logit fit and the moderation surface all run on,
so its edge cases sit in one place instead of one family's fixtures.

Two sections skip themselves with a message when an optional package is
missing: the slider check in `matrix-inverse.R` needs `shiny`, and the
scatter3d section of `moderation.R` needs `plotly`. Both are package
dependencies, so a full development install runs everything.

## Precision

Values print through `sprintf("%.17g", x)`. Seventeen significant digits
round-trip an IEEE-754 double exactly, so a port can pin the same bits rather
than a rounded decimal. `pca.R` adds `%a` hex floats for the bundled dataset.

A value that looks wrong usually is not: `0.025000000000000001` is the correct
17-digit form of the double R holds for `0.025`, and `-2.9999999999999996` is
what `det()` returns for a matrix whose determinant is the integer −3, because
`det()` exponentiates a sum of logarithms.

## Verified under

R 4.5.3 (2026-03-11), arm64 macOS with R's reference BLAS. Two values in
`matrix-inverse.R` depend on the BLAS contracting a multiply-add; a build that
does not contract it differs in the last two digits.

## Changing a script

Editing a script invalidates the values every port has pinned, and moves the
conformance bar under ports that already met it. Change one only when this
package's own behavior changes — a new default, a corrected formula, a
different R version whose numbers move. Then re-run the script, update the
ports against its output, and say in the commit which values moved and why.

Adding a case is safer than editing one: a port that does not know about the
new case keeps passing, and a port that adopts it gains coverage.

## Scripts contributed by a port

A port may add a script here for a primitive this package never exports but
the port had to write — the TypeScript port's linear-algebra layer is the
first case. Such a script follows the conventions above, names the test files
that consume it, and gets a row in the table. It is committed straight to
`develop`, the same as any other contribution to this package.
