# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`compstatslib` is an educational R package providing interactive visualizations and plotting functions for teaching computational statistics concepts. It uses base R graphics and the `manipulate` package for interactivity within RStudio.

## Build & Development Commands

```r
devtools::load_all()      # Load package for development
devtools::document()      # Regenerate NAMESPACE and man/*.Rd from roxygen2 comments
devtools::test()          # Run testthat suite
devtools::check()         # Full R CMD check
```

Run a single test file:

```r
testthat::test_file("tests/testthat/test-plots.R")
```

Install from GitHub:

```r
devtools::install_github("soumyaray/compstatslib")
```

## Architecture

Functions follow a paired pattern: **interactive** + **plot** versions.

- **Interactive functions** (`*_interactive.R`): Use `manipulate::manipulate()` for sliders/pickers and `locator()` for mouse-click data entry. These only work in RStudio.
- **Plot functions** (`*_plot.R`): Standalone visualization functions that accept data and parameters directly. These are the testable, non-interactive counterparts.

Current function pairs: regression, logit, t-test, sampling, sample CI, matrix inverse, PCA.

Standalone utility: `precision.R` (machine precision demonstration).

## Related packages

`../compstatslib-ts` is a TypeScript/browser port of this package, published to
npm as `@compstats/core` (repo: `compstatslib/compstatslib-ts`). It covers the
same nine function families in three layers — `src/core/` (statistics, no DOM),
`src/plot/` (Canvas 2D and Plotly renderers), `src/interactive/` (components).

**This R package is canonical for the statistics and the plotting logic.** The
port follows it, and asserts its core math against fixtures generated here in
`conformance-fixtures/`. Its `CHANGELOG.md` states every deliberate departure.

Because the port re-implements R's primitives by hand, it sometimes finds
defects in this package's logic. When either side changes behavior, check the
other:

- Fixes made here should be mirrored in the port (it tracks R's rules).
- Fixes made there may be backports due here — the port's changelog entries
  that read "R does X, this follows / this departs" are the ones to read.

## Key Conventions

- **R >= 4.1.0** required.
- **Base R graphics only** — no ggplot2.
- **roxygen2** generates all documentation: never edit `NAMESPACE` or `man/*.Rd` files directly.
- **README.md** is generated from `README.Rmd` — edit `README.Rmd` only, then run `devtools::build_readme()` to regenerate. Never edit `README.md` directly.
- **testthat edition 3** for tests.
- **Markdown linting** — after editing or creating any `.md` file, run `/ray-md-lint` to ensure it is lint-free.
- Build options: `--no-multiarch --with-keep.source`

## Git Workflow

- **main**: release branch
- **develop**: active development branch
- Feature branches branch from and merge back to `develop`
- **Commits arriving from the port** — a Claude Code session in
  `../compstatslib-ts` may commit here, normally to add a script under
  `conformance-fixtures/` and its row in that directory's README. Such commits
  go **directly on `develop`**: not on a feature branch, and never on `main`.
  They add cases and scripts; they do not edit existing scripts (see
  `conformance-fixtures/README.md`, "Changing a script").
