# CRAN comments

## Test environments

* macOS (local), R 4.5.3
* macOS-latest (GitHub Actions), R release
* windows-latest (GitHub Actions), R release
* ubuntu-latest (GitHub Actions), R release, R oldrel-1, R devel
* win-builder, R devel
* R-hub, CRAN-relevant platforms

## R CMD check results

0 errors | 0 warnings | 1 note

The note is:

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Soumya Ray <soumya.ray@gmail.com>'

New submission
```

This is a new submission, so that note is expected.

## Notes for the reviewer

### Examples wrapped in `if (interactive())`

Eight of this package's exported functions are Shiny gadgets
(`interactive_regression()`, `interactive_pca()`, and so on). They open a
`miniUI` gadget in the viewer pane and block until the user clicks Done or
Cancel, so they cannot run unattended in a check.

Their examples are therefore wrapped in `if (interactive())` rather than
`\dontrun{}`, so the code is still parsed and checked but is skipped in a
non-interactive session. Every other exported function has plain runnable
examples.

### Package purpose

The package covers two related things, which is why the exported functions vary
in how general they are.

`plot_scatter3d()` and `plot_moderation_3d()` (and their interactive
counterparts) are general-purpose visualization tools: they accept arbitrary
data frames and model formulas, and expose axis, color, aspect-ratio, and
camera / rotation control. The remaining functions demonstrate a concept —
sampling distributions, confidence intervals, t-statistics, matrix inversion —
by simulating it rather than by plotting user data, and are built for in-class
demonstration and self-study. Their layout and axis ranges are deliberately
opinionated because they are meant to be read from the back of a lecture hall.

Every interactive gadget returns, and prints, the `plot_*()` call that
reproduces its final view, so a session in the viewer pane can be turned into
reproducible code.
