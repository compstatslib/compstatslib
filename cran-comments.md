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

This is a teaching package. The functions are built for in-class demonstration
and self-study of introductory computational statistics — regression, logistic
regression, t-tests, sampling distributions, confidence intervals, PCA, matrix
inverses, and moderation. The plotting functions are deliberately opinionated
about layout because they are meant to be read from the back of a lecture hall.
