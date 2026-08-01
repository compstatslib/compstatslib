<!-- markdownlint-disable MD025 -->

# compstatslib 0.8.0

First CRAN submission. This release settles the public API before the package
has any users to break, so it carries deliberate breaking changes.

## Breaking changes

* `plot_regr()` is renamed `plot_regression()`. Every other interactive/plot
  pair already matched its gadget name; this was the last one that did not.
  There is no deprecation shim — the package has never been released.

* `interactive_matrix_inverse()` drops the `_init` suffix from its arguments:
  `x1_init`, `y1_init`, `x2_init`, `y2_init` are now `x1`, `y1`, `x2`, `y2`.
  They now match `plot_matrix_inverse()`, so `do.call()` works between them.

* All eight `interactive_*()` gadgets return a new reproduction object when you
  click Done. Previously `interactive_matrix_inverse()` and
  `interactive_t_test()` returned nothing at all, and
  `interactive_moderation_3d()` returned the data frame while discarding the
  viewing angles the gadget exists to set. `interactive_pca()` no longer
  returns a two-element `list(points, pca)`; the points come back directly and
  the `prcomp` result is attached as an attribute.

## New features

* Every gadget prints the `plot_*()` call that reproduces what was on screen
  when you click Done, with arguments still at their defaults omitted. The
  returned object is a plain list (settings-style gadgets) or a plain data
  frame (points-style gadgets), so `do.call(plot_fn, result)`, `nrow()`, `[`
  and passing the result straight back to the plot function all work. See
  `?"compstatslib-reproduce"`.

* Derived state that a user would not retype rides along as an attribute
  rather than cluttering the printed call: `attr(result, "pca")` from
  `interactive_pca()`, `attr(result, "vars")` from `interactive_sampling()`.

* `interactive_t_test()` accepts `diff`, `sd`, `n`, `alpha` and `error_matrix`
  seed arguments, so it can be launched pre-configured like the other gadgets.

## Bug fixes

* `plot_regression()`, `plot_logit()` and `plot_sampling()` no longer leave the
  caller's `par()` settings modified, as CRAN Repository Policy requires. The
  first two used to force `par(family = "sans")` rather than restoring what the
  caller had set; all three now restore on exit, including when the call errors
  part-way through.

* `plot_sampling()` restores `cex` correctly. Its saved value was captured by
  the same `par()` call that set `mfrow`, and since `mfrow` resets `cex` as a
  side effect, the value it saved was already clobbered.

## Documentation

* Every exported function now documents its return value and carries runnable
  examples. Gadget examples use `if (interactive())` throughout and show the
  round trip from gadget to plot function.

* Package prose standardized on en-US spelling; `Language: en-US` declared.

* The README documented a `visualize_inverse()` function that does not exist.
  Removed, and a section on reproducing an interactive session added.

# compstatslib 0.7.1

* Minimum R version raised to 4.1.0.

# compstatslib 0.7.0

* Added `plot_scatter3d()` and `interactive_scatter3d()`: a 3D point cloud with
  runtime column pickers, aspect / opacity / marker-size controls, and camera
  capture so a specific rotation can be reproduced non-interactively.
* Added an R-CMD-check GitHub Actions workflow.

# compstatslib 0.6.0

* Added `plot_moderation_3d()` and `interactive_moderation_3d()` for visualizing
  moderated regression as a rotatable wireframe surface.
* `plot_moderation_3d()` supports multi-predictor models; pass `iv` and `mod` to
  choose which two predictors go on the plot axes.
* Bundled the synthetic `moderation_data` teaching dataset.

# compstatslib 0.5.1

* Documentation fixes: missing plot functions listed in the README, roxygen
  usage tags corrected.
