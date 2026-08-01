
<!-- README.md is generated from README.Rmd. Please edit that file -->

# compstatslib

<!-- badges: start -->

[![R-CMD-check](https://github.com/soumyaray/compstatslib/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/soumyaray/compstatslib/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This R Package is a collection of interactive tools and helper functions
that help teachers and learners learn concepts in computational
statistics. Useful for homework, in-class demonstrations, or
self-learning.

## Major Functions

Three types of functions are made available:

- **Interactive functions** let you use your mouse and/or keyboard to
  interact with a visualization of a technique (e.g., regression, PCA)
- **Plot functions** functions produce plots of data, distributions, or
  particular statistical concepts (often a non-interactive counterpart
  to an interactive function)
- **Code functions** are provided as examples of code that one might
  want to see (they are executable, but the major value is in seeing
  their code)

### Statistical Tests

- `interactive_t_test()` Interactive visualization function that will
  show you a simulation of null and alternative distributions of the
  t-statistic. You will be able to play with the different parameters
  that affect hypothesis tests in order to see how their variation
  influences the null t and alternative t distributions, as well as
  statistical power.
- `plot_t_test()` Non-interactive visualization that plots null and
  alternative t distributions of a t-test. Shows the rejection zone and
  statistical power as shaded areas under the curves. Accepts parameters
  for the test difference, standard deviation, sample size, significance
  level, and an optional type I/II error matrix overlay.

### Statistical Sampling

- `interactive_sampling()` Interactive sampling simulation that will
  sample given population data to show how a sampling statistic is
  distributed across repetitions of sampling exercise.
- `plot_sampling()` Plotting function that shows the distribution of a
  population alongside samples drawn from it and the distribution of a
  given sampling statistic (e.g., mean or median).
- `plot_sample_ci()` Simulated visualization of samples drawn from a
  given population function, with each sample’s confidence intervals
  displayed.

### Linear Regression

- `interactive_regression()` Interactive visualization function that
  lets you point-and-click to add data points, while it automatically
  plots and updates a regression line and associated statistics.
- `plot_regression()` Plotting function that takes a dataframe of points
  (x, y) and plots them with a regression line and associated
  statistics.

### Logistic Regression

- `interactive_logit()` Interactive visualization function that lets you
  point-and-click to add data points, while it automatically plots and
  updates a logistic regression line and associated statistics.
- `plot_logit()` Plotting function that takes a dataframe of points
  (x, y) and plots them with a logistic regression curve and associated
  statistics.

### Moderation (Interaction Effects)

- `interactive_moderation_3d()` Interactive Shiny gadget that fits a
  moderated regression and renders the fitted surface as a rotatable 3D
  wireframe. Two sliders control the viewing angle so students can see
  how an interaction term twists the surface relative to an additive
  (planar) model.
- `plot_moderation_3d()` Non-interactive counterpart that returns a
  `lattice::wireframe` trellis object for the moderation surface.
  Accepts any model formula (`y ~ x * z`, `y ~ x + z`, or larger models
  with extra controls — pass `iv` and `mod` to choose which two
  predictors are plotted; the rest are held at typical values).
- `moderation_data` Bundled synthetic dataset used as the default
  example for the two functions above; calibrated to make the
  interaction effect visually obvious. Includes an unrelated noise
  variable `w` for demonstrating multi-predictor formulas.

### 3D Scatterplots

- `interactive_scatter3d()` Interactive Shiny gadget for exploring three
  numeric columns of a data frame as a rotatable 3D point cloud. Column
  pickers swap x / y / z (and an optional colour mapping) at runtime;
  aspect, opacity, and marker-size sliders tune the view. Rotation and
  zoom persist across slider/picker changes within the gadget. On Done,
  prints a reproducible `plot_scatter3d(...)` call to the console —
  including the captured camera position — so the exact rotation and
  zoom can be pasted into an Rmd or script.
- `plot_scatter3d()` Non-interactive counterpart that returns a `plotly`
  htmlwidget for a 3D scatterplot of three numeric columns. Supports
  optional colour mapping (numeric → continuous scale; factor /
  character → discrete palette), aspect-ratio control, marker opacity /
  size, custom axis titles, and an explicit `camera` argument for
  reproducing a specific view captured from the gadget.

### Principal Components Analysis

- `interactive_pca()` Interactive visualization function that lets you
  point-and-click to add data points, while it automatically plots and
  updates principal component vectors.
- `plot_pca()` Plotting function that takes a dataframe of points (x, y)
  and plots them with their principal component vectors. Supports
  optional mean-centering.

### Precision

- `machine_precision()` Code function that shows how to find the
  smallest number your computer can effectively represent

### Linear Algebra

- `interactive_matrix_inverse()` Interactive function that allows one to
  *manipulate* a matrix inversion.
- `plot_matrix_inverse()` Plotting function that visualizes a matrix and
  its inverse as vector pairs, showing their geometric relationship.
- `visualize_inverse()` Plotting function that helps visual an inverse.

## Installation

You can install the current development version from
[GitHub](https://github.com/) using the `devtools` package:

``` r
# install.packages("devtools")
devtools::install_github("soumyaray/compstatslib")
```

Feel free to send open issues or send pull requests. Happy hacking!
