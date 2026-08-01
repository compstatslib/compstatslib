# Future work

Planning notes for features deferred from past branches. Each entry is a
seed, not a spec — flesh out before starting work.

## `plot_moderation()` — 2D companion to `plot_moderation_3d()`

### Motivation

`plot_moderation_3d()` requires both the IV and the moderator to be
numeric. Categorical (especially binary) moderators are common in the
applied literature — treatment vs. control, male vs. female, before vs.
after — and the standard textbook visualization for them is a 2D plot
with one regression line per moderator level on shared axes. Adding a
2D companion lets the package cover both common moderation cases with a
consistent API.

No interactive version is needed. The interactivity in
`interactive_moderation_3d()` exists to expose viewing angles of a 3D
surface; a 2D plot has no analogous control surface worth exposing.

### Sketch

```r
plot_moderation(
  formula = y ~ x * z,
  data    = moderation_data,
  iv      = NULL,    # required when formula has 3+ predictors
  mod     = NULL,    # required when formula has 3+ predictors
  mod_at  = NULL,    # see "Open questions"
  ...
)
```

Behavior by moderator type:

- **Binary or factor moderator**: one regression line per level, drawn
  in distinct colors with a legend. The IV varies along the x-axis; the
  DV is on the y-axis.
- **Continuous moderator**: three regression lines at "low / mean /
  high" slices of the moderator. The Aiken & West convention is
  mean ± 1 SD; an alternative is quartiles (25th / 50th / 75th
  percentile).

Other predictors (when the formula has more than IV and moderator) are
held at their typical values, identical to `plot_moderation_3d()`.
Console message follows the same one-line convention.

### Open questions

1. **Slice selection for continuous moderators.** Aiken & West
   (mean ± 1 SD) is the most-cited convention, but quartiles or
   user-specified values are common too. Default to mean ± 1 SD and
   accept a `mod_at` numeric vector to override?
2. **Categorical moderator with 3+ levels.** Plot one line per level
   (could get crowded above ~5 levels). Cap or warn?
3. **Confidence bands.** Skip in v1 (matches the 3D version's no-CI
   behavior), or include a translucent band per slope from
   `predict.lm(..., interval = "confidence")`? This is the single
   biggest "publication-ready" gap in the 3D version too.
4. **Plotting backend.** Base R graphics (per package convention) is
   sufficient — `plot()` for the points, `abline()` per slope, custom
   legend. No new dependency required.
5. **Naming.** `plot_moderation()` (no suffix) reads naturally as the
   default 2D case, with `plot_moderation_3d()` as the special-purpose
   3D variant. Alternative: `plot_moderation_2d()` for symmetry.
   Recommend the no-suffix form.

### Out of scope (for the branch that builds this)

- Interactive Shiny gadget version (no useful slider).
- Higher-order moderation (3-way interactions).
- Standardized vs. raw coefficient toggle.
- Johnson-Neyman regions of significance overlay.

### Related

- Current `plot_moderation_3d()` rejects categorical iv/mod with a
  clear error pointing nowhere — this function is the eventual
  destination for that case.
- The `moderation_data` bundled dataset (continuous `x`, `z`, `w`)
  works as the default example for the continuous-moderator branch;
  the binary-moderator branch needs either a small bundled binary
  dataset or a `factor(z > 0)` derivation in the examples.

## Accept string **and** NSE column references across model-relevant functions

### Motivation

Most plotting/model functions in the package currently accept column
references as character strings (e.g. `plot_scatter3d(df, x = "hj")`). Strings
are simple, easy to validate, and trivial to pass programmatically. The
deferred alternative is **formula-style NSE**: `plot_scatter3d(df, x = ~hj)`,
matching plotly / lattice / lme4 idioms. NSE's one real functional gain
is inline expressions — `x = ~log(hj)`, `x = ~hj / X110h` — letting
users transform on the fly without pre-computing a column. For a
teaching package this is a double-edged sword (pre-computing makes the
transform visible in user code), but it's a real ergonomic gain for
report writers porting plotly snippets from the wild.

This is a package-wide ergonomics question, not a per-function one —
should be designed once across all model/plot entry points so users
don't have to remember which functions accept which form.

### Sketch

Each affected function accepts either form for its column arguments:

```r
plot_scatter3d(df, x = "hj",      y = "X100m",    z = "X110h")     # string
plot_scatter3d(df, x = ~hj,       y = ~X100m,     z = ~X110h)      # NSE name
plot_scatter3d(df, x = ~log(hj),  y = ~X100m,     z = ~X110h)      # NSE expr
```

A small internal helper would normalize either form: if the arg is a
one-sided formula, evaluate its RHS in the `data` environment and use
the result; if it's a character, treat it as a column name. Validation
("column exists / column is numeric") works for the string form; for
the NSE form, validation reduces to "the expression evaluates without
error and yields a numeric vector of length `nrow(data)`."

### Affected functions (candidate list)

`plot_scatter3d()`, `interactive_scatter3d()`, `plot_logit()`, `plot_regression()`,
`plot_pca()`, `plot_moderation()` (if/when added), and any future
visualization that takes column references. Functions that already
take a `formula` argument (e.g. `plot_moderation_3d()`) are out —
their formula is a model spec, not column-picking.

### Open questions

1. **Internal helper API.** A single `.resolve_col(arg, data, name)`
   that returns a numeric vector and a display label (for axis titles)
   would centralize the dispatch. Implementation choice: base R
   (`is.character()` / `inherits(arg, "formula")` / `eval(arg[[2]],
   data)`) is sufficient — no need to add `rlang`.
2. **Display labels.** For `x = "hj"` the axis title is obvious. For
   `x = ~log(hj)` it should probably be `"log(hj)"` — i.e.
   `deparse(arg[[2]])`. Confirm this reads sensibly for compound
   expressions.
3. **Programmatic-use ergonomics.** Users holding a column name in a
   variable should still have a clean path: `plot_scatter3d(df, x = colname)`
   when `colname <- "hj"` already works under the string branch. Make
   sure adding NSE doesn't break or shadow that.
4. **Documentation cost.** Every `@param` for a column-reference
   argument needs to document both forms with an example. Non-trivial
   doc churn — worth doing in one sweep, not piecemeal.
5. **Ordering.** Roll out alongside or after a 1.0 release? This is a
   surface-area change to many user-facing functions; ideally bundled
   into one minor version with a clear changelog entry.

### Out of scope

- Tidy-eval / `rlang` quosures. Base R's formula handling is enough
  for the use cases above and avoids pulling in tidyverse machinery.
- Bare-symbol NSE (`x = hj` without the `~`). Requires `substitute()`
  in every entry point and breaks programmatic use without an escape
  hatch. The `~` is a small price for a much cleaner implementation.

### Related

- Decided against NSE for `plot_scatter3d()` in the `feature-interactive-plot3d`
  branch (Q1 of that plan). Reason: consistency with the rest of the
  package and simpler validation. This entry captures the option for a
  future package-wide sweep rather than a piecemeal adoption.

## `plot_scatter3d()` — non-numeric axis support (Date / POSIXct / factor)

### Motivation

`plot_scatter3d()` v1 (shipped in `feature-interactive-plot3d`) enforces a
numeric-only contract for its x / y / z axes — matching
`plot_moderation_3d()` and the textbook "rotatable quantitative point
cloud" metaphor. plotly itself happily renders categorical axes
(discrete tick positions per unique factor/string value) and date
axes (formatted POSIXct/Date ticks), so two real use cases are
deferred rather than impossible:

- **Time-series 3D demos**: plotting two quantitative variables
  against a Date or POSIXct timestamp on the z-axis ("how do x and y
  evolve over time, viewed as a trajectory").
- **Grouped position separation**: putting a 3-to-5-level factor on
  one axis so each group occupies its own slab in the cube. Today
  the package-recommended pattern is to put the group in `color`
  instead; this is usually right but not always (e.g. when the user
  also wants to color by a fourth continuous variable).

### Sketch

Loosen the axis-type guard in `plot_scatter3d()`:

- `is.numeric(col)` → numeric axis (today's only path).
- `inherits(col, c("Date", "POSIXct", "POSIXlt"))` → forward to
  plotly with `axis.type = "date"`. Verify axis labels render
  readably under rotation.
- `is.factor(col) || is.character(col) || is.logical(col)` →
  categorical axis. Cap the number of unique values
  (e.g. error above ~10) to prevent unreadable tick spam.

`interactive_scatter3d()`'s axis pickers would correspondingly broaden
beyond "numeric columns only".

### Open questions

1. **Type coercion vs. error.** Logical → numeric (0/1) coercion is
   tempting but loses information vs. the discrete-axis treatment.
   Pick one path per type, document, stick to it.
2. **Aspect ratio under mixed axis types.** The current `aspect`
   argument assumes commensurable numeric ranges. Date and
   categorical axes don't compose with `aspect = c(1, 2, 4)` in any
   intuitive way. Either drop `aspect` when any axis is
   non-numeric, or document the surprise.
3. **Mixed numeric + categorical color.** If `color` is continuous
   and one axis is categorical, the legend / colorbar pairing gets
   busy. Worth a small design pass before shipping.

### Out of scope

- Animation frames (plotly's `frame` aesthetic) — separate feature.
- 3D surface / mesh from non-numeric inputs (doesn't make sense).

### Related

- v1 of `plot_scatter3d()` rejects non-numeric axes with an error pointing
  users to `color` for grouping. This entry is the eventual home for
  that case.

## A real bundled teaching dataset (replaces / augments `moderation_data`)

### Motivation

`moderation_data` is a synthetic four-column data frame (`y`, `x`, `z`,
`w`, all numeric) built for one specific function:
`plot_moderation_3d()`. As more functions in the package adopt it as
their default `data =` argument (currently
`interactive_moderation_3d()` and, as of the
`feature-interactive-plot3d` branch, `plot_scatter3d()` /
`interactive_scatter3d()`), it's getting stretched into a "general demo
dataset" role it was never designed for.

A real, richer bundled dataset would:

- Make `?plot_scatter3d`, `?plot_logit`, `?plot_pca`, `?plot_regression`, etc. tell
  a coherent narrative ("here is one dataset, here are several
  questions you can ask of it") rather than each example being a toy.
- Cover the **type mix** different functions need:
  - 3+ numeric columns (for `plot_scatter3d`, `plot_pca`)
  - At least one continuous predictor + one binary/factor outcome
    (for `plot_logit`)
  - A continuous predictor + continuous moderator + optional
    categorical moderator (for `plot_moderation_3d`,
    `plot_moderation()` when added)
  - A categorical grouping variable (for `color` in `plot_scatter3d`, and
    for the deferred non-numeric-axis support)
  - Ideally enough rows (~100–500) to make sampling/CI demos meaningful
- Carry a public-domain or permissive license suitable for
  redistribution in a CRAN package.

### Sketch

Candidate sources to evaluate:

- **`palmerpenguins`-style**: a small, real, well-documented dataset
  with the right type mix and an evocative narrative. Either bundle a
  similar dataset directly or use `palmerpenguins` as Suggests in
  examples.
- **A subset of an athletic decathlon dataset** (the user's original
  3D scatter sample plotted `hj` / `X100m` / `X110h` from one) —
  natural fit for `plot_scatter3d` and has continuous variables with
  genuinely different scales (seconds vs. meters), exactly the case
  `aspect` exists to address.
- **A small Likert / survey dataset** for moderation and logit demos.

Whichever is chosen: stored in `data/` via `usethis::use_data()`,
documented in `R/data.R` alongside the existing entries, and added to
`Depends: R (>= 4.0.0)` only — no new heavy dataset packages as hard
dependencies.

### Open questions

1. **One dataset or two?** A single dataset that serves every function
   has to cover a lot of ground (numeric mix, categorical levels,
   binary outcome, dates) and risks feeling artificial. Two smaller
   datasets — one continuous-heavy, one mixed-with-categorical — may
   be cleaner. Cost: more `?dataname` entries to maintain and more
   choice for users to make.
2. **Replace or add?** ~~Open.~~ **Settled: add.** As of the v0.8.0 CRAN
   release this is no longer a free choice. Four exported functions carry
   `data = moderation_data` as a default, so an argument-less call is now
   part of the released compatibility surface: repointing
   `moderation_data` at different data would silently change the output
   of code that already works. Any new dataset must be **added**
   alongside it. `moderation_data` also still earns its keep as a
   *clean / no-confounders* demo for moderation specifically.
3. **License and provenance.** Real datasets need a documented
   source and license. Bundle the cleaning code under
   `data-raw/` (already a directory in the package) so the
   derivation is reproducible.
4. **Tests.** Every function defaulting to the new dataset needs
   updated tests — including the auto-pick path for `plot_scatter3d()`
   (which is deterministic on column order).

### Out of scope

- Replacing the synthetic vectors used inside individual tests (those
  are local, fast, and deterministic — leave alone).
- A vignette built around the dataset (separate work; should follow
  the dataset, not precede it).

### Related

- `data/moderation_data.rda` and `R/data.R` document the current
  state.
- The `feature-interactive-plot3d` branch added `plot_scatter3d()` /
  `interactive_scatter3d()` with `data = moderation_data` defaults —
  this entry is the cleanup once the package has more than one
  function casually depending on `moderation_data` as a generic demo.

## Publication-quality output for the 3D visualizations

### Motivation

As of v0.8.0 the README and `DESCRIPTION` position `plot_scatter3d()`
and `plot_moderation_3d()` as general-purpose data visualization, not
only teaching aids — they accept arbitrary data frames and formulas
and expose axis, color, aspect, and camera control. The stated
direction is figures good enough for textbooks and manuscripts.

They are not there yet, and the README says so. This entry records
what "there" actually requires, so the claim can be made honestly
later rather than aspirationally now.

### The concrete gaps

- **No static export path.** `plot_scatter3d()` returns a `plotly`
  htmlwidget, which renders to interactive HTML. A manuscript needs a
  vector or high-DPI raster file. `plotly::save_image()` requires
  `kaleido` (a Python dependency), so this cannot become a hard
  dependency of a CRAN package — it has to be a documented recipe or
  a `Suggests`-guarded helper.
- **No figure-geometry control.** Neither function exposes size, DPI,
  margins, or font family / size. A journal figure is specified in
  millimeters and points, not in viewer-pane pixels.
- **Hardcoded aesthetics.** Colors are baked in (the wireframe's
  height gradient, `plot_scatter3d()`'s default marker color). No
  palette argument, no grayscale mode, no colorblind-safe default —
  and print submissions still routinely need grayscale.
- **No legend / annotation control** beyond axis titles.
- **`lattice` vs `plotly` split.** The two 3D functions render through
  different engines with different export stories. Anyone producing a
  figure pair for one paper will hit two different sets of controls.

### Open questions

- Is a `Suggests`-guarded `save_figure()` helper worth it, or is a
  vignette section with the `kaleido` recipe the honest answer?
- Should the wireframe move to `plotly` for a single export path, or
  does `lattice` stay because it is a base-graphics-friendly trellis
  object? Moving it is a breaking return-type change and would need a
  deprecation cycle post-CRAN.
- Grayscale / colorblind-safe palettes: new argument, or a global
  option?

### Out of scope

- Any change to the concept-demonstration functions
  (`plot_t_test()`, `plot_sampling()`, `plot_sample_ci()`,
  `plot_matrix_inverse()`). Those are lecture-hall visuals and there
  is no reason to make them publication-shaped.

### Related

- `README.Rmd` names this direction and links here.
- `cran-comments.md` describes the two-part split for reviewers.
