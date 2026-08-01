#' compstatslib plot_moderation_3d() function
#'
#' Non-interactive visualization of a moderated regression. Fits an
#' \code{lm()} from the supplied formula and data, predicts the outcome
#' over a regular grid of the IV and moderator (any other predictors
#' held at typical values), and renders the fitted surface as a 3D
#' wireframe with a height-mapped color gradient.
#'
#' @param formula A model formula. The first variable on the right-hand
#'   side is treated as the IV and the second as the moderator when the
#'   formula has exactly two predictors. With three or more predictors,
#'   you must specify \code{iv} and \code{mod} explicitly. Any formula
#'   structure R supports works (\code{x * z}, \code{x + z + x:z},
#'   \code{x:z}, transformations, etc.).
#' @param data A data frame containing every variable named in
#'   \code{formula}. Defaults to the bundled
#'   \code{\link{moderation_data}}.
#' @param iv Optional character. Name of the predictor to place on the
#'   first horizontal axis. Required when \code{formula} has more than
#'   two predictors.
#' @param mod Optional character. Name of the predictor to place on the
#'   second horizontal axis. Required when \code{formula} has more than
#'   two predictors.
#' @param z_rot Numeric. Rotation about the vertical axis, in degrees
#'   (lattice's \code{screen$z}). Default 40. Try 0 to align the IV
#'   slope plane with the screen, or 270 for the moderator slope plane.
#' @param x_rot Numeric. Rotation about the horizontal axis, in degrees
#'   (lattice's \code{screen$x}). Default -70.
#' @param zlim Numeric length-2 vector. Vertical-axis range. Default
#'   \code{NULL}, which uses the union of the actual outcome's range
#'   in \code{data} and the fitted-surface range over the prediction
#'   grid.
#' @param ... Further arguments forwarded to
#'   \code{\link[lattice]{wireframe}}.
#'
#' @return A \code{trellis} object (lattice surface). Print or assign to
#'   render.
#'
#' @details
#' The user's \code{formula} is fitted as-is via \code{lm()}, so any
#' structure R's formula language supports works. \code{predict()} then
#' evaluates the fitted model at every point of an \code{expand.grid()}
#' over the IV and moderator ranges (15 steps each). Predictors other
#' than the IV and moderator are held at typical values: \code{mean()}
#' for numeric variables, the first level for factors, the
#' lexicographically first value for character variables, and
#' \code{FALSE} for logicals. This is a pedagogical simplification; if
#' you need other hold-out values, fit \code{lm()} yourself and slice
#' the surface manually.
#'
#' The \emph{vertical} axis of the rendered plot depicts the outcome
#' (DV) regardless of variable naming. With the bundled
#' \code{moderation_data} the moderator is named \code{z}, but \code{z}
#' appears on a horizontal plot axis, not the vertical "z-axis" of the
#' wireframe.
#'
#' When the formula contains predictors beyond \code{iv} and \code{mod},
#' a one-line \code{message()} is emitted noting that other predictors
#' are held at their typical values. Suppress with
#' \code{suppressMessages()}.
#'
#' \code{iv} and \code{mod} must be numeric. Categorical (factor or
#' character) variables on the plot axes are not supported and will
#' raise an error.
#'
#' @seealso \code{\link{interactive_moderation_3d}} for the rotatable
#'   Shiny gadget version; \code{\link{moderation_data}} for the
#'   bundled teaching dataset.
#'
#' @examples
#' # Default synthetic example
#' plot_moderation_3d()
#'
#' # Additive model — flat surface, no twist
#' plot_moderation_3d(y ~ x + z, moderation_data)
#'
#' # User's own data and naming
#' set.seed(1)
#' my_df <- data.frame(
#'   score = rnorm(100),
#'   time  = rnorm(100),
#'   dose  = rnorm(100)
#' )
#' plot_moderation_3d(score ~ time * dose, my_df)
#'
#' # Multi-predictor model — `w` is bundled noise, included as a
#' # control. Pick which two predictors go on the plot axes; the rest
#' # are held at typical values (here, `mean(w)`).
#' plot_moderation_3d(y ~ x + z + w + x:z, moderation_data,
#'                    iv = "x", mod = "z")
#'
#' @export
plot_moderation_3d <- function(formula = y ~ x * z,
                               data    = moderation_data, # nolint: object_usage_linter
                               iv      = NULL,
                               mod     = NULL,
                               z_rot   = 40,
                               x_rot   = -70,
                               zlim    = NULL,
                               ...) {
  vars <- all.vars(formula)
  if (length(vars) < 3) {
    stop("`formula` must name a DV and at least two predictors. ",
         "Got: ", paste(vars, collapse = ", "))
  }
  dv         <- vars[1]
  predictors <- vars[-1]

  if (is.null(iv) && is.null(mod)) {
    if (length(predictors) == 2) {
      iv  <- predictors[1]
      mod <- predictors[2]
    } else {
      stop("With more than two predictors in `formula`, you must specify ",
           "`iv` and `mod` to choose which two appear on the plot axes. ",
           "Predictors found: ", paste(predictors, collapse = ", "))
    }
  } else if (is.null(iv) || is.null(mod)) {
    stop("Specify both `iv` and `mod`, or neither.")
  }

  if (!iv %in% predictors) {
    stop("`iv = \"", iv, "\"` is not a predictor in `formula`.")
  }
  if (!mod %in% predictors) {
    stop("`mod = \"", mod, "\"` is not a predictor in `formula`.")
  }
  if (iv == mod) {
    stop("`iv` and `mod` must name different variables.")
  }

  missing_cols <- setdiff(vars, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing columns named in formula: ",
         paste(missing_cols, collapse = ", "))
  }

  for (axis_name in c("iv", "mod")) {
    axis_var <- get(axis_name)
    if (!is.numeric(data[[axis_var]])) {
      stop("`", axis_name, " = \"", axis_var, "\"` is ",
           paste(class(data[[axis_var]]), collapse = "/"),
           "; plot_moderation_3d() does not support categorical variables ",
           "on the plot axes.")
    }
  }

  fit <- lm(formula, data = data)

  grid <- setNames(
    expand.grid(
      seq(min(data[[iv]]),  max(data[[iv]]),  length.out = 15),
      seq(min(data[[mod]]), max(data[[mod]]), length.out = 15)
    ),
    c(iv, mod)
  )

  for (nm in setdiff(predictors, c(iv, mod))) {
    grid[[nm]] <- hold_value(data[[nm]])
  }

  msg <- describe_holds(predictors, iv, mod, data, dv)
  if (!is.null(msg)) message(msg)

  grid[[dv]] <- predict(fit, grid)

  if (is.null(zlim)) zlim <- range(c(data[[dv]], grid[[dv]]))

  lattice::wireframe(
    reformulate(c(iv, mod), response = dv),
    data     = grid,
    drape    = TRUE,
    colorkey = FALSE,
    scales   = list(arrows = FALSE),
    screen   = list(z = z_rot, x = x_rot),
    zlim     = zlim,
    ...
  )
}

hold_value <- function(x) {
  if (is.numeric(x)) {
    mean(x, na.rm = TRUE)
  } else if (is.factor(x)) {
    factor(levels(x)[1], levels = levels(x))
  } else if (is.character(x)) {
    sort(unique(x))[1]
  } else if (is.logical(x)) {
    FALSE
  } else {
    stop("Cannot hold variable of class ", paste(class(x), collapse = "/"),
         " at a typical value. Drop it from the model or recode it.")
  }
}

describe_holds <- function(predictors, iv, mod, data, dv) {
  others <- setdiff(predictors, c(iv, mod))
  if (length(others) == 0) return(NULL)

  paste0(
    "Surface shows predicted ", dv, " over ", iv, " and ", mod,
    ". Other predictors are held at their typical values."
  )
}
