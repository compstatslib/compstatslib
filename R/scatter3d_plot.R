#' compstatslib plot_scatter3d() function
#'
#' Non-interactive 3D scatterplot. Renders three numeric columns of a
#' data frame as a rotatable point cloud, with an optional fourth
#' column mapped to marker colour.
#'
#' @param data A data frame. Defaults to the bundled
#'   \code{\link{moderation_data}}.
#' @param x,y,z Optional character. Names of numeric columns of
#'   \code{data} to place on each axis. When all three are \code{NULL},
#'   the first three numeric columns of \code{data} are used; if
#'   \code{data} has more than three numeric columns, a one-line
#'   \code{message()} reports the chosen and skipped columns.
#' @param color Optional character. Name of any column of \code{data}
#'   to map to marker colour. Numeric columns yield a continuous colour
#'   scale; factor / character / logical columns yield a discrete
#'   palette with a legend. \code{NULL} (default) draws a uniform colour.
#' @param aspect Length-3 positive numeric vector giving the
#'   x / y / z aspect ratio (forwarded to plotly's
#'   \code{scene$aspectratio}). Default \code{c(1, 1, 1)}.
#' @param opacity Single numeric in \code{(0, 1]}. Marker opacity.
#'   Default \code{0.8}.
#' @param size Single positive numeric. Marker size. Default \code{5}.
#' @param camera Optional list giving the initial 3D camera position
#'   (plotly's \code{scene$camera}). Typically a list with \code{eye},
#'   \code{center}, and/or \code{up}, each itself a list with numeric
#'   \code{x}, \code{y}, \code{z} components. Useful for reproducing a
#'   specific rotation / zoom captured from
#'   \code{\link{interactive_scatter3d}}. \code{NULL} (default) uses
#'   plotly's default view. The plot also sets a constant
#'   \code{uirevision} so user-driven rotation / zoom is preserved
#'   across re-renders.
#' @param titles Optional named character vector / list giving custom
#'   axis titles. Recognised names are \code{x}, \code{y}, \code{z}.
#'   Unsupplied axes fall back to the source column name.
#' @param ... Further arguments forwarded to
#'   \code{\link[plotly]{plot_ly}}.
#'
#' @return A \code{plotly} htmlwidget. Print or assign to render.
#'
#' @details
#' Axis types (\code{x} / \code{y} / \code{z}) must be numeric. Factor,
#' character, logical, and Date / POSIXct columns are rejected with a
#' clear error pointing to \code{color} for categorical separation.
#'
#' @seealso \code{\link{interactive_scatter3d}} for a Shiny gadget that
#'   wraps this function with live column pickers and display sliders.
#'
#' @examples
#' # Default synthetic example — `moderation_data` has 4 numeric cols,
#' # so a one-line message notes which 3 were picked.
#' plot_scatter3d()
#'
#' # Explicit columns, no colour
#' plot_scatter3d(moderation_data, x = "x", y = "z", z = "y")
#'
#' # Map a fourth column to colour
#' plot_scatter3d(moderation_data, x = "x", y = "z", z = "y", color = "w")
#'
#' # Stretched aspect, thinner larger markers, custom titles
#' plot_scatter3d(moderation_data,
#'                x = "x", y = "z", z = "y",
#'                aspect = c(1, 2, 4),
#'                opacity = 0.3, size = 10,
#'                titles = list(x = "IV", y = "Moderator", z = "Outcome"))
#'
#' @export
plot_scatter3d <- function(data    = moderation_data, # nolint: object_usage_linter
                           x       = NULL,
                           y       = NULL,
                           z       = NULL,
                           color   = NULL,
                           aspect  = c(1, 1, 1),
                           opacity = 0.8,
                           size    = 5,
                           camera  = NULL,
                           titles  = NULL,
                           ...) {
  scatter3d_validate_style(aspect, opacity, size, camera)

  for (axis_name in c("x", "y", "z")) {
    scatter3d_validate_axis_arg(axis_name, environment())
  }

  if (is.null(x) || is.null(y) || is.null(z)) {
    num_cols <- scatter3d_numeric_cols(data)
    scatter3d_require_3_numeric(num_cols, "plot_scatter3d")
    chosen  <- num_cols[1:3]
    skipped <- num_cols[-(1:3)]
    if (length(skipped) > 0) {
      message("plot_scatter3d(): using numeric columns ",
              paste(chosen, collapse = ", "),
              "; skipped ", paste(skipped, collapse = ", "),
              ". Pass x/y/z to choose explicitly.")
    }
    if (is.null(x)) x <- chosen[1]
    if (is.null(y)) y <- chosen[2]
    if (is.null(z)) z <- chosen[3]
  }

  for (axis in c("x", "y", "z")) {
    col <- get(axis)
    if (!col %in% names(data)) {
      stop("Column \"", col, "\" (passed as `", axis,
           "`) is not in `data`.")
    }
    if (!is.numeric(data[[col]])) {
      stop("Column \"", col, "\" (passed as `", axis, "`) is ",
           paste(class(data[[col]]), collapse = "/"),
           "; plot_scatter3d() requires numeric columns on x/y/z. ",
           "Use `color` for categorical separation.")
    }
  }

  if (!is.null(color) && !color %in% names(data)) {
    stop("Column \"", color, "\" (passed as `color`) is not in `data`.")
  }

  axis_titles <- c(x = x, y = y, z = z)
  if (!is.null(titles)) {
    for (a in intersect(names(titles), c("x", "y", "z"))) {
      axis_titles[a] <- titles[[a]]
    }
  }

  plot_args <- list(
    x      = data[[x]],
    y      = data[[y]],
    z      = data[[z]],
    type   = "scatter3d",
    mode   = "markers",
    marker = list(opacity = opacity, size = size),
    ...
  )
  if (!is.null(color)) plot_args$color <- data[[color]]
  p <- do.call(plotly::plot_ly, plot_args)
  scene <- list(
    aspectmode  = "manual",
    aspectratio = list(x = aspect[1], y = aspect[2], z = aspect[3]),
    xaxis       = list(title = axis_titles[["x"]]),
    yaxis       = list(title = axis_titles[["y"]]),
    zaxis       = list(title = axis_titles[["z"]]),
    uirevision  = "scatter3d"
  )
  if (!is.null(camera)) scene$camera <- camera
  plotly::layout(p, uirevision = "scatter3d", scene = scene)
}
