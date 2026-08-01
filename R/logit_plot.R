#' compstatslib plot_logit() function
#'
#' Non-interactive visualization function that plots given points,
#' logistic regression line and relevant statistics.
#'
#' @param points A \code{dataframe} of *x* and *y* coordinates to plot.
#'
#' @param formula A \code{formula} to use in estimating logit
#' (e.g., \code{y ~ x}).
#'
#' @param regression Logical parameter of whether to plot a regression line
#' (TRUE by default)
#'
#' @param stats Logical parameter of whether to display relevant statistics on
#' the plot area (TRUE by default).
#' The following parameters are provided on the plot area:
#' \describe{
#'  \item{Intercept}{The intercept of the logistic regression.}
#'  \item{Coefficient}{The coefficient of the independent variable.}
#'  \item{AIC}{An information theoretic fit criterion.}
#' }
#'
#' @param min_x Minimum x value for the plot range.
#'
#' @param max_x Maximum x value for the plot range.
#'
#' @param legend_loc Position of the statistics legend on the plot
#' (e.g., "topleft", "bottomright").
#'
#' @return A \code{dataframe} containing the points coordinates.
#'
#' @seealso \code{\link{interactive_regression}}
#'
#' @examples
#' mydata <- data.frame(iv = c(-6, -3, 1, 3, 5, 8), dv = c(0, 0, 0, 1, 1 ,1))
#' plot_logit(mydata, formula = dv ~ iv)
#'
#' @export
plot_logit <- function(points, formula = y ~ x, regression = TRUE, stats = TRUE,
                       min_x = 0, max_x = 1, legend_loc = "topleft") {
  # Blank plotting figure if no data yet
  if (nrow(points) == 0) {
    plot_points_logit(NA, min_x, max_x)
    return()
  }

  y_name <- as.character(formula[[2]])
  x_name <- as.character(formula[[3]])

  if (nrow(points) > 0) {
    max_x <- max(max_x, points[[x_name]])
    min_x <- min(min_x, points[[x_name]])
  }

  # Plot points
  plot_points_logit(points, min_x, max_x)

  if (nrow(points) < 2) return()

  # Plot regression curve if requested
  if (regression) {
    suppressWarnings( # suppress "probabilities numerically 0 or 1 occurred"
      regr <- glm(formula, data = points, family = binomial)
    )

    # Plot curve using predicted values based on model
    newdata <- data.frame(x = seq(min_x, max_x, len = 500))
    names(newdata) <- x_name
    newdata[[y_name]] <- predict(regr, newdata, type = "response")
    lines(formula, newdata, lwd = 2, col = "cornflowerblue")

    # Print stats on plot if requested
    if (stats) {
      regr_summary <- summary(regr)

      old_par <- par(family = "mono")
      on.exit(par(old_par), add = TRUE)
      legend(legend_loc, legend = c(
        paste("Intercept  : ", round(regr$coefficients[1], 2), "\n",
              "Coefficient: ", round(regr$coefficients[2], 2), "\n",
              "AIC        : ", round(regr_summary$aic, 2), sep = "")),
        bty = "n")
    }
  }
}

plot_points_logit <- function(points, min_x = 0, max_x = 50) {
  plot(points, xlim = c(min_x, max_x), ylim = c(0, 1),
       xlab = "x", ylab = "y", pch = 19, cex = 2, col = "gray")

  abline(h = 0.5, lwd = 1, col = "lightgray", lty = "dotted")
}
