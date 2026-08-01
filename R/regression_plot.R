#' compstatslib plot_regr() function
#' 
#' Non-interactive visualization function that plots given points, regression line and relevant statistics.
#'
#' @param points A \code{dataframe} of *x* and *y* coordinates to plot.
#' 
#' @param regression A logical parameter that plots a regression line when set to TRUE and hides it when set to FALSE. It is set to TRUE by default.
#' 
#' @param stats A logical parameter that displays the relevant statistics on the plot area when set to TRUE; it hides them when set to FALSE. It is set to TRUE by default.
#' 
#'   
#' @return A \code{dataframe} containing the points coordinates. Additionally, the following parameters are provided on the plot area:
#'  \item{Raw intercept}{The y-coordinate at which the regression line crosses the y-axis.}
#'  \item{Raw slope}{The value of the slope parameter.}
#'  \item{Correlation}{The strength of the linear relationship.}
#'  \item{SSR}{The sum of squares regression.}
#'  \item{SSE}{The sum of squares error.}
#'  \item{SST}{The sum of squares total.}
#'  \item{R-squared}{The multiple coefficient of determination.}
#'
#' @seealso \code{\link{interactive_regression}}
#' 
#' @examples
#' points <- data.frame(x = c(1, 3, 5, 8), y = c(2, 4, 6 ,8))
#' 
#' plot_regr(points)
#' 
#' @export
plot_regr <- function(points, regression=TRUE, stats=TRUE) {
  max_x <- 50
  if (nrow(points) == 0) {
    plot(NA, xlim=c(-5,max_x), ylim=c(-5,max_x), xlab="x", ylab="y")
    return()
  }
  plot(points, xlim=c(-5,max_x), ylim=c(-5,max_x), pch=19, cex=2, col="gray")
  if (nrow(points) < 2) return()
  
  if (regression) {
    mean_x <- mean(points$x)
    mean_y <- mean(points$y)
    segments(0, mean_y, max_x, mean_y, lwd=1, col="lightgray", lty="dotted")
    segments(mean_x, 0, mean_x, mean_y, lwd=1, col="lightgray", lty="dotted")
    regr <- lm(points$y ~ points$x)
    abline(regr, lwd=2, col="cornflowerblue")
    
    if (stats) {  
      regr_summary <- summary(regr)
      ssr <- sum((regr$fitted.values - mean(points$y))^2)
      sse <- sum((points$y - regr$fitted.values)^2)
      sst <- sum((points$y - mean(points$y))^2)
      
      old_par <- par(family="mono")
      on.exit(par(old_par), add = TRUE)
      legend("topleft", legend = c(
        paste(" Raw intercept: ", round(regr$coefficients[1], 2), "\n",
              "Raw slope    : ", round(regr$coefficients[2], 2), "\n",
              "Correlation  : ", round(cor(points$x, points$y), 2), "\n",
              "SSR          : ", round(ssr, 2), "\n",
              "SSE          : ", round(sse, 2), "\n",
              "SST          : ", round(sst, 2), "\n",
              "R-squared    : ", round(regr_summary$r.squared, 2))),
        bty="n")
    }
  }
}
