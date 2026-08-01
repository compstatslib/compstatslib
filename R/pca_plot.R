#' compstatslib plot_pca() function
#'
#' Plots data points and their principal component vectors.
#'
#' @param points A \code{dataframe} with \code{x} and \code{y} columns
#' containing the point coordinates.
#' @param meancenter Logical; if \code{TRUE} (default), points are
#' mean-centered before computing PCA.
#' @param xlim The x-axis limits for the plot.
#' @param ylim The y-axis limits for the plot.
#'
#' @return If 3 or more points are provided, invisibly returns the
#' \code{prcomp} result. Otherwise returns \code{NULL} invisibly.
#'
#' @seealso \code{\link{interactive_pca}}
#'
#' @examples
#' points <- data.frame(x = c(-20, -5, 5, 15, 30), y = c(-15, -10, 5, 10, 25))
#' plot_pca(points)
#'
#' # Without mean-centering, the components are anchored at the origin
#' plot_pca(points, meancenter = FALSE)
#'
#' @export
plot_pca <- function(points = data.frame(),
                     meancenter = TRUE,
                     xlim = c(-50, 50),
                     ylim = c(-50, 50)) {
  if (nrow(points) == 0) {
    plot(NA, xlim = xlim, ylim = ylim, xlab = "x", ylab = "y", asp = 1)
    return(invisible(NULL))
  }

  # mean-center points
  if (meancenter && nrow(points) > 1) {
    mc_diff <- sapply(points, mean)
    mc_points <- sweep(points, 2, mc_diff)
  } else {
    mc_diff <- c(x = 0, y = 0)
    mc_points <- points
  }

  # Plot points
  plot(points[c("x", "y")], xlim = xlim, ylim = ylim,
       pch = 19, cex = 2, col = "gray", asp = 1)

  # Plot PC vectors
  pca <- NULL
  if (nrow(points) >= 3) {
    pca <- prcomp(mc_points, scale. = FALSE)
    egvec <- pca$rotation[, c("PC1", "PC2")]
    sv <- pca$sdev[1:2]
    vec <- egvec %*% diag(sv)
    rownames(vec) <- c("x", "y")
    colnames(vec) <- c("PC1", "PC2")

    arrows(-vec["x", ] + mc_diff["x"], -vec["y", ] + mc_diff["y"],
            vec["x", ] + mc_diff["x"],  vec["y", ] + mc_diff["y"],
           lty = c("solid", "dotted"), length = 0.1)
  }

  invisible(pca)
}
