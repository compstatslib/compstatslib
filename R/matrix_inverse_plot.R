#' compstatslib plot_matrix_inverse() function
#' 
#' Non-interactive plotting function that helps visualize an inverse.
#' 
#' @param x1 The first row (or column) vector of matrix A.
#'
#' @param y1 The second row (or column) vector of matrix A.
#' 
#' @param x2 The first row (or column) vector of the inverse matrix A^(-1).
#' 
#' @param y2 The second row (or column) vector of the inverse matrix A^(-1).
#'
#' @return No return value, called for side effects (plots to the active
#' device).
#'
#' @details
#' The user can choose the magnitude of the vectors, which will be graphically
#' represented by the function.
#'
#' @seealso \code{\link{interactive_matrix_inverse}}
#'
#' @examples
#' # A matrix and its inverse: the two parallelograms have reciprocal areas
#' plot_matrix_inverse(1, 2, 2, 1)
#'
#' # A near-singular matrix stretches its inverse dramatically
#' plot_matrix_inverse(1, 0.9, 0.9, 1)
#'
#' @export
plot_matrix_inverse <- function(x1, y1, x2, y2) {
  A <- matrix(c(x1, y1, x2, y2), nrow = 2)
  Ainv <- solve(A)
  
  A_col <- rgb(1, 0, 0, 0.1)
  Ainv_col <- rgb(0, 0, 1, 0.1)
  
  plot(NA, xlim=c(-3, 3), ylim=c(-3,3), frame.plot = FALSE)
  plot_matrix_det(A, A_col)
  plot_matrix_det(Ainv, Ainv_col)
}

plot_matrix_det <- function(M, col) {
  polygon(c(0,M[1,1], M[1,1]+M[1,2], M[1,2]), 
          c(0,M[2,1], M[2,1]+M[2,2], M[2,2]), 
          col=col, border=rgb(0,0,0,0.3))
  
  arrows(0, 0, M[1,1], M[2,1])
  arrows(0, 0, M[1,2], M[2,2])
}