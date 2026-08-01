#' compstatslib machine_precision() function
#' 
#' 
#' Code function that reports the smallest possible number on the user's machine such that 1 + x != 1.
#' 
#' @return A numeric value representing the smallest possible number that the user's computer can effectively represent.
#' 
#' @examples
#' machine_precision()
#'
#' # Anything smaller is swallowed by rounding
#' 1 + machine_precision() != 1
#' 1 + machine_precision() / 2 != 1
#'
#' @export
machine_precision <- function() {
  .Machine$double.eps
}
