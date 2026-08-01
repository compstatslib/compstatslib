#' Internal imports
#'
#' Consolidates every function this package imports from base R packages and
#' from \pkg{plotly}. Keeping them in one place means \code{NAMESPACE} has a
#' single source of truth rather than tags scattered across \code{R/*.R}.
#'
#' @importFrom grDevices rgb
#' @importFrom graphics abline arrows axis hist legend lines par points polygon
#'   segments text
#' @importFrom plotly plot_ly layout
#' @importFrom stats binomial cor density dt glm lm prcomp predict pt qt
#'   reformulate rnorm sd setNames
#' @importFrom utils tail
#'
#' @keywords internal
#' @name compstatslib-imports
NULL
