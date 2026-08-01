#' compstatslib interactive_matrix_inverse() function
#'
#' Interactive function that allows one to visualize a matrix inversion
#' using sliders to adjust matrix parameters.
#'
#' Inspired by: \url{https://math.stackexchange.com/questions/295250/geometric-interpretations-of-matrix-inverses/1922830#1922830}
#'
#' @param x1 The first row (or column) vector of matrix A. This parameter is set to 1 by default.
#'
#' @param y1 The second row (or column) vector of matrix A. This parameter is set to 2 by default.
#'
#' The area of the parallelogram resulting from these two vectors is the determinant of matrix A.
#'
#' @param x2 The first row (or column) vector of the inverse matrix A^(-1). This parameter is set to 2 by default.
#'
#' @param y2 The second row (or column) vector of the inverse matrix A^(-1). This parameter is set to 1 by default.
#'
#' The area of the parallelogram resulting from these two vectors is the determinant of the inverse matrix A^(-1).
#'
#' @return On "Done", a \code{compstatslib_args} object: a named list of
#' \code{x1}, \code{y1}, \code{x2} and \code{y2} at their final slider
#' positions, which prints the \code{\link{plot_matrix_inverse}} call that
#' reproduces the plot. It is still an ordinary list, so
#' \code{do.call(plot_matrix_inverse, result)} works. On "Cancel",
#' \code{NULL}. See \link{compstatslib-reproduce}.
#'
#' @details
#' Use the sliders in the viewer to adjust the matrix parameters and see the
#' resulting transformation. Click "Done" to close.
#'
#' @seealso \code{\link{plot_matrix_inverse}}
#'
#' @examples
#' if (interactive()) {
#'   # Move the sliders, then Done
#'   result <- interactive_matrix_inverse()
#'
#'   # Reproduce the plot non-interactively
#'   do.call(plot_matrix_inverse, result)
#'
#'   # Or resume the gadget where it was left
#'   do.call(interactive_matrix_inverse, result)
#' }
#'
#' @export
interactive_matrix_inverse <- function(x1 = 1, y1 = 2, x2 = 2, y2 = 1) {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Matrix Inverse",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      padding = 0,
      shiny::tags$div(
        style = "display: flex; height: 100%;",
        shiny::tags$div(
          style = "width: 140px; flex-shrink: 0; padding: 4px 6px; overflow-y: auto;",
          shiny::sliderInput("x1", "x1",
            min = -2, max = 2, value = x1, step = 0.1,
            width = "100%"),
          shiny::sliderInput("y1", "y1",
            min = -2, max = 2, value = y1, step = 0.1,
            width = "100%"),
          shiny::sliderInput("x2", "x2",
            min = -2, max = 2, value = x2, step = 0.1,
            width = "100%"),
          shiny::sliderInput("y2", "y2",
            min = -2, max = 2, value = y2, step = 0.1,
            width = "100%")
        ),
        shiny::tags$div(
          style = "flex: 1; min-width: 0;",
          shiny::plotOutput("matrix_plot", height = "100%")
        )
      )
    )
  )

  server <- function(input, output, session) {
    output$matrix_plot <- shiny::renderPlot({
      plot_matrix_inverse(input$x1, input$y1, input$x2, input$y2)
    })

    shiny::observeEvent(input$done, {
      result <- compstatslib_args(
        list(x1 = input$x1, y1 = input$y1,
             x2 = input$x2, y2 = input$y2),
        fn = "plot_matrix_inverse"
      )
      print(result)
      shiny::stopApp(invisible(result))
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  old_opts <- options(shiny.quiet = TRUE)
  on.exit(options(old_opts))
  suppressMessages(
    shiny::runGadget(ui, server, viewer = shiny::paneViewer())
  )
}
