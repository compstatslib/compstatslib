#' compstatslib interactive_regression() function
#'
#' Interactive visualization function that lets you point-and-click to add data
#' points, while it automatically plots and updates a regression line and
#' associated statistics.
#'
#' @param points An optional \code{dataframe} of *x* and *y* points to plot and
#' estimate the regression. If no \code{points} are provided, the user is free
#' to click and create points on the plot area.
#'
#' @param ... Further arguments passed to the \code{plot_regression()} function that
#' produces the plot.
#'
#' @return A \code{dataframe} containing the points coordinates. Additionally,
#' the following parameters are provided on the plot area:
#'  \item{Raw intercept}{The y-coordinate at which the regression line crosses
#'  the y-axis.}
#'  \item{Raw slope}{The value of the slope parameter.}
#'  \item{Correlation}{The strength of the linear relationship.}
#'  \item{SSR}{The sum of squares regression.}
#'  \item{SSE}{The sum of squares error.}
#'  \item{SST}{The sum of squares total.}
#'  \item{R-squared}{The multiple coefficient of determination.}
#'
#' @details
#' Click on the plot area to add points and see a corresponding regression line.
#' Click "Done" to return the points to the console.
#'
#' @seealso \code{\link{plot_regression}}
#'
#' @examples
#' \dontrun{
#' # Selecting coordinates on the plot area, storing them in 'pts'
#' pts <- interactive_regression()
#'
#' # Replotting the points stored earlier in 'pts', allowing the user to
#' # continue the interactive regression
#' interactive_regression(pts)
#'
#' # Providing coordinates beforehand
#' points <- data.frame(x = c(1, 4, 7), y = c(2, 5, 8))
#'
#' # Replotting the coordinates and continuing the interactive regression
#' interactive_regression(points)
#' }
#' @export
interactive_regression <- function(points = data.frame(), ...) {
  dot_args <- list(...)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Interactive Regression",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      padding = 0,
      shiny::tags$div(
        style = paste("display: flex; flex-direction: column;",
                      "height: 100%;"),
        shiny::tags$div(
          style = "flex: 1; min-height: 0;",
          shiny::plotOutput("regr_plot", height = "100%",
                           click = "plot_click")
        )
      )
    )
  )

  server <- function(input, output, session) {
    pts <- shiny::reactiveVal(points)

    shiny::observeEvent(input$plot_click, {
      click <- input$plot_click
      new_pt <- data.frame(x = click$x, y = click$y)
      current <- pts()
      if (nrow(current) == 0) {
        pts(new_pt)
      } else {
        pts(rbind(current, new_pt))
      }
    })

    output$regr_plot <- shiny::renderPlot({
      do.call(plot_regression, c(list(points = pts()), dot_args))
    })

    shiny::observeEvent(input$done, {
      result <- compstatslib_points(pts(), fn = "plot_regression",
                                    args = dot_args)
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
