#' compstatslib interactive_logit() function
#'
#' Interactive visualization function that lets you point-and-click to add
#' data points, while it automatically plots and updates a logistic regression
#' curve and associated statistics.
#'
#' @param points An optional \code{dataframe} of x and y points to plot
#' and estimate the regression.
#' If no \code{points} are provided, the user is free to click and create points
#' on the plot area.
#'
#' @param formula A \code{formula} to use in estimating logit
#' (e.g., \code{y ~ x}).
#'
#' @param min_x Minimum x value for the plot range.
#'
#' @param max_x Maximum x value for the plot range.
#'
#' @param ... Further arguments passed to the \code{plot_logit()} function that
#' produces the plot.
#'
#' @return A \code{dataframe} containing the points coordinates.
#'
#' @details
#' Click on the plotting area to add points and see a corresponding logistic
#' regression curve. Click "Done" to return the points to the console.
#'
#' @seealso \code{\link{plot_logit}}
#'
#' @examples
#' \dontrun{
#' # Selecting coordinates on the plot area, storing them in 'pts'
#' pts <- interactive_logit()
#'
#' # Replotting the points stored earlier in 'pts', allowing the user to
#' # continue the interactive regression
#' interactive_logit(pts)
#' }
#' @export
interactive_logit <- function(points = data.frame(), formula = y ~ x,
                              min_x = 0, max_x = 50, ...) {
  dot_args <- list(...)
  y_name <- as.character(formula[[2]])
  x_name <- as.character(formula[[3]])

  if (nrow(points) > 0) max_x <- max(max_x, points[[x_name]])

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Interactive Logit",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      padding = 0,
      shiny::tags$div(
        style = paste("display: flex; flex-direction: column;",
                      "height: 100%;"),
        shiny::tags$div(
          style = "flex: 1; min-height: 0;",
          shiny::plotOutput("logit_plot", height = "100%",
                           click = "plot_click")
        )
      )
    )
  )

  server <- function(input, output, session) {
    pts <- shiny::reactiveVal(points)

    shiny::observeEvent(input$plot_click, {
      click <- input$plot_click
      click_x <- max(0, min(max_x, click$x))
      click_y <- round(click$y)

      new_pt <- data.frame(click_x, click_y)
      names(new_pt) <- c(x_name, y_name)

      current <- pts()
      if (nrow(current) == 0) {
        pts(new_pt)
      } else {
        pts(rbind(current, new_pt))
      }
    })

    output$logit_plot <- shiny::renderPlot({
      do.call(plot_logit,
              c(list(points = pts(), formula = formula,
                     min_x = min_x, max_x = max_x),
                dot_args))
    })

    shiny::observeEvent(input$done, {
      result <- compstatslib_points(
        pts(), fn = "plot_logit",
        args = c(list(formula = formula, min_x = min_x, max_x = max_x),
                 dot_args),
        defaults = list(formula = y ~ x, min_x = 0, max_x = 1)
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
