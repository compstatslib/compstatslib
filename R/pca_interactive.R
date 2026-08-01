#' compstatslib interactive_pca() function
#'
#' Interactive visualization function that lets you point-and-click to add data
#' points, while it automatically plots and updates principal component vectors.
#'
#' @param meancenter A logical parameter that will mean center the points if
#' set to TRUE, while it will not mean center the points if set to FALSE. By
#' default it is set to TRUE.
#'
#' @return A list containing:
#'  \item{points}{A dataframe with \code{x} and \code{y} coordinates.}
#'  \item{pca}{The \code{prcomp} result if 3+ points were added, or
#'  \code{NULL} otherwise.}
#'
#' @details
#' Click on the plotting area to add points and see corresponding principal
#' components. Click "Done" to return results to the console.
#'
#' @seealso \code{\link{plot_pca}}
#'
#' @export
interactive_pca <- function(meancenter = TRUE) {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Interactive PCA",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      padding = 0,
      shiny::tags$div(
        style = paste("display: flex; flex-direction: column;",
                      "height: 100%;"),
        shiny::tags$div(
          style = "flex: 1; min-height: 0;",
          shiny::plotOutput("pca_plot", height = "100%",
                           click = "plot_click")
        )
      )
    )
  )

  server <- function(input, output, session) {
    pts <- shiny::reactiveVal(data.frame())
    pca_result <- shiny::reactiveVal(NULL)

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

    output$pca_plot <- shiny::renderPlot({
      result <- plot_pca(pts(), meancenter = meancenter)
      pca_result(result)
    })

    shiny::observeEvent(input$done, {
      result <- compstatslib_points(
        pts(), fn = "plot_pca",
        args     = list(meancenter = meancenter),
        defaults = list(meancenter = TRUE),
        extra    = list(pca = pca_result())
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
