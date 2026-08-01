#' compstatslib interactive_t_test() function
#'
#' Interactive visualization that allows one to adjust the parameters affecting
#' hypothesis testing in order to see how their variation influences the null t
#' and alternative t distributions, and statistical power.
#'
#' @param diff The initial difference between the null and alternative means.
#'
#' @param sd The initial standard deviation of the population.
#'
#' @param n The initial sample size.
#'
#' @param alpha The initial significance level.
#'
#' @param error_matrix Logical; whether to show the error matrix initially.
#'
#' @details
#' Use the sliders in the viewer to adjust parameters. The movement of the
#' alternative t-statistics distribution with respect to the null distribution
#' will be visible, as well as the consequent change in statistical power.
#' Click "Done" to close.
#'
#' All five arguments match \code{\link{plot_t_test}}'s, so a gadget session can
#' be resumed from a previous result.
#'
#' @seealso \code{\link{plot_t_test}}
#'
#' @export
interactive_t_test <- function(diff = 0.5, sd = 4, n = 100, alpha = 0.05,
                               error_matrix = FALSE) {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("T-Test Visualization",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      padding = 0,
      shiny::tags$div(
        style = "display: flex; height: 100%;",
        shiny::tags$div(
          style = "width: 140px; flex-shrink: 0; padding: 4px 6px; overflow-y: auto;",
          shiny::sliderInput("diff", "Difference",
            min = 0, max = 4, value = diff, step = 0.1,
            width = "100%"),
          shiny::sliderInput("sd", "Std Dev",
            min = 1, max = 5, value = sd, step = 0.1,
            width = "100%"),
          shiny::sliderInput("n", "Sample Size",
            min = 2, max = 500, value = n, step = 1,
            width = "100%"),
          shiny::sliderInput("alpha", "Alpha",
            min = 0.01, max = 0.1, value = alpha, step = 0.01,
            width = "100%"),
          shiny::checkboxInput("error_matrix", "Error Matrix",
            value = error_matrix)
        ),
        shiny::tags$div(
          style = "flex: 1; min-width: 0;",
          shiny::plotOutput("t_test_plot", height = "100%")
        )
      )
    )
  )

  server <- function(input, output, session) {
    output$t_test_plot <- shiny::renderPlot({
      suppressWarnings(
        plot_t_test(input$diff, input$sd, input$n,
                    input$alpha, input$error_matrix)
      )
    })

    shiny::observeEvent(input$done, {
      result <- compstatslib_args(
        list(diff = input$diff, sd = input$sd, n = input$n,
             alpha = input$alpha, error_matrix = input$error_matrix),
        fn = "plot_t_test",
        defaults = list(diff = 0.5, sd = 4, n = 100, alpha = 0.05,
                        error_matrix = FALSE)
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
