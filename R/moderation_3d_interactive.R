#' compstatslib interactive_moderation_3d() function
#'
#' Interactive 3D moderation visualization. Shiny gadget wrapping
#' \code{\link{plot_moderation_3d}} with rotation sliders for live
#' viewing of the wireframe surface from any angle. Useful in classroom
#' demos to show how the interaction-induced surface twist becomes
#' visible (or vanishes) under different viewing angles.
#'
#' @param formula A model formula. With exactly two predictors the
#'   first is taken as the IV and the second as the moderator. With
#'   three or more predictors, you must supply \code{iv} and \code{mod}.
#'   Defaults to \code{y ~ x * z}, matching the bundled
#'   \code{\link{moderation_data}}.
#' @param data A data frame containing the variables named in
#'   \code{formula}. Defaults to the bundled
#'   \code{\link{moderation_data}}.
#' @param iv Optional character. Name of the predictor to place on the
#'   first horizontal axis. Required when \code{formula} has more than
#'   two predictors.
#' @param mod Optional character. Name of the predictor to place on the
#'   second horizontal axis. Required when \code{formula} has more than
#'   two predictors.
#' @param ... Further arguments forwarded to
#'   \code{\link{plot_moderation_3d}} (and from there to
#'   \code{\link[lattice]{wireframe}}).
#'
#' @return On "Done", a \code{compstatslib_args} object: a named list of
#'   \code{formula}, \code{data}, \code{iv}, \code{mod} and — crucially —
#'   the \code{z_rot} / \code{x_rot} viewing angles the gadget was left at,
#'   plus anything passed through \code{...}. It prints the
#'   \code{\link{plot_moderation_3d}} call that reproduces the view, and is
#'   still an ordinary list, so \code{do.call(plot_moderation_3d, result)}
#'   works. On "Cancel", \code{NULL}. See \link{compstatslib-reproduce}.
#'
#'   The printed call shows the expression you passed for \code{data} rather
#'   than its values, so pass a named variable if you want a pasteable call.
#'
#' @details
#' The gadget exposes two rotation sliders:
#' \itemize{
#'   \item Z rotation (0–360, default 40) — rotation about the vertical
#'         axis. Try 0 to align the IV slope plane with the screen,
#'         or 270 to align the moderator slope plane.
#'   \item X rotation (-90 to -70, default -70) — tilt above/below
#'         the surface.
#' }
#'
#' The \emph{vertical} axis of the rendered plot depicts the outcome
#' (DV) regardless of variable naming. With the bundled data the
#' moderator is named \code{z}, but \code{z} appears on a horizontal
#' plot axis, not the vertical "z-axis" of the wireframe.
#'
#' This function only works in IDEs that support Shiny gadgets
#' (RStudio, Positron, etc.).
#'
#' @seealso \code{\link{plot_moderation_3d}} for the non-interactive
#'   version; \code{\link{moderation_data}} for the bundled teaching
#'   dataset.
#'
#' @examples
#' if (interactive()) {
#'   # Default synthetic example
#'   interactive_moderation_3d()
#'
#'   # User's own data and naming
#'   set.seed(1)
#'   my_df <- data.frame(
#'     score = rnorm(100),
#'     time  = rnorm(100),
#'     dose  = rnorm(100)
#'   )
#'   interactive_moderation_3d(score ~ time * dose, my_df)
#'
#'   # Multi-predictor model — `w` is bundled noise, included as a
#'   # control. Pick which two predictors go on the plot axes; the
#'   # rest are held at typical values (here, `mean(w)`).
#'   interactive_moderation_3d(y ~ x + z + w + x:z, moderation_data,
#'                             iv = "x", mod = "z")
#'
#'   # Rotate to an angle worth keeping, click Done, then freeze that
#'   # exact view into a script or report
#'   result <- interactive_moderation_3d()
#'   do.call(plot_moderation_3d, result)
#' }
#'
#' @export
interactive_moderation_3d <- function(formula = y ~ x * z,
                                      data    = moderation_data, # nolint: object_usage_linter
                                      iv      = NULL,
                                      mod     = NULL,
                                      ...) {
  dot_args <- list(...)
  data_label <- paste(deparse(substitute(data)), collapse = " ")
  force(data)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Interactive 3D Moderation",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      padding = 0,
      shiny::tags$div(
        style = "display: flex; flex-direction: column; height: 100%;",
        shiny::tags$div(
          style = paste("display: flex; align-items: center;",
                        "gap: 16px; padding: 8px; flex-shrink: 0;"),
          shiny::sliderInput("z_rot", "Z rotation",
                             min = 0, max = 360, value = 40,
                             width = "320px"),
          shiny::sliderInput("x_rot", "X rotation",
                             min = -90, max = -70, value = -70,
                             width = "220px")
        ),
        shiny::tags$div(
          style = "flex: 1; min-height: 0;",
          shiny::plotOutput("wire", height = "100%")
        )
      )
    )
  )

  server <- function(input, output, session) {
    first_render <- shiny::reactiveVal(TRUE)
    output$wire <- shiny::renderPlot({
      call_args <- c(list(formula = formula, data = data,
                          iv = iv, mod = mod,
                          z_rot = input$z_rot, x_rot = input$x_rot),
                     dot_args)
      if (first_render()) {
        first_render(FALSE)
        do.call(plot_moderation_3d, call_args)
      } else {
        suppressMessages(do.call(plot_moderation_3d, call_args))
      }
    })

    shiny::observeEvent(input$done, {
      result <- compstatslib_args(
        c(list(formula = formula, data = data, iv = iv, mod = mod,
               z_rot = input$z_rot, x_rot = input$x_rot),
          dot_args),
        fn       = "plot_moderation_3d",
        defaults = list(iv = NULL, mod = NULL, z_rot = 40, x_rot = -70),
        display  = list(data = .verbatim(data_label))
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
