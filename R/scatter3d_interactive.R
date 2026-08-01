#' compstatslib interactive_scatter3d() function
#'
#' Interactive 3D scatterplot. Shiny gadget wrapping
#' \code{\link{plot_scatter3d}} with column pickers (x / y / z / colour)
#' and display-control sliders (aspect ratio per axis, marker opacity,
#' marker size). The plot re-renders live as inputs change. On Done,
#' the equivalent \code{plot_scatter3d()} call is printed to the
#' console (copy-pasteable into a script) and the chosen arguments are
#' returned invisibly so that \code{do.call(plot_scatter3d, result)}
#' (or \code{do.call(interactive_scatter3d, result)} to resume a
#' session) reproduces the same plot. On Cancel, returns \code{NULL}.
#'
#' Every \code{plot_scatter3d()} argument is accepted here as a
#' starting value — column choices, aspect ratio, opacity, marker size,
#' camera position, and axis titles. Pickers and sliders are
#' pre-positioned accordingly; the user can change them mid-session
#' (the title arg has no in-gadget UI but threads through every render
#' and into the Done output).
#'
#' @param data A data frame containing at least three numeric columns.
#'   Defaults to the bundled \code{\link{moderation_data}}.
#' @param x,y,z Optional character. Names of numeric columns of
#'   \code{data} to pre-select in the axis pickers. When \code{NULL}
#'   (default), the first three numeric columns are used.
#' @param color Optional character. Name of any column of \code{data}
#'   to pre-select in the colour picker, or \code{NULL} (default) for
#'   uniform colour.
#' @param aspect,opacity,size Initial values for the aspect-ratio,
#'   opacity, and marker-size sliders. Same defaults and validation as
#'   \code{\link{plot_scatter3d}}.
#' @param camera Optional list giving the initial 3D camera position.
#'   Useful for resuming a session at a previously captured view.
#'   \code{NULL} (default) uses plotly's default view.
#' @param titles Optional named list / character vector with custom
#'   axis titles (recognised names: \code{x}, \code{y}, \code{z}).
#'   Threaded through every render and included in the Done output.
#' @param ... Further arguments forwarded to \code{\link{plot_scatter3d}}
#'   on every render. Not echoed in the printed Done call.
#'
#' @return On Done, \code{invisible(list(data, x, y, z, color, aspect,
#'   opacity, size, camera, titles))}; on Cancel, \code{NULL}.
#'   \code{camera} is \code{NULL} unless the user rotated or zoomed
#'   during the session (or supplied an initial camera).
#'
#' @details
#' Only numeric columns of \code{data} appear in the x / y / z pickers
#' (matching \code{\link{plot_scatter3d}}'s numeric-axis contract); the
#' colour picker offers all columns plus a \code{"(none)"} sentinel.
#' Errors before launching if \code{data} has fewer than three numeric
#' columns, or if any supplied initial \code{x}/\code{y}/\code{z} is
#' not a numeric column, or if initial \code{color} is not in
#' \code{names(data)}.
#'
#' User-driven rotation and zoom are preserved across slider / picker
#' re-renders within the gadget. The current camera state is captured
#' via the \code{plotly_relayout} event and re-passed into each render
#' so structural changes (e.g. toggling colour) don't reset the view.
#' If the user rotated or zoomed (or a starting \code{camera} was
#' supplied), the camera is included in the Done output so the
#' reproduced \code{plot_scatter3d()} call opens at the same view —
#' useful for nailing down a specific angle for an Rmd / report.
#'
#' The printed Done call uses \code{deparse(substitute(data))} captured
#' at gadget entry, so it shows the user's variable name (e.g.
#' \code{plot_scatter3d(data = my_df, ...)}). Args still at their
#' \code{plot_scatter3d()} defaults are omitted from the printed call.
#' Caveat: inline expressions are reproduced literally — calling
#' \code{interactive_scatter3d(read.csv("x.csv"))} prints
#' \code{plot_scatter3d(data = read.csv("x.csv"), ...)} which re-runs the
#' read when pasted; capture the data in a variable first to avoid that.
#'
#' This function only works in IDEs that support Shiny gadgets
#' (RStudio, Positron, etc.).
#'
#' @seealso \code{\link{plot_scatter3d}} for the non-interactive
#'   version; \code{\link{moderation_data}} for the bundled teaching
#'   dataset.
#'
#' @examples
#' \dontrun{
#' # Default synthetic example
#' interactive_scatter3d()
#'
#' # User's own data
#' set.seed(1)
#' my_df <- data.frame(
#'   score = rnorm(100),
#'   time  = rnorm(100),
#'   dose  = rnorm(100),
#'   group = factor(sample(c("ctrl", "treat"), 100, replace = TRUE))
#' )
#' interactive_scatter3d(my_df)
#'
#' # Pre-position columns and starting display
#' interactive_scatter3d(my_df,
#'                       x = "score", y = "time", z = "dose",
#'                       color = "group",
#'                       aspect = c(1, 1.5, 1),
#'                       titles = list(x = "Score", y = "Time",
#'                                     z = "Dose"))
#'
#' # Resume a previous session from its returned state
#' result <- interactive_scatter3d(my_df)
#' do.call(interactive_scatter3d, result)
#'
#' # Or finalise the captured view as a non-interactive plot
#' do.call(plot_scatter3d, result)
#' }
#'
#' @export
interactive_scatter3d <- function(data    = moderation_data, # nolint: object_usage_linter
                                  x       = NULL,
                                  y       = NULL,
                                  z       = NULL,
                                  color   = NULL,
                                  aspect  = c(1, 1, 1),
                                  opacity = 0.8,
                                  size    = 5,
                                  camera  = NULL,
                                  titles  = NULL,
                                  ...) {
  data_sym <- substitute(data)
  force(data)

  scatter3d_validate_style(aspect, opacity, size, camera)

  for (axis_name in c("x", "y", "z")) {
    scatter3d_validate_axis_arg(axis_name, environment())
  }

  num_cols <- scatter3d_numeric_cols(data)
  scatter3d_require_3_numeric(num_cols, "interactive_scatter3d")

  for (axis_name in c("x", "y", "z")) {
    val <- get(axis_name)
    if (!is.null(val) && !val %in% num_cols) {
      stop("Initial `", axis_name, "` (\"", val,
           "\") is not a numeric column of `data`. ",
           "Numeric columns: ", paste(num_cols, collapse = ", "), ".",
           call. = FALSE)
    }
  }

  if (!is.null(color) && !color %in% names(data)) {
    stop("Initial `color` (\"", color, "\") is not in `data`.",
         call. = FALSE)
  }

  init_x       <- if (!is.null(x))     x     else num_cols[1]
  init_y       <- if (!is.null(y))     y     else num_cols[2]
  init_z       <- if (!is.null(z))     z     else num_cols[3]
  init_color   <- if (!is.null(color)) color else "(none)"

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Interactive 3D Scatterplot",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      padding = 0,
      shiny::tags$div(
        style = "display: flex; flex-direction: column; height: 100%;",
        shiny::tags$div(
          style = paste("display: flex; flex-wrap: wrap; gap: 12px;",
                        "padding: 8px; flex-shrink: 0;",
                        "border-bottom: 1px solid #ddd;"),
          shiny::selectInput("x", "X",
                             choices = num_cols,
                             selected = init_x, width = "140px"),
          shiny::selectInput("y", "Y",
                             choices = num_cols,
                             selected = init_y, width = "140px"),
          shiny::selectInput("z", "Z",
                             choices = num_cols,
                             selected = init_z, width = "140px"),
          shiny::selectInput("color", "Color",
                             choices = c("(none)", names(data)),
                             selected = init_color, width = "180px")
        ),
        shiny::tags$div(
          style = paste("display: flex; flex-wrap: wrap; gap: 12px;",
                        "padding: 8px; flex-shrink: 0;"),
          shiny::sliderInput("aspect_x", "Aspect X",
                             min = 0.1, max = 10, value = aspect[1],
                             step = 0.1, width = "150px"),
          shiny::sliderInput("aspect_y", "Aspect Y",
                             min = 0.1, max = 10, value = aspect[2],
                             step = 0.1, width = "150px"),
          shiny::sliderInput("aspect_z", "Aspect Z",
                             min = 0.1, max = 10, value = aspect[3],
                             step = 0.1, width = "150px"),
          shiny::sliderInput("opacity", "Opacity",
                             min = 0.05, max = 1, value = opacity,
                             step = 0.05, width = "150px"),
          shiny::sliderInput("size", "Size",
                             min = 1, max = 20, value = size,
                             step = 1, width = "150px")
        ),
        shiny::tags$div(
          style = "flex: 1; min-height: 0;",
          plotly::plotlyOutput("plot", height = "100%")
        )
      )
    )
  )

  server <- function(input, output, session) {
    camera_state <- shiny::reactiveVal(camera)

    output$plot <- plotly::renderPlotly({
      color_col <- if (input$color == "(none)") NULL else input$color
      cam       <- shiny::isolate(camera_state())
      p <- plot_scatter3d(
        data    = data,
        x       = input$x,
        y       = input$y,
        z       = input$z,
        color   = color_col,
        aspect  = c(input$aspect_x, input$aspect_y, input$aspect_z),
        opacity = input$opacity,
        size    = input$size,
        camera  = cam,
        titles  = titles,
        ...,
        source  = "scatter3d"
      )
      plotly::event_register(p, "plotly_relayout")
    })

    shiny::observe({
      ev <- plotly::event_data("plotly_relayout", source = "scatter3d")
      shiny::req(ev)
      if (!is.null(ev[["scene.camera"]])) {
        camera_state(ev[["scene.camera"]])
      }
    })

    shiny::observeEvent(input$done, {
      color_col  <- if (input$color == "(none)") NULL else input$color
      aspect_vec <- c(input$aspect_x, input$aspect_y, input$aspect_z)
      cam        <- camera_state()
      result <- compstatslib_args(
        list(
          data    = data,
          x       = input$x,
          y       = input$y,
          z       = input$z,
          color   = color_col,
          aspect  = aspect_vec,
          opacity = input$opacity,
          size    = input$size,
          camera  = cam,
          titles  = titles
        ),
        fn       = "plot_scatter3d",
        defaults = list(color = NULL, aspect = c(1, 1, 1), opacity = 0.8,
                        size = 5, camera = NULL, titles = NULL),
        display  = list(data = .verbatim(paste(deparse(data_sym),
                                               collapse = " ")))
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
  invisible(suppressMessages(
    shiny::runGadget(ui, server, viewer = shiny::paneViewer())
  ))
}
