#' Reproduction calls returned by the interactive gadgets
#'
#' Every \code{interactive_*()} gadget hands its final state back to the
#' console in one of two shapes, both of which print as the
#' \code{plot_*()} call that reproduces what was on screen:
#'
#' \describe{
#'   \item{\code{compstatslib_args}}{A named list of arguments for the
#'     paired plot function. Still an ordinary list, so
#'     \code{do.call(plot_fn, result)} works.}
#'   \item{\code{compstatslib_points}}{A dataframe of points, with any
#'     further arguments carried alongside. Still an ordinary dataframe,
#'     so \code{nrow()}, \code{[} and passing it straight back to the
#'     plot function all behave as before.}
#' }
#'
#' Derived or accumulated state that a user would not retype (a
#' \code{prcomp} result, an accumulated sampling cache) rides along as an
#' attribute rather than appearing in the printed call.
#'
#' @param x An object of one of the two classes.
#' @param ... Ignored, for consistency with \code{\link{print}}.
#'
#' @return \code{x}, invisibly. Called for the printed reproduction call.
#'
#' @name compstatslib-reproduce
NULL

# Marks a string to be emitted into the reproduction call verbatim, rather
# than deparsed. Gadgets use it to show the caller's own variable names
# (captured with substitute()) instead of dumping large objects.
.verbatim <- function(text) {
  structure(as.character(text), class = "compstatslib_verbatim")
}

.deparse_arg <- function(value) {
  if (inherits(value, "compstatslib_verbatim")) return(as.character(value))
  if (is.data.frame(value)) return(sprintf("<%d points>", nrow(value)))
  # Shiny returns integers for step-1 sliders; "250L" is valid but noisy.
  if (is.integer(value)) value <- as.numeric(value)
  paste(deparse(value), collapse = " ")
}

.at_default <- function(value, default) {
  if (is.numeric(value) && is.numeric(default)) {
    return(isTRUE(all.equal(as.numeric(value), as.numeric(default))))
  }
  # Formulas built in different environments are never identical(), so
  # compare what they say rather than where they were made.
  if (inherits(value, "formula") && inherits(default, "formula")) {
    return(identical(deparse(value), deparse(default)))
  }
  identical(value, default)
}

# Builds "fn(a = 1, b = 2)" from a named list, dropping any argument that
# still sits at the default `defaults` gives for it. `leading` is emitted
# first without a `name =` prefix (used for the points dataframe).
.format_call <- function(fn, args, defaults = list(), leading = NULL) {
  keep <- vapply(
    names(args),
    function(nm) {
      !(nm %in% names(defaults) && .at_default(args[[nm]], defaults[[nm]]))
    },
    logical(1)
  )
  args <- args[keep]

  parts <- vapply(
    names(args),
    function(nm) sprintf("%s = %s", nm, .deparse_arg(args[[nm]])),
    character(1)
  )
  parts <- c(leading, parts)

  sprintf("%s(%s)", fn, paste(parts, collapse = ", "))
}

# Constructors -----------------------------------------------------------

compstatslib_args <- function(args, fn, defaults = list(), display = list(),
                              extra = list()) {
  structure(args,
            class    = "compstatslib_args",
            fn       = fn,
            defaults = defaults,
            display  = display) |>
    .attach_extra(extra)
}

compstatslib_points <- function(points, fn, args = list(), defaults = list(),
                                extra = list(), symbol = "points") {
  structure(points,
            class    = c("compstatslib_points", "data.frame"),
            fn       = fn,
            args     = args,
            defaults = defaults,
            symbol   = symbol) |>
    .attach_extra(extra)
}

.attach_extra <- function(x, extra) {
  for (nm in names(extra)) attr(x, nm) <- extra[[nm]]
  x
}

# Strips the bookkeeping attributes so the object prints / behaves as its
# underlying type.
.bare_args <- function(x) {
  out <- unclass(x)
  for (nm in c("fn", "defaults", "display", "args", "symbol")) {
    attr(out, nm) <- NULL
  }
  out
}

# Substitutes any print-only stand-ins (e.g. the caller's variable name in
# place of a 100,000-element population vector) before formatting.
.display_args <- function(x) {
  args <- .bare_args(x)
  display <- attr(x, "display")
  if (length(display)) args[names(display)] <- display
  args
}

# print methods ----------------------------------------------------------

#' @rdname compstatslib-reproduce
#' @export
print.compstatslib_args <- function(x, ...) {
  cat(.format_call(attr(x, "fn"), .display_args(x), attr(x, "defaults")), "\n",
      sep = "")
  invisible(x)
}

#' @rdname compstatslib-reproduce
#' @export
print.compstatslib_points <- function(x, ...) {
  cat(.format_call(attr(x, "fn"), attr(x, "args"), attr(x, "defaults"),
                   leading = attr(x, "symbol")), "\n\n", sep = "")
  NextMethod()
  invisible(x)
}
