#' compstatslib plot_t_test() function
#' 
#' Non-interactive visualization function that plots null and alternative
#' t distributions of a t-test. Shows rejection zone and power as areas
#' under the curves.
#' 
#' @param diff The test difference (defaults to 0.5).
#' @param sd Population standard deviation (defaults to 4).
#' @param n Sample size (defaults to 100).
#' @param alpha Significance level (defaults to 0.05).
#' @param error_matrix Whether to display type I/II error matrix (defaults to FALSE).
#' 
#' @return No return value, called for side effects (plots to the active
#' device).
#'
#' @examples
#' plot_t_test()
#' plot_t_test(diff=-0.1, sd=3)
#' 
#' @seealso \code{\link{interactive_t_test}}
#' 
#' @export
plot_t_test <- function(diff = 0.5, sd = 4, n = 100, alpha = 0.05, error_matrix = FALSE) {
  df <- n - 1
  t <- diff / (sd / sqrt(n))
  t_null_plot(df, alpha)
  t_alt_lines(df, t, alpha)
  
  alt_stats <- t_alt_lines(df, t, alpha)
  
  if(error_matrix == TRUE) {
    plot_error_matrix(alpha, alt_stats)
  }
}

# Plot a distribution
plotdist <- function(xseq, xdens, col, xlim, type, lty, lwd, segments=NULL, qlty, qcol, polyfill=NULL) {
  if (type == "plot") {
    plot(xseq, xdens, type="l", lwd=0, col=col, frame=FALSE, xlim=xlim, lty=lty, ylab='', xlab='')
  }
  
  if (!is.null(polyfill)) {
    polygon(polyfill[,1], polyfill[,2], col=qcol, border=qcol)
  }
  
  # draw quantile lines
  if (!is.null(segments)) {
    segments(x0=segments[,1], x1=segments[,1], y0=0, y1=segments[,2], lwd=lwd, col=qcol, lty=qlty)
  }
  
  lines(xseq, xdens, type="l", lwd=lwd, col=col, lty=lty)
}

# Plot the t distribution
plott <- function(lwd=2, ncp=0, df=300, col=rgb(0.30,0.50,0.75), xlim=c(-3,3), type="plot", lty="solid", quants=NULL, qlty="solid", qcol=rgb(0.30,0.50,0.75, 0.5), fill_quants=NULL) {
  xseq = seq(ncp-6, ncp+6, length=1000)
  xdens = dt(xseq, ncp=ncp, df=df)
  
  if (length(xlim) == 0) {
    xlim = c(ncp-3.5, ncp+3.5)
  }
  
  segments <- NULL
  polyfill <- NULL
  
  if (!is.null(quants)) {
    xquants = qt(quants, ncp=ncp, df=df)
    dquants = dt(xquants, ncp=ncp, df=df)
    segments = cbind(xquants, dquants)
  }
  
  else {
    xquants = NULL
  }
  
  if(!is.null(fill_quants)) {
    polyq = qt(fill_quants, ncp=ncp, df=df)
    polyfill.x = seq(polyq[1], polyq[2], by=0.001)
    polyfill.y = dt(polyfill.x, ncp=ncp, df=df)
    polyfill.x = c(polyfill.x[1], polyfill.x, tail(polyfill.x,1))
    polyfill.y = c(0, polyfill.y, 0)
    polyfill <- cbind(polyfill.x, polyfill.y)
  }
  
  plotdist(xseq, xdens, col, xlim, type, lty, lwd, segments, qlty, qcol, polyfill)
  c(polyq[1], xquants)
}

t_null_plot <- function(df, alpha) {
  plott(df=df, col=rgb(0.75, 0.1, 0.1), qcol=rgb(1, 0.5, 0.5), xlim=c(-6, 6), fill_quants=c(1-alpha, 0.999))
}

t_alt_lines <- function(df, ncp=0, alpha) {
  blue <- rgb(0.1, 0.1, 0.75)
  lightblue <- rgb(0.4, 0.4, 1, 0.3)
  quants <- c(0.5)
  beta <- pt(qt(1-alpha, df=df), df=df, ncp=ncp)
  one_minus_beta <- 1 - beta
  highlight_stats <- plott(df=df, ncp=ncp, type='lines', lty="dashed", col=blue, quants=quants, qcol=lightblue, xlim=c(-6, 6), fill_quants=c(beta, 0.999))
  c(beta, one_minus_beta, highlight_stats)
}

# Plot the error matrix
recttext <- function(xl, yb, xr, yt, pwr_qnt, left_cap = NULL, top_cap = NULL, title = NULL,
                     text = NULL, rectArgs = NULL, leftcapArgs = NULL, 
                     topcapArgs = NULL, titleArgs = NULL, textArgs = NULL) {
  left_cap_text <- c(xl - 0.45, mean(c(yb, yt)))
  top_cap_text <- c(mean(c(xl, xr)), yt + 0.02)
  title_text <- c(mean(c(xl, xr)), yt - 0.015)
  center <- c(mean(c(xl, xr)), mean(c(yb, yt)))

  do.call('rect', c(list(xleft = xl, ybottom = yb, xright = xr, ytop = yt), rectArgs))
  do.call('text', c(list(x = left_cap_text[1], y = left_cap_text[2], 
                         labels = left_cap), leftcapArgs, srt = 90))
  do.call('text', c(list(x = top_cap_text[1], y = top_cap_text[2], 
                         labels = top_cap), topcapArgs))
  do.call('text', c(list(x = title_text[1], y = title_text[2], labels = title),
                    titleArgs))
  do.call('text', c(list(x = center[1], y = center[2], labels = text), textArgs))
}

plot_error_matrix <- function(alpha, alt_stats) {
  # Top-left square
  recttext(xl = -5.5, yb = 0.25, xr = -4, yt = 0.375, 
           rectArgs = list(col = rgb(1, 0.5, 0.5), lty = "solid"),
           left_cap = "If evidence says \nREJECT \nnull hypothesis",
           leftcapArgs = list(cex = 0.45),
           top_cap = "If null is \nreally TRUE",
           topcapArgs = list(cex = 0.5),
           title = "Type I error", text = alpha,
           titleArgs = list(col = "black", cex = 0.5),
           textArgs = list(col = "black", cex = 0.75))
  
  # Top-right square
  recttext(xl = -4, yb =0.25, xr = -2.5, yt = 0.375,
           rectArgs = list(col = rgb(0.30,0.50,0.75, 0.5), lty = "solid"),
           top_cap = "If null is \nreally FALSE",
           topcapArgs = list(cex = 0.5),
           title = "Correct!", text = round((alt_stats[2]), 2),
           titleArgs = list(col = "black", cex = 0.5),
           textArgs = list(col = "black", cex = 0.75))
    
  # Bottom-left square
  recttext(xl = -5.5, yb = 0.125, xr = -4, yt = 0.25,
           rectArgs = list(col = rgb(0.30,0.50,0.75, 0.5), lty = "solid"),
           left_cap = "If evidence says \nCANNOT REJECT \nnull hypothesis",
           leftcapArgs = list(cex = 0.45),
           title = "Correct!", text = 1 - alpha,
           titleArgs = list(col = "black", cex = 0.5),
           textArgs = list(col = "black", cex = 0.75))

  # Bottom-right square
  recttext(xl = -4, yb = 0.125, xr = -2.5, yt = 0.25, title = "Type II error", 
           rectArgs = list(col = rgb(1, 0.5, 0.5), lty = "solid"),
           text = round(alt_stats[1], 2),
           titleArgs = list(col = "black", cex = 0.5),
           textArgs = list(col = "black", cex = 0.75))
  
  # Top rectangle
  if (alt_stats[3] < alt_stats[4]) {
  recttext(xl = -5.5, yb = 0.25, xr = -2.5, yt = 0.375,
           rectArgs = list(lwd = 4, border = rgb(0.1, 0.1, 0.75)))
  }
  
  # Bottom rectangle
  else {
  recttext(xl = -5.5, yb = 0.125, xr = -2.5, yt = 0.25,
           rectArgs = list(lwd = 4, border = rgb(0.1, 0.1, 0.75)))
  }

}

