#' compstatslib plot_sampling() function
#' 
#' Plot distribution of a population, samples drawn from the population, given sampling statistic.
#' 
#' @param population A \code{vector} of values following any population distribution you wish to simulate.
#' @param sample_size The size of each sample to draw from the population
#' @param theta The \code{function} that computes the statistic of interest from each sample (e.g., \code{mean} or \code{median}).
#' @param reps Number of repeated samples to draw on each call.
#' @param vars Optional cached state from a previous call, used by
#'   \code{interactive_sampling()} to accumulate samples across draws.
#' @param replot_population Logical; whether to redraw the population panel.
#'
#' @return A \code{list} containing the population data, sample statistics, and other information of interest
#'
#' @examples
#' \dontrun{
#' plot_sampling(rnorm(100000), sample_size = 100, reps = 50, theta = median)
#' }
#' 
#' @export
plot_sampling <- function(population, sample_size, theta, reps = 1,
                          vars = NULL, replot_population = TRUE) {
  if (!is.null(vars)) {
    xmin <- vars$xmin
    xmax <- vars$xmax
    sample_theta <- vars$sample_theta
  } else {
    xmin <- min(population)
    xmax <- max(population)
    sample_theta <- c()
  }
  
  # Preplotting computations needed on each plot
  samples <- replicate(reps, sample(population, sample_size))
  reps_theta <- apply(samples, FUN = theta, MARGIN = 2)
  sample_theta <- append(sample_theta, reps_theta)
  popd <- density(population)
  samd <- density(samples)
  
  # Capture before setting: par(mfrow=) resets cex as a side effect, so a
  # combined call would save the already-clobbered cex rather than the user's.
  old_par <- par(c("mfrow", "mar", "cex"))
  on.exit(par(old_par), add = TRUE)
  par(mfrow = c(3, 1), mar=c(2, 2, 1, 1), cex=0.5)

  plot(popd, lty = "dotted", lwd = 2, xlim = c(xmin, xmax), xaxt = "n",
       yaxt = "n", frame = FALSE, main = NA)
  axis(1)
  text(xmin, max(popd$y)/2, boldly("Population Distribution"), adj = 0)
  
  plot(samd, lty = "solid", lwd = 2, xlim = c(xmin, xmax), xaxt = "n",
       yaxt = "n", frame = FALSE, main = NA)
  apply(samples, MARGIN=2, \(s) { 
    lines(density(s), lty = "solid", col=rgb(0.7, 0.7, 0.7, 0.5), lwd = 1) })
  lines(samd, lty = "solid", lwd = 2)
  
  axis(1)
  text(xmin, max(samd$y)/2, boldly("Sample Distribution"), adj = 0)
  
  # TODO: allow user to fix bin size
  # TODO: use some fraction of population hist height
  # if (length(sample_theta) < 3) { breaks <- "Sturges" } else { breaks <- "FD" }
  samh <- hist(sample_theta, border = FALSE,
               xlim = c(xmin, xmax), xaxt = "n", yaxt = "n", main = NA)
  axis(1)
  theta_count <- paste("Sampling Statistic", "\n(", length(sample_theta), ")")
  text(xmin, max(samh$counts)/2, theta_count, adj = 0)
  
  vars <- list(
    population = population,
    xmin = xmin,
    xmax = xmax,
    sample_size = sample_size,
    theta = theta,
    sample_theta = sample_theta
  )
  
  class(vars) <- append(class(vars), c("compstats", "sampling"))
  
  vars
}

boldly <- function(str) {
  substitute(paste(bold(str)))
}
