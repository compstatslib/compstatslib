#' compstatslib plot_sample_ci() function
#' 
#' Visualize the confidence intervals of samples drawn from a population.
#' 
#' @param num_samples Number of the samples to draw from the population.
#' @param sample_size Size of each sample drawn from the population.
#' @param pop_size Size of the overall population to simulate.
#' @param distr_func Random data generation function to simulate the population 
#'                   data (e.g., \code{rnorm} or \code{runif}).
#' @param ... Parameters to pass to the \code{distr_func} function specified.
#'
#' @return No return value, called for side effects (plots to the active
#' device).
#'
#' @examples
#' plot_sample_ci(sample_size=300, distr_func=rnorm, mean=50, sd=10)
#' plot_sample_ci(sample_size=300, distr_func=runif, min=17, max=35)
#'
#' @export
plot_sample_ci <- function(num_samples = 100, sample_size = 100, 
                                pop_size=10000, distr_func=rnorm, ...) {
  # Simulate a large population
  population_data <- distr_func(pop_size, ...)
  pop_mean <- mean(population_data)
  pop_sd <- sd(population_data)
  
  # Simulate samples
  samples <- replicate(num_samples, 
                       sample(population_data, sample_size, replace=FALSE))
  
  # Calculate descriptives of samples
  sample_means = apply(samples, 2, FUN=mean)
  sample_stdevs = apply(samples, 2, FUN=sd)
  sample_stderrs <- sample_stdevs/sqrt(sample_size)
  ci95_low  <- sample_means - sample_stderrs*1.96
  ci95_high <- sample_means + sample_stderrs*1.96 
  ci99_low  <- sample_means - sample_stderrs*2.58
  ci99_high <- sample_means + sample_stderrs*2.58
  
  # Visualize confidence intervals of all samples
  plot(NULL, xlim=c(pop_mean-(pop_sd/2), pop_mean+(pop_sd/2)), 
       ylim=c(1,num_samples), ylab="Samples", xlab="Confidence Intervals")
  segments_ci(ci95_low, ci95_high, ci99_low, ci99_high,
                 sample_means, 1:num_samples, good=TRUE)
  
  # Visualize samples with CIs that don't include population mean
  bad = which(((ci95_low > pop_mean) | (ci95_high < pop_mean)) |
                ((ci99_low > pop_mean) | (ci99_high < pop_mean)))
  segments_ci(ci95_low[bad], ci95_high[bad], ci99_low[bad], ci99_high[bad],
                 sample_means[bad], bad, good=FALSE)
  
  # Draw true population mean
  abline(v=mean(population_data))
}

segments_ci <- function(ci95_low, ci95_high, ci99_low, ci99_high, 
                           sample_means, indices, good=TRUE) {
  segment_colors <- list(c("lightcoral", "coral3", "coral4"),
                         c("lightskyblue", "skyblue3", "skyblue4"))
  color <- segment_colors[[as.integer(good)+1]]
  
  segments(ci99_low, indices, ci99_high, indices, lwd=3, col=color[1])
  segments(ci95_low, indices, ci95_high, indices, lwd=3, col=color[2])
  points(sample_means, indices, pch=18, cex=0.6, col=color[3])
}
