#' Computes log likelihood for linear regression
#' based on https://stats.stackexchange.com/questions/73196/recalculate-log-likelihood-from-a-simple-r-lm-model
#' set.seed(23982938)
#' y <- rnorm(1000)
#' x1 <- rnorm(1000)
#' x2 <- rnorm(1000)
#' reg <- lm(y ~ x1 + x2)
#'
#' loglik_lm(reg)
#' # compare against: logLik(reg)
loglik_lm <- function(lm_model) {
  n <- nrow(lm_model$model)           # sample size
  k <- dim(model.matrix(lm_model))[2] # number of coefficients
  df <- n - k                         # degrees of freedom

  y <- lm_model$model[, 1]            # dv
  y_hat <- lm_model$fitted.values     # fitted dv

  sigma <- sqrt(sum((y - y_hat)^2) / df) # OLS residual standard error
  sigma_ml <- sigma * sqrt(df / n)       # ML residual standard error

  y_probs <- dnorm(y, mean = y_hat, sd = sigma_ml)
  sum(log(y_probs))
}
