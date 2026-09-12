# Linear regression plotting
test_that("Log likelihood of linear regression correct computed", {
  set.seed(23982938)
  y <- rnorm(1000)
  x1 <- rnorm(1000)
  x2 <- rnorm(1000)
  reg <- lm(y ~ x1 + x2)

  expect_equal(compstatslib:::loglik_lm(reg), logLik(reg)[1])
})
