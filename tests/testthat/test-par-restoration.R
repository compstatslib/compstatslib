# CRAN policy: functions must restore the user's par() settings on exit,
# including when they error part-way through.

test_that("plot_regression() leaves par('family') untouched", {
  pdf(NULL)
  on.exit({ dev.off() }, add = TRUE)

  old <- par(family = "serif")
  on.exit(par(old), add = TRUE)

  points <- data.frame(x = c(1, 3, 5, 8), y = c(2, 4, 6, 8))
  plot_regression(points, stats = TRUE)

  expect_equal(par("family"), "serif")
})

test_that("plot_logit() leaves par('family') untouched", {
  pdf(NULL)
  on.exit({ dev.off() }, add = TRUE)

  old <- par(family = "serif")
  on.exit(par(old), add = TRUE)

  points <- data.frame(iv = c(-6, -3, 1, 3, 5, 8), dv = c(0, 0, 0, 1, 1, 1))
  plot_logit(points, formula = dv ~ iv, stats = TRUE)

  expect_equal(par("family"), "serif")
})

test_that("plot_sampling() restores par() even when it errors mid-plot", {
  pdf(NULL)
  on.exit({ dev.off() }, add = TRUE)

  before <- par(c("mfrow", "mar", "cex"))

  # Non-finite plot limits error inside the first panel, after par() is set.
  bad_vars <- list(xmin = NA_real_, xmax = NA_real_, sample_theta = NULL)
  expect_error(
    plot_sampling(rnorm(1000), sample_size = 10, reps = 2, theta = mean,
                  vars = bad_vars)
  )

  expect_equal(par(c("mfrow", "mar", "cex")), before)
})
