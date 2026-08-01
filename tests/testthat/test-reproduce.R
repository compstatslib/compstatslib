# Reproduction-call formatting and the two S3 return classes that every
# gadget uses to hand its state back to the console.

test_that(".format_call() omits arguments sitting at their defaults", {
  defaults <- list(diff = 0.5, sd = 4, n = 100, alpha = 0.05,
                   error_matrix = FALSE)

  expect_equal(
    .format_call("plot_t_test", defaults, defaults),
    "plot_t_test()"
  )
  expect_equal(
    .format_call("plot_t_test",
                 utils::modifyList(defaults, list(n = 250)),
                 defaults),
    "plot_t_test(n = 250)"
  )
})

test_that(".format_call() compares numeric defaults tolerantly", {
  # shiny sliders hand back integers where the plot function defaults to double
  expect_equal(
    .format_call("plot_t_test", list(n = 100L), list(n = 100)),
    "plot_t_test()"
  )
})

test_that(".format_call() prints integers without the L suffix", {
  expect_equal(
    .format_call("plot_t_test", list(n = 250L), list(n = 100)),
    "plot_t_test(n = 250)"
  )
})

test_that(".format_call() compares formulas by what they say, not identity", {
  # A formula built in the caller's frame has a different environment than
  # the plot function's default, so identical() would never drop it.
  elsewhere <- local(y ~ x)
  expect_false(identical(y ~ x, elsewhere))

  expect_equal(
    .format_call("plot_logit", list(formula = y ~ x),
                 list(formula = elsewhere)),
    "plot_logit()"
  )
  expect_equal(
    .format_call("plot_logit", list(formula = dv ~ iv),
                 list(formula = elsewhere)),
    "plot_logit(formula = dv ~ iv)"
  )
})

test_that("compstatslib_args() shows print-only stand-ins for bulky values", {
  res <- compstatslib_args(
    list(population = rnorm(1000), sample_size = 100, theta = median),
    fn = "plot_sampling",
    display = list(population = .verbatim("my_pop"),
                   theta = .verbatim("median"))
  )
  expect_output(
    print(res),
    "plot_sampling(population = my_pop, sample_size = 100, theta = median)",
    fixed = TRUE
  )
  # The real values are still there for do.call()
  expect_length(res$population, 1000)
  expect_true(is.function(res$theta))
})

test_that(".format_call() names every argument and keeps order", {
  expect_equal(
    .format_call("plot_matrix_inverse", list(x1 = 1, y1 = 2, x2 = 2, y2 = 1)),
    "plot_matrix_inverse(x1 = 1, y1 = 2, x2 = 2, y2 = 1)"
  )
})

test_that(".format_call() deparses vectors and formulas readably", {
  out <- .format_call("plot_scatter3d",
                      list(aspect = c(1, 2, 4), titles = list(x = "IV")))
  expect_match(out, "aspect = c(1, 2, 4)", fixed = TRUE)
  expect_match(out, 'titles = list(x = "IV")', fixed = TRUE)

  out <- .format_call("plot_logit", list(formula = dv ~ iv))
  expect_equal(out, "plot_logit(formula = dv ~ iv)")
})

test_that(".format_call() abbreviates a dataframe rather than dumping it", {
  pts <- data.frame(x = runif(42), y = runif(42))
  expect_equal(
    .format_call("plot_regression", list(points = pts)),
    "plot_regression(points = <42 points>)"
  )
})

test_that(".format_call() emits verbatim values as written", {
  out <- .format_call("plot_sampling",
                      list(population = .verbatim("my_pop"),
                           theta = .verbatim("median")))
  expect_equal(out, "plot_sampling(population = my_pop, theta = median)")
})

test_that(".format_call() output for settings round-trips through parse()", {
  out <- .format_call("plot_t_test",
                      list(diff = 1.2, sd = 3, n = 250, alpha = 0.01,
                           error_matrix = TRUE))
  parsed <- parse(text = out)[[1]]
  expect_equal(as.character(parsed[[1]]), "plot_t_test")
  expect_equal(parsed$n, 250)
  expect_true(parsed$error_matrix)
})

test_that("compstatslib_args() prints its reproduction call", {
  res <- compstatslib_args(list(x1 = 1, y1 = 2, x2 = 2, y2 = 1),
                           fn = "plot_matrix_inverse")
  expect_s3_class(res, "compstatslib_args")
  expect_output(print(res),
                "plot_matrix_inverse(x1 = 1, y1 = 2, x2 = 2, y2 = 1)",
                fixed = TRUE)
})

test_that("compstatslib_args() stays a plain list for do.call()", {
  res <- compstatslib_args(list(x1 = 1, y1 = 2, x2 = 2, y2 = 1),
                           fn = "plot_matrix_inverse")
  expect_type(unclass(res), "list")
  expect_equal(res$x1, 1)
  expect_equal(names(res), c("x1", "y1", "x2", "y2"))

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(do.call(plot_matrix_inverse, res), NA)
})

test_that("compstatslib_points() prints the call, then the dataframe", {
  pts <- data.frame(x = c(1, 3, 5), y = c(2, 4, 6))
  res <- compstatslib_points(pts, fn = "plot_regression")

  out <- capture.output(print(res))
  expect_equal(out[1], "plot_regression(points)")
  expect_equal(out[2], "")
  expect_true(any(grepl("^1", out[-(1:2)])))
})

test_that("compstatslib_points() carries extra args into the printed call", {
  pts <- data.frame(iv = c(-6, 1, 5), dv = c(0, 1, 1))
  res <- compstatslib_points(pts, fn = "plot_logit",
                             args = list(formula = dv ~ iv,
                                         min_x = 0, max_x = 50),
                             defaults = list(min_x = 0, max_x = 1))
  expect_output(print(res),
                "plot_logit(points, formula = dv ~ iv, max_x = 50)",
                fixed = TRUE)
})

test_that("compstatslib_points() still behaves as a dataframe", {
  pts <- data.frame(x = c(1, 3, 5, 8), y = c(2, 4, 6, 8))
  res <- compstatslib_points(pts, fn = "plot_regression")

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 4)
  expect_equal(res$x, c(1, 3, 5, 8))
  expect_equal(nrow(res[1:2, ]), 2)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(plot_regression(res), NA)
})

test_that("derived state rides along as an attribute and is not printed", {
  pts <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 8))
  pca <- prcomp(pts)
  res <- compstatslib_points(pts, fn = "plot_pca",
                             args = list(meancenter = TRUE),
                             defaults = list(meancenter = TRUE),
                             extra = list(pca = pca))

  expect_s3_class(attr(res, "pca"), "prcomp")
  out <- paste(capture.output(print(res)), collapse = "\n")
  expect_false(grepl("prcomp|sdev|rotation", out))

  vars <- list(sample_theta = c(1, 2, 3))
  res2 <- compstatslib_args(list(sample_size = 10), fn = "plot_sampling",
                            extra = list(vars = vars))
  expect_equal(attr(res2, "vars"), vars)
  expect_output(print(res2), "plot_sampling(sample_size = 10)", fixed = TRUE)
})
