# Linear regression plotting
test_that("Plotting regression points should not produce errors", {
  x <- runif(20) * 50
  y <- (0.5 * x) + rnorm(20)
  points <- data.frame(x, y)
  expect_error(plot_regression(points), NA)
})

# t-test plotting
test_that("Plotting t-test visualization should not produce errors", {
  expect_error(plot_t_test(), NA)
})

# Logit plotting
test_that("Plotting logit points called (x, y) should not produce errors", {
  x <- runif(20) * 50
  y <- sample(c(0, 1), 20, replace=TRUE)
  points <- data.frame(x, y)
  expect_error(plot_logit(points), NA)
})

test_that("Plotting logit points called (ant, out) should not produce errors", {
  ant <- runif(20) * 50
  out <- sample(c(0, 1), 20, replace=TRUE)
  points <- data.frame(out, ant)
  expect_error(plot_logit(points, formula = out ~ ant), NA)
})

# Sampling plotting
test_that("Plotting sampling simulation should not produce errors", {
  expect_error(
    plot_sampling(rnorm(100000), sample_size = 100, reps = 50, theta = median),
    NA)
})

# Inverse matrix plotting
test_that("Plotting matrix inverse should not produce errors", {
  expect_error(
    plot_matrix_inverse(1, 2, 2, 1), NA)
})

# PCA plotting
test_that("Plotting PCA with no points should not produce errors", {
  expect_error(plot_pca(), NA)
})

test_that("Plotting PCA with 2 points should not produce errors", {
  points <- data.frame(x = c(1, 2), y = c(3, 4))
  expect_error(plot_pca(points), NA)
})

test_that("Plotting PCA with 3+ points should return prcomp result", {
  points <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 8))
  result <- plot_pca(points)
  expect_s3_class(result, "prcomp")
})

# 3D moderation plotting
test_that("Plotting 3D moderation with defaults should not produce errors", {
  expect_error(plot_moderation_3d(), NA)
})

test_that("Plotting 3D moderation with additive formula should not error", {
  expect_error(plot_moderation_3d(y ~ x + z, moderation_data), NA)
})

test_that("Plotting 3D moderation with custom column names should not error", {
  set.seed(1)
  df <- data.frame(
    out  = rnorm(50),
    pred = rnorm(50),
    modr = rnorm(50)
  )
  expect_error(plot_moderation_3d(out ~ pred * modr, df), NA)
})

test_that("Plotting 3D moderation returns a trellis object", {
  result <- plot_moderation_3d()
  expect_s3_class(result, "trellis")
})

test_that("Plotting 3D moderation with explicit zlim should not error", {
  expect_error(plot_moderation_3d(zlim = c(-5, 10)), NA)
})

test_that("Plotting 3D moderation with multi-predictor formula and explicit iv/mod", {
  set.seed(2)
  df <- data.frame(
    y  = rnorm(60),
    a  = rnorm(60),
    b  = rnorm(60),
    c  = rnorm(60),
    d  = rnorm(60)
  )
  expect_error(
    plot_moderation_3d(y ~ a + b + c + d + c:d, df, iv = "c", mod = "d"),
    NA
  )
})

test_that("Plotting 3D moderation with bundled data and `w` covariate works", {
  expect_error(
    plot_moderation_3d(y ~ x + z + w + x:z, moderation_data,
                       iv = "x", mod = "z"),
    NA
  )
})

test_that("Plotting 3D moderation emits a hold-out note when extra predictors present", {
  expect_message(
    plot_moderation_3d(y ~ x + z + w + x:z, moderation_data,
                       iv = "x", mod = "z"),
    "held at their typical values"
  )
})

test_that("Plotting 3D moderation is silent when only IV and moderator are present", {
  expect_no_message(
    plot_moderation_3d(y ~ x * z, moderation_data)
  )
})

test_that("Plotting 3D moderation rejects categorical variable on a plot axis", {
  set.seed(6)
  df <- data.frame(
    y     = rnorm(40),
    iv    = rnorm(40),
    group = factor(sample(c("ctrl", "treat"), 40, replace = TRUE))
  )
  expect_error(
    plot_moderation_3d(y ~ iv * group, df),
    "does not support categorical"
  )
})

test_that("Plotting 3D moderation with multi-predictor formula errors when iv/mod missing", {
  set.seed(3)
  df <- data.frame(
    y = rnorm(40), a = rnorm(40), b = rnorm(40), c = rnorm(40)
  )
  expect_error(
    plot_moderation_3d(y ~ a + b + c, df),
    "must specify"
  )
})

test_that("Plotting 3D moderation errors when iv name not in formula predictors", {
  set.seed(4)
  df <- data.frame(
    y = rnorm(40), a = rnorm(40), b = rnorm(40), c = rnorm(40)
  )
  expect_error(
    plot_moderation_3d(y ~ a + b + c, df, iv = "q", mod = "b"),
    "not a predictor"
  )
})

test_that("Plotting 3D moderation holds factor covariate at first level", {
  set.seed(5)
  df <- data.frame(
    y     = rnorm(80),
    iv    = rnorm(80),
    mod   = rnorm(80),
    group = factor(sample(c("ctrl", "treat"), 80, replace = TRUE))
  )
  expect_error(
    plot_moderation_3d(y ~ group + iv + mod + iv:mod, df,
                       iv = "iv", mod = "mod"),
    NA
  )
})

# 3D scatter plotting
test_that("plot_scatter3d() with explicit x/y/z runs without error", {
  set.seed(1)
  df <- data.frame(
    a = rnorm(30),
    b = rnorm(30),
    c = rnorm(30)
  )
  expect_error(plot_scatter3d(df, x = "a", y = "b", z = "c"), NA)
})

test_that("plot_scatter3d() returns a plotly htmlwidget", {
  set.seed(2)
  df <- data.frame(
    a = rnorm(30),
    b = rnorm(30),
    c = rnorm(30)
  )
  result <- plot_scatter3d(df, x = "a", y = "b", z = "c")
  expect_s3_class(result, "plotly")
})

test_that("plot_scatter3d() is silent when data has exactly 3 numeric columns", {
  set.seed(3)
  df <- data.frame(
    a = rnorm(30),
    b = rnorm(30),
    c = rnorm(30)
  )
  expect_no_message(plot_scatter3d(df))
})

test_that("plot_scatter3d() messages chosen and skipped columns when more than 3 numeric", {
  set.seed(4)
  df <- data.frame(
    a = rnorm(30),
    b = rnorm(30),
    c = rnorm(30),
    d = rnorm(30),
    e = rnorm(30)
  )
  expect_message(plot_scatter3d(df), "a.*b.*c")
  expect_message(plot_scatter3d(df), "d.*e")
})

test_that("plot_scatter3d() errors when data has fewer than 3 numeric columns", {
  set.seed(5)
  df <- data.frame(
    a = rnorm(20),
    b = rnorm(20),
    grp = factor(sample(c("x", "y"), 20, replace = TRUE))
  )
  expect_error(plot_scatter3d(df), "at least 3 numeric")
})

test_that("plot_scatter3d() errors when a named column is missing from data", {
  set.seed(6)
  df <- data.frame(
    a = rnorm(20),
    b = rnorm(20),
    c = rnorm(20)
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "missing"),
    "missing"
  )
})

test_that("plot_scatter3d() errors when an axis column is non-numeric", {
  set.seed(7)
  df <- data.frame(
    a = rnorm(20),
    b = rnorm(20),
    fac = factor(sample(c("p", "q"), 20, replace = TRUE)),
    chr = sample(letters[1:3], 20, replace = TRUE),
    bool = sample(c(TRUE, FALSE), 20, replace = TRUE),
    dte = as.Date("2025-01-01") + 1:20
  )
  for (bad in c("fac", "chr", "bool", "dte")) {
    expect_error(
      plot_scatter3d(df, x = "a", y = "b", z = bad),
      "numeric"
    )
  }
})

test_that("plot_scatter3d() accepts color column of any type", {
  set.seed(8)
  df <- data.frame(
    a = rnorm(30),
    b = rnorm(30),
    c = rnorm(30),
    num_grp = runif(30),
    fac_grp = factor(sample(c("alpha", "beta"), 30, replace = TRUE)),
    chr_grp = sample(c("p", "q", "r"), 30, replace = TRUE)
  )
  for (col in c("num_grp", "fac_grp", "chr_grp")) {
    expect_error(
      plot_scatter3d(df, x = "a", y = "b", z = "c", color = col),
      NA
    )
  }
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", color = "missing"),
    "missing"
  )
})

test_that("plot_scatter3d() validates aspect (length-3 positive numeric)", {
  set.seed(9)
  df <- data.frame(
    a = rnorm(20),
    b = rnorm(20),
    c = rnorm(20)
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", aspect = c(1, 2, 4)),
    NA
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", aspect = c(1, 2)),
    "length 3"
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", aspect = c(1, 2, 3, 4)),
    "length 3"
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", aspect = c(1, 0, 1)),
    "positive"
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", aspect = c(1, -1, 1)),
    "positive"
  )
})

test_that("plot_scatter3d() validates opacity (in (0, 1])", {
  set.seed(10)
  df <- data.frame(
    a = rnorm(20),
    b = rnorm(20),
    c = rnorm(20)
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", opacity = 0.3),
    NA
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", opacity = 1),
    NA
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", opacity = 0),
    "opacity"
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", opacity = -0.1),
    "opacity"
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", opacity = 1.5),
    "opacity"
  )
})

test_that("plot_scatter3d() validates size (positive numeric)", {
  set.seed(11)
  df <- data.frame(
    a = rnorm(20),
    b = rnorm(20),
    c = rnorm(20)
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", size = 10),
    NA
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", size = 0),
    "size"
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", size = -2),
    "size"
  )
})

test_that("plot_scatter3d() validates camera arg", {
  set.seed(12)
  df <- data.frame(a = rnorm(20), b = rnorm(20), c = rnorm(20))
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c",
                   camera = list(eye = list(x = 2, y = 2, z = 2))),
    NA
  )
  expect_error(
    plot_scatter3d(df, x = "a", y = "b", z = "c", camera = "bad"),
    "camera"
  )
})

test_that("plot_scatter3d() rewrites unquoted-identifier errors with a quote hint", {
  set.seed(13)
  df <- data.frame(a = rnorm(20), b = rnorm(20), c = rnorm(20))
  # `nope` is intentionally undefined in this scope
  expect_error(
    plot_scatter3d(df, x = nope, y = "b", z = "c"),
    "character column name"
  )
  expect_error(
    plot_scatter3d(df, x = 1, y = "b", z = "c"),
    "character"
  )
})

unlink("Rplots.pdf")
