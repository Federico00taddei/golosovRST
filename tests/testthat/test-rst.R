test_that("rst_coords computes expected columns", {
  x <- data.frame(
    s1 = c(0.5, 0.4),
    s2 = c(0.3, 0.2),
    s3 = c(0.1, 0.2)
  )

  out <- rst_coords(x)

  expect_true(all(c("sr", "x", "y") %in% names(out)))
  expect_equal(nrow(out), 2)
  expect_true(all(out$x >= 0 & out$x <= 1))
  expect_true(all(out$y >= 0 & out$y <= 1))
})

test_that("validation catches invalid sums", {
  bad <- data.frame(s1 = 0.7, s2 = 0.3, s3 = 0.2)
  expect_error(rst_coords(bad), "s1 \\+ s2 \\+ s3 <= 1")
})

test_that("rst_classify returns subtype column", {
  x <- data.frame(
    s1 = c(0.508, 0.4714, 0.3514),
    s2 = c(0.357, 0.1730, 0.2571),
    s3 = c(0.086, 0.1556, 0.1971)
  )

  out <- rst_classify(x)

  expect_true("golosov_subtype" %in% names(out))
  expect_equal(nrow(out), nrow(x))
})

test_that("plot_rst returns ggplot object", {
  x <- data.frame(
    country = c("uk 2015", "ita 2013", "spa 2015"),
    s1 = c(0.508, 0.4714, 0.3514),
    s2 = c(0.357, 0.1730, 0.2571),
    s3 = c(0.086, 0.1556, 0.1971)
  )

  p <- plot_rst(x, label_col = "country")
  expect_s3_class(p, "ggplot")
})
