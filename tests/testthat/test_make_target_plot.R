library(testthat)
library(ggplot2)
source("../../R/make_target_plot.R")  # Adjust path as needed

test_that("make_target_plot returns a ggplot object", {
  df <- data.frame(default_payment_next_month = as.factor(c(0, 1, 1, 0, 0)))
  plot <- make_target_plot(df)
  expect_s3_class(plot, "ggplot")
})

test_that("make_target_plot includes expected aesthetics", {
  df <- data.frame(default_payment_next_month = as.factor(rep(c(0, 1), times = 5)))
  plot <- make_target_plot(df)
  
  mapping <- plot$mapping
  
  expect_true("x" %in% names(mapping))
  expect_true("fill" %in% names(mapping))
  expect_equal(as_label(mapping$x), "default_payment_next_month")
  expect_equal(as_label(mapping$fill), "default_payment_next_month")
})

test_that("make_target_plot fails gracefully if column is missing", {
  df <- data.frame(not_target = c(0, 1, 1))
  expect_error(make_target_plot(df), "Column 'default_payment_next_month' not found in the dataframe.")
})