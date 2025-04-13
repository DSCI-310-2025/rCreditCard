
library(testthat)
library(rCreditCard)

test_that("make_scatter_facet_plot works for valid input", {
  df <- data.frame(
    default_payment_next_month = sample(0:1, 100, replace = TRUE),
    LIMIT_BAL = rnorm(100, 200000, 50000),
    AGE = sample(20:60, 100, replace = TRUE)
  )
  
  p <- make_scatter_facet_plot(df, c("LIMIT_BAL", "AGE"))
  expect_s3_class(p, "ggplot")
})

test_that("make_scatter_facet_plot throws error for missing columns", {
  df <- data.frame(
    default_payment_next_month = sample(0:1, 50, replace = TRUE),
    LIMIT_BAL = rnorm(50)
  )
  
  expect_error(
    make_scatter_facet_plot(df, c("LIMIT_BAL", "AGE")),
    "Missing columns: AGE"
  )
})
