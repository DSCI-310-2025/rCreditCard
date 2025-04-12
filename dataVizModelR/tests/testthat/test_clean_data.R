library(testthat)

test_that("clean_column_names converts column names correctly", {
  df <- data.frame("column one" = 1, "column two" = 2)  # Spaces in column names
  cleaned_df <- clean_column_names(df)
  expect_equal(colnames(cleaned_df), c("column_one", "column_two"))  # Match snake_case format
})

test_that("split_data splits data correctly", {
  df <- data.frame(x = 1:10, y = 11:20)
  split <- split_data(df, train_size = 0.7)
  expect_equal(nrow(split$train) + nrow(split$test), nrow(df))
})

test_that("compute_avg_amounts creates new avg columns", {
  df <- data.frame(
    bill_amt1 = c(100, 200), 
    bill_amt2 = c(300, 400), 
    pay_amt1 = c(50, 100),
    pay_amt2 = c(20, 50)  # Add another `pay_amt` column
  )
  new_df <- compute_avg_amounts(df)
  expect_true("avg_bill_amt" %in% colnames(new_df))
  expect_true("avg_pay_amt" %in% colnames(new_df))
})