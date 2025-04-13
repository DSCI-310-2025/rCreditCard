library(testthat)
library(rCreditCard)

# Test normalize()
test_that("normalize scales between 0 and 1", {
  x <- c(5, 10, 15)
  result <- normalize(x)
  expect_true(all(result >= 0 & result <= 1))
  expect_equal(result[1], 0)
  expect_equal(result[3], 1)
})

test_that("normalize returns NaN for constant input", {
  x <- rep(5, 3)
  result <- normalize(x)
  expect_true(all(is.nan(result)))
})

# Test split_dataset()
test_that("split_dataset splits into 80/20", {
  df <- data.frame(x = 1:100, y = rnorm(100))
  splits <- split_dataset(df)
  expect_named(splits, c("train", "test"))
  expect_equal(nrow(splits$train), 80)
  expect_equal(nrow(splits$test), 20)
})

# Test prepare_knn_data()
test_that("prepare_knn_data returns proper structure", {
  df <- data.frame(
    default_payment_next_month = sample(0:1, 100, replace = TRUE),
    LIMIT_BAL = runif(100, 10000, 500000),
    AGE = sample(20:60, 100, replace = TRUE)
  )
  splits <- split_dataset(df)
  result <- prepare_knn_data(splits$train, splits$test)
  expect_named(result, c("train", "test", "train_labels", "test_labels"))
  expect_equal(nrow(result$train), nrow(splits$train))
  expect_equal(nrow(result$test), nrow(splits$test))
})

test_that("prepare_knn_data throws error if target column missing", {
  df <- data.frame(LIMIT_BAL = runif(10), AGE = runif(10))
  expect_error(prepare_knn_data(df, df), "Target column")
})

# Test evaluate_k_values()
test_that("evaluate_k_values returns accuracy for each k", {
  df <- data.frame(
    default_payment_next_month = sample(0:1, 60, replace = TRUE),
    LIMIT_BAL = runif(60, 10000, 500000),
    AGE = sample(20:60, 60, replace = TRUE)
  )
  splits <- split_dataset(df)
  knn_data <- prepare_knn_data(splits$train, splits$test)
  result <- evaluate_k_values(knn_data$train, knn_data$test, knn_data$train_labels, knn_data$test_labels, k_values = 1:3)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_named(result, c("k", "accuracy"))
  expect_true(all(result$accuracy >= 0 & result$accuracy <= 1))
})

# Test save_accuracy_plot()
test_that("save_accuracy_plot creates a PNG file", {
  tmp <- tempfile(fileext = ".png")
  df <- data.frame(k = 1:3, accuracy = c(0.7, 0.75, 0.8))
  save_accuracy_plot(df, tmp)
  expect_true(file.exists(tmp))
  unlink(tmp)
})

# Test save_model_outputs()
test_that("save_model_outputs creates files", {
  tmp_prefix <- tempfile()
  actual <- factor(sample(0:1, 20, replace = TRUE))
  predicted <- actual
  save_model_outputs(predicted, actual, tmp_prefix)
  
  expect_true(file.exists(paste0(tmp_prefix, "_confusion_matrix.csv")))
  expect_true(file.exists(paste0(tmp_prefix, "_performance_metrics.txt")))
  
  unlink(paste0(tmp_prefix, "_confusion_matrix.csv"))
  unlink(paste0(tmp_prefix, "_performance_metrics.txt"))
})