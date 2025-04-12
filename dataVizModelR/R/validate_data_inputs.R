# 1. Checks if a data.frame is in correct datatype.

#' Assert that input is a data frame
#'
#' @param df An object to validate
#' @return Stops execution if `df` is not a data frame.
#' @export
assert_dataframe <- function(df) {
  if (!is.data.frame(df)) {
    stop('Input is not a dataframe')
  }
}


# 2. Checks for the correct train-test split data. -> Checks Target/response variable follows expected distribution.

#' Validate train-test split and optional target distribution
#'
#' @param df Original full data frame
#' @param train Training split
#' @param test Testing split
#' @param target_col Optional name of target column for class balance check
#'
#' @return Stops if validation checks fail
#' @export
validate_split_data <- function(df, train, test, target_col = NULL) {
  # a. Expect non-empty train/test
  testthat::expect_true(nrow(train) > 0, info = '! Stop, train is empty')
  testthat::expect_true(nrow(test) > 0, info = '! Stop, test is empty')

  # b. Check total row counts matches original
  testthat::expect_equal(
    object = nrow(train) + nrow(test),
    expected = nrow(df),
    info = '! Stop train rows + test rows does not equal to the original dataframe.'
  )

  # c. Expect cols are the same across train-test split
  testthat::expect_equal(
    object = colnames(train),
    expected = colnames(test),
    info = '! Stop train test cols does not align.'
  )

  # d. Checks the expected distribution of train&test to the original target dataset.
  if (!is.null(target_col)) {
    testthat::expect_true(target_col %in% colnames(df), info = paste0("! Cols ", target_col, " Not Found."))

    cat(' Class Balance Check:\n')
    cat('Original:\n'); print(prop.table(table(df[[target_col]])))
    cat('Train:\n'); print(prop.table(table(train[[target_col]])))
    cat("Test:\n"); print(prop.table(table(test[[target_col]])))
  }
  cat(" Train/test split validation passed.\n")
}


# 3. no outlier or anomalous values - since KNNs are distanced-based, and highly sensitive to outliers.

#' Validate outlier presence in normalized KNN features
#'
#' @param train_norm Normalized training data
#' @param test_norm Normalized test data
#' @param lower Lower bound for expected range (default: -0.1)
#' @param upper Upper bound for expected range (default: 1.1)
#'
#' @return Stops if outliers are found
#' @export
eval_norm_outliers <- function(train_norm, test_norm, lower = -0.1, upper = 1.1){
  assert_dataframe(train_norm)
  assert_dataframe(test_norm)

  # Combine both sets for checking
  combined = rbind(train_norm, test_norm)

  # Find which values are outside of safe normal range
  outlier_mask <- combined < lower | combined > upper;

  # Count how many outliers per column
  outlier_counts <- colSums(outlier_mask, na.rm = TRUE)
  total_outliers <- sum(outlier_counts)

  if (total_outliers > 0) {
    cat("Detected anomalous normalized values :\n")
    print(outlier_counts[outlier_counts > 0])
    stop(paste0("! Warning ", total_outliers, "outlier value found after normalization"))
  }

  cat(" Correct! No anomalous values detected after normalization.")
}


# 4. Evaluate correct category levels

#' Validate categorical factor levels in train/test labels
#'
#' @param train_labels Factor vector of training labels
#' @param test_labels Factor vector of testing labels
#' @param min_count Minimum number of instances required per class (default: 5)
#'
#' @return Stops or warns if levels are mismatched or underrepresented
#' @export
eval_factor_levels <- function(train_labels, test_labels, min_count = 5) {
  # a. Ensure both inputs are factors
  if (!is.factor(train_labels) || !is.factor(test_labels)) {
    stop("Both train_labels and test_labels must be factors.")
  }

  # b. Check if factor levels match
  train_levels <- levels(train_labels)
  test_levels  <- levels(test_labels)

  if (!setequal(train_levels, test_levels)) {
    warning("Train and test factor levels do not match.")
    cat("Train levels:\n"); print(train_levels)
    cat("Test levels:\n");  print(test_levels)
    stop("Inconsistent factor levels between train and test.")
  }

  # c. Check for underrepresented classes in training set
  label_counts <- table(train_labels)
  low_freq_levels <- names(label_counts[label_counts < min_count])

  if (length(low_freq_levels) > 0) {
    warning("Some levels in the training data have fewer than ", min_count, " instances:")
    print(label_counts[low_freq_levels])
  }

  cat("Factor level validation passed: matching levels and sufficient support.\n")
}


# 5. No anomalous correlations between target/resposne variable and features/explanatory variables.

#' Evaluate high correlations between numeric features and the target
#'
#' @param df A data frame including the target column and predictors
#' @param target The name of the target column
#' @param threshold Correlation threshold for flagging (default: 0.9)
#'
#' @return Invisible vector of features with high correlation to the target
#' @export
eval_target_feature_correlations <- function(df, target, threshold = 0.9) {
  if (!target %in% colnames(df)) {
    stop(paste("Target column", target, "not found in data."))
  }

  if (!is.numeric(df[[target]])) {
    warning("Target variable is not numeric. Correlation may not be meaningful.")
    return(invisible(NULL))
  }

  numeric_cols <- sapply(df, is.numeric)
  numeric_df <- df[, numeric_cols]

  cor_values <- cor(numeric_df, use = "complete.obs")[, target]
  cor_values <- cor_values[!names(cor_values) %in% target]

  high_corr <- cor_values[abs(cor_values) > threshold]

  if (length(high_corr) > 0) {
    warning("Features with high correlation to target (possible leakage or overfitting):")
    print(round(high_corr, 3))
  } else {
    cat("No suspiciously high correlations with the target.\n")
  }

  invisible(high_corr)
}


# 6.

#' Warn if there are duplicate rows in either train or test
#'
#' @param train A data frame containing training data
#' @param test A data frame containing test data
#'
#' @return Prints warning if duplicate rows are found
#' @export
warn_on_duplicates <- function(train, test) {
  dup_train <- any(duplicated(train))
  dup_test  <- any(duplicated(test))

  if (dup_train || dup_test) {
    warning("Duplicate rows detected:")
    if (dup_train) cat("  - In training data\n")
    if (dup_test)  cat("  - In test data\n")
  } else {
    cat("No duplicate rows in train/test.\n")
  }
}
