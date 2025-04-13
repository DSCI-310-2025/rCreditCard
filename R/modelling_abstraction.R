#' Normalize a numeric vector using Min-Max scaling
#'
#' Scales a numeric vector to be between 0 and 1.
#'
#' @param x A numeric vector
#' @return A normalized vector between 0 and 1
#'
#' @examples
#' normalize(c(1, 5, 10))
#'
#' @export

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#' Split dataset into 80% training and 20% testing
#'
#' @param df A data frame
#' @return A list with 'train' and 'test' data frames

#' @examples
#' split_dataset(mtcars)

#' @export
split_dataset <- function(df) {
  n <- nrow(df)
  train_index <- 1:round(0.8 * n)
  list(train = df[train_index, ], test = df[-train_index, ])
}

#' Prepare normalized predictors and extract labels
#'
#' @param df_train Training data
#' @param df_test Testing data
#' @param target Column name of target variable
#' @return A list with normalized train/test data and labels
#' @examples
#' # prepare_knn_data(train_df, test_df, "default_payment_next_month")
#' @export
prepare_knn_data <- function(df_train, df_test, target = "default_payment_next_month") {
  if (!(target %in% colnames(df_train)) || !(target %in% colnames(df_test))) {
    stop(paste("Target column", target, "not found in data."))
  }

  numeric_columns <- sapply(df_train, is.numeric)

  train_labels <- as.factor(df_train[[target]])
  test_labels <- as.factor(df_test[[target]])

  train_norm <- as.data.frame(lapply(df_train[, numeric_columns], normalize))
  test_norm <- as.data.frame(lapply(df_test[, numeric_columns], normalize))

  train_norm <- train_norm[, colnames(train_norm) != target]
  test_norm <- test_norm[, colnames(test_norm) != target]

  list(
    train = train_norm,
    test = test_norm,
    train_labels = train_labels,
    test_labels = test_labels
  )
}

#' Evaluate accuracy of KNN across multiple k values
#'
#' @param train Normalized training predictors
#' @param test Normalized test predictors
#' @param train_labels Training labels
#' @param test_labels Test labels
#' @param k_values Vector of k values to evaluate (default: 1 to 20)
#' @return A data frame with k and corresponding accuracy
#' @export
evaluate_k_values <- function(train, test, train_labels, test_labels, k_values = 1:20) {
  accuracy_results <- data.frame(k = k_values, accuracy = NA)

  for (i in seq_along(k_values)) {
    pred <- class::knn(train, test, cl = train_labels, k = k_values[i])
    accuracy_results$accuracy[i] <- mean(pred == test_labels)
  }

  return(accuracy_results)
}

#' Save accuracy vs. k plot to a file
#'
#' Creates a line plot from KNN accuracy results and saves it as a PNG.
#'
#' @param accuracy_df Data frame with k and accuracy
#' @param out_path Output file path for the plot (PNG)
#'
#' @examples
#' # save_accuracy_plot(df, "knn_accuracy.png")
#'

#' @export
save_accuracy_plot <- function(accuracy_df, out_path) {
  p <- ggplot2::ggplot(accuracy_df, ggplot2::aes(x = k, y = accuracy)) +
    ggplot2::geom_line(color = "blue") +
    ggplot2::geom_point(color = "red", size = 3) +
    ggplot2::labs(
      title = "KNN Accuracy vs K",
      x = "Number of Neighbors (k)",
      y = "Accuracy"
    ) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(out_path, plot = p, width = 7, height = 5)
}

#' Create a KNN Accuracy Plot
#'
#' @param accuracy_df A data frame with k and accuracy columns
#'
#' @return A ggplot object showing accuracy vs. k
#' @examples
#' # accuracy_results <- data.frame(k = 1:10, accuracy = runif(10))
#' # make_accuracy_plot(accuracy_results)
#' @export
make_accuracy_plot <- function(accuracy_df) {
  ggplot2::ggplot(accuracy_df, ggplot2::aes(x = k, y = accuracy)) +
    ggplot2::geom_line(color = "blue") +
    ggplot2::geom_point(color = "red", size = 3) +
    ggplot2::labs(
      title = "KNN Accuracy vs K",
      x = "Number of Neighbors (k)",
      y = "Accuracy"
    ) +
    ggplot2::theme_minimal()
}

#' Save confusion matrix and performance metrics to files
#'
#' Saves confusion matrix and model performance to CSV and TXT files.
#'
#' @param predictions Predicted labels
#' @param actual Actual labels
#' @param prefix Output file path prefix (no extension)
#'
#' @examples
#' # save_model_outputs(preds, actuals, "results/knn")
#'
#' @export
save_model_outputs <- function(predictions, actual, prefix) {
  conf_mat <- table(Predicted = predictions, Actual = actual)
  utils::write.csv(as.data.frame(conf_mat), paste0(prefix, "_confusion_matrix.csv"))

  conf <- caret::confusionMatrix(as.factor(predictions), as.factor(actual))
  utils::write.table(conf$overall, paste0(prefix, "_performance_metrics.txt"), sep = "\t")
  print(conf)
}
