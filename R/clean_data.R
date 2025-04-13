
#' Clean column names
#' @param df A data frame
#' @return A data frame with cleaned column names
#' @export
clean_column_names <- function(df) {
  df <- janitor::clean_names(df)  # janitor::clean_names already applies snake_case
  return(df)
}


#' Split dataset into training and test sets
#' @param df A data frame
#' @param train_size Proportion of data to use for training (default: 0.8)
#' @return A list with train and test data frames
#' @export
split_data <- function(df, train_size = 0.8) {
  set.seed(123)  # For reproducibility
  n <- nrow(df)
  train_index <- sample(seq_len(n), size = floor(train_size * n))
  list(train = df[train_index, ], test = df[-train_index, ])
}



#' Compute row-wise averages for BILL_AMT and PAY_AMT columns
#' @param df A data frame
#' @return A data frame with new avg columns and removed original columns
#' @export
compute_avg_amounts <- function(df) {
  library(dplyr)
  # Identify columns
  bill_amt_cols <- names(df)[grepl("^bill_amt", names(df))]
  pay_amt_cols <- names(df)[grepl("^pay_amt", names(df))]
  
  # Compute averages only if relevant columns exist
  if (length(bill_amt_cols) > 0) {
    df$avg_bill_amt <- rowMeans(df[, bill_amt_cols, drop = FALSE], na.rm = TRUE)
  }
  if (length(pay_amt_cols) > 0) {
    df$avg_pay_amt <- rowMeans(df[, pay_amt_cols, drop = FALSE], na.rm = TRUE)
  }
  # Remove original columns after computing averages
  df <- df %>% select(-all_of(c(bill_amt_cols, pay_amt_cols)))
  return(df)
}
