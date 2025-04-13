#' Create a bar plot for the target variable distribution
#'
#' @param df A data frame containing the target column "default_payment_next_month"
#' @return A ggplot object showing the bar plot
#' @examples
#' make_target_plot(df)  # df must include default_payment_next_month

#' @export
make_target_plot <- function(df) {
  if (!"default_payment_next_month" %in% colnames(df)) {
    stop("Column 'default_payment_next_month' not found in the dataframe.")
  }
  df$default_payment_next_month <- as.factor(df$default_payment_next_month)
  
  ggplot(df, aes(x = default_payment_next_month, fill = default_payment_next_month)) +
    geom_bar() +
    labs(title = "Distribution of Default Payments",
         x = "Default (0 = No, 1 = Yes)",
         y = "Count") +
    theme_minimal()
}
