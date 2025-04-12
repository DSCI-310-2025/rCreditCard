#' Create scatter plots of features vs default status with faceting
#'
#' This function takes a dataframe and a vector of feature column names, then
#' produces a faceted scatter plot showing the relationship between each feature
#' and the binary default status.
#'
#' @param df A data frame containing the features and a column named `default_payment_next_month`
#' @param features A character vector of column names to plot
#'
#' @return A `ggplot` object
#' @export
#'
#' @examples
#' # make_scatter_facet_plot(df, c("LIMIT_BAL", "AGE"))
make_scatter_facet_plot <- function(df, features) {
  assert_dataframe(df)

  missing <- setdiff(features, colnames(df))
  if (length(missing) > 0) stop(paste("Missing columns:", paste(missing, collapse = ", ")))
  
  df$default_payment_next_month <- as.factor(df$default_payment_next_month)
  
  df_long <- df |>
    tidyr::pivot_longer(cols = tidyselect::all_of(features), names_to = "Feature", values_to = "Value")
  
  ggplot2::ggplot(df_long, ggplot2::aes(x = Value, y = default_payment_next_month, color = default_payment_next_month)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
    ggplot2::facet_wrap(~ Feature, scales = "free_x") +
    ggplot2::labs(
      title = "Scatter Plots of Numeric Features vs Default Payment",
      x = "Feature Value",
      y = "Default Payment (0 = No, 1 = Yes)",
      color = "Default Status"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 14)
    )
}
