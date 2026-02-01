average_order_value <- function(data) {
  mean(data$Total.Spent, na.rm = TRUE)
}