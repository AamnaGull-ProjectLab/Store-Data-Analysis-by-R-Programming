total_sales <- function(data) {
  sum(data$Total.Spent, na.rm = TRUE)
}