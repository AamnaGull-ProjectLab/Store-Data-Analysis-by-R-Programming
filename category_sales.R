category_sales <- function(data) {
  aggregate(Total.Spent ~ Category, data, sum)
}