discount_sales <- function(data) {
  aggregate(Total.Spent ~ Discount.Applied, data, sum)
}