top_items <- function(data, n = 5) {
  sales <- aggregate(Quantity ~ Item, data, sum)
  sales[order(-sales$Quantity), ][1:n, ]
}