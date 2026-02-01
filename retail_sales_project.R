# Import data
sales <- read.csv("retail_store_sales.csv")

# Inspect data
str(sales)
summary(sales)
head(sales)

# Creating vectors
prices <- c(18.5, 29, 21.5, 27.5)
categories <- c("Food", "Beverages", "Milk")
discount <- c(TRUE, FALSE, TRUE)

# Data types check
class(prices)
class(categories)
class(discount)

# Indexing
prices[1]
categories[2]

# Vector arithmetic
prices + 5
prices * 2

# List
sales_list <- list(
  total_transactions = nrow(sales),
  categories = unique(sales$Category),
  avg_sales = mean(sales$Total.Spent)
)

sales_list


# Removing the NA values from total spent column
sales_list <- list(
  total_transactions = nrow(sales),
  categories = unique(sales$Category),
  avg_sales = mean(sales$Total.Spent, na.rm = TRUE)
)

sales_list


sales_clean <- sales

sales_clean <- sales_clean[!is.na(sales_clean$Total.Spent), ]

write.csv(
  sales_clean,
  "retail_store_sales_cleaned.csv",
  row.names = FALSE
)

sales_clean <- read.csv("retail_store_sales_cleaned.csv")

# Access columns
sales_clean$Category
sales_clean$Total.Spent

# Read the CSV file
sales_clean <- read.csv("retail_store_sales_cleaned.csv", stringsAsFactors = FALSE)

# Remove rows with any NA values
sales_clean <- na.omit(sales_clean)

# Overwrite the same CSV file
write.csv(sales_clean, "retail_store_sales_cleaned.csv", row.names = FALSE)

#Removing outliers
# Step 1: Read your CSV
outliers <- read.csv("retail_store_sales_cleaned.csv")

# Step 2: Identify numeric columns
num_cols <- sapply(outliers, is.numeric)

# Step 3: Remove outliers using IQR method for all numeric columns
for(col in names(outliers)[num_cols]) {
  Q1 <- quantile(outliers[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(outliers[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  # Keep only rows within this range
  outliers <- outliers[outliers[[col]] >= lower & outliers[[col]] <= upper, ]
}

# Overwrite the same CSV file
write.csv(outliers, "retail_store_sales_cleaned.csv", row.names = FALSE)


#SUBSETS CREATION
#Creating unique categories subsets
data <- read.csv("retail_store_sales_cleaned.csv")
unique_categories <- unique(data$Category)
unique_categories

#Creating subsets 
subsets <- list()

for (cat in unique_categories) {
  subsets[[cat]] <- subset(data, Category == cat)
}

aggregate(Total.Spent ~ Category, data, sum)

aggregate(Total.Spent ~ Item, data, sum)

aggregate(Total.Spent ~ Location, data, sum)

data <- read.csv("retail_store_sales_cleaned.csv")


#visualization
#Creating bar chart 
category_sales <- data %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Total.Spent))

ggplot(category_sales, aes(x = Category, y = Total_Sales)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Category-wise Total Sales",
    x = "Category",
    y = "Total Sales"
  ) +
  theme_minimal()
ggsave("category_sales_bar.png", width = 17, height = 6)
 


#Creating bar chart 
item_sales <- data %>%
  group_by(Item) %>%
  summarise(Total_Sales = sum(Total.Spent)) %>%
  arrange(desc(Total_Sales)) %>%
  head(10)  # only top 10 items

ggplot(item_sales, aes(x = reorder(Item, Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Items by Sales", x = "Item", y = "Total Sales") +
  theme_minimal()

ggsave("top_item_sales.png", width = 10, height = 6)



#bar chart sales by location 
location_sales <- data %>%
  group_by(Location) %>%
  summarise(Total_Sales = sum(Total.Spent))

ggplot(location_sales, aes(x = reorder(Location, Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Sales by Location", x = "Location", y = "Total Sales") +
  theme_minimal()

ggsave("sales_by_location.png", width = 10, height = 6)



#Creating scatter plot
ggplot(data, aes(x = Price.Per.Unit, y = Quantity, color = Category)) +
  geom_point(alpha = 0.7) +
  labs(title = "Price vs Quantity by Category", x = "Price per Unit", y = "Quantity Sold") +
  theme_minimal()
ggsave("Price_vs_Quantity_by_Price.png", width = 10, height = 6)



#creating line chart
# Load dataset
gemonline <- read.csv("retail_store_sales_cleaned.csv")

# Summarize total spent by category
category_summary <- aggregate(Total.Spent ~ Category, data = gemonline, sum)

# Create line chart
ggplot(category_summary, aes(x = Category, y = Total.Spent, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Total Spent by Category",
    x = "Category",
    y = "Total Spent"
  ) +
  theme_minimal()

ggsave("Line_graph.png", width=17, height=8)

#Tidying strings

retail <- read.csv("retail_store_sales_cleaned.csv")

# Clean strings
retail$Category <- retail$Category |> str_trim() |> str_to_title()
retail$Payment.Method <- retail$Payment.Method |> str_trim() |> str_to_title()
retail$Location <- retail$Location |> str_trim() |> str_to_title()
view(retail)
write.csv(retail, "retail_store_sales_cleaned.csv", row.names = FALSE)


#FILTERING
#filtering online sales
online_sales <- retail |> 
  filter(Location == "Online")

#GROUPING 

#Looking for the repeating customers
top_customers <- retail |> 
  group_by(Customer.ID) |> 
  summarize(Total_Spent = sum(Total.Spent)) |> 
  arrange(desc(Total_Spent))

#Check discounts applied
discount_summary <- retail |> 
  group_by(Discount.Applied) |> 
  summarize(Total_Sales = sum(Total.Spent), Transactions = n())


#CLEANING DUPLICATES
#Clean duplicates (distinct)
  unique_transactions <- retail |> 
  distinct(Customer.ID, Item, .keep_all = TRUE)
  
 # Save unique transactions to a CSV
  write.csv(unique_transactions, "unique_transactions.csv", row.names = FALSE)


#TESTING MY PACKAGE
data <- read.csv("retail_store_sales_cleaned.csv")

total_sales(data)
category_sales(data)
top_items(data, 5)
average_order_value(data)
discount_sales(data)
  
#MENU DESIGN
# CSV read
data <- read.csv("retail_store_sales_cleaned.csv")

menu_column <- "Category"
options <- unique(na.omit(data[[menu_column]]))

menu <- paste0(seq_along(options), ". ", options)
cat("Select a Category:\n")
cat(menu, sep = "\n")

choice <- as.integer(readline("Enter your choice number: "))

# initialize variables (IMPORTANT)
selected_value <- NULL
result <- NULL

if (is.na(choice) || choice < 1 || choice > length(options)) {
  cat("Invalid choice")
} else {
  selected_value <- options[choice]
  result <- subset(data, data[[menu_column]] == selected_value)
  print(result)
}


#USING PIVOT WIDER FUNCTION
data <- read.csv("retail_store_sales_cleaned.csv")
category_summary <- aggregate(
  Total.Spent ~ Category,
  data,
  sum
)
category_wide <- category_summary %>%
  pivot_wider(
    names_from = Category,
    values_from = Total.Spent
  )

print(category_wide)
