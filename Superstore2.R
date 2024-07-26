library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)

# Read the dfset
df <- read.csv("/Users/jayanthmadhav/Desktop/sales.csv")

# Summary of the dfset
summary(df)

# Identify missing values
missing_values <- colSums(is.na(df))
print(missing_values[missing_values > 0])

# Imputation for numerical variables (e.g., Sales, Quantity, Profit)
mean_sales <- mean(df$Sales, na.rm = TRUE)
df$Sales[is.na(df$Sales)] <- mean_sales
mean_quantity <- mean(df$Quantity, na.rm = TRUE)
df$Quantity[is.na(df$Quantity)] <- mean_quantity
mean_profit <- mean(df$Profit, na.rm = TRUE)
df$Profit[is.na(df$Profit)] <- mean_profit

# Imputation for categorical variables (e.g., Ship.Mode, Segment, Region)
mode_ship_mode <- names(sort(table(df$Ship.Mode), decreasing = TRUE))[1]
df$Ship.Mode[is.na(df$Ship.Mode)] <- mode_ship_mode
mode_segment <- names(sort(table(df$Segment), decreasing = TRUE))[1]
df$Segment[is.na(df$Segment)] <- mode_segment
mode_region <- names(sort(table(df$Region), decreasing = TRUE))[1]
df$Region[is.na(df$Region)] <- mode_region

# Standardize variable names
names(df) <- tolower(names(df))  # Convert variable names to lowercase

# Replace spaces with underscores
names(df) <- gsub(" ", "_", names(df))

# Convert categorical variables to factors
categorical_vars <- c("ship.mode", "segment", "region", "category", "sub.category")
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)

# Display the structure of the dfset
str(df)

# Define winsorization function
winsorize <- function(x, trim = 0.05) {
  q <- quantile(x, probs = c(trim, 1 - trim), na.rm = TRUE)
  
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}

# Identify and winsorize outliers for numerical variables
numerical_vars <- c("sales", "quantity", "profit")
for (var in numerical_vars) {
  df[[var]] <- winsorize(df[[var]])
}

# Display summary statistics after winsorization
summary(df[numerical_vars])

# Define min-max scaling function
min_max_scaling <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply min-max scaling to numerical variables
numerical_vars <- c("sales", "quantity", "profit")
for (var in numerical_vars) {
  df[[var]] <- min_max_scaling(df[[var]])
}

# Display summary statistics after min-max scaling
summary(df[numerical_vars])

# Define z-score normalization function
z_score_normalization <- function(x) {
  (x - mean(x)) / sd(x)
}

# Apply z-score normalization to numerical variables
numerical_vars <- c("sales", "quantity", "profit")
for (var in numerical_vars) {
  df[[var]] <- z_score_normalization(df[[var]])
}

# Display summary statistics after z-score normalization
summary(df[numerical_vars])

# Profit Margin
df$profit_margin <- df$profit / df$sales


# Load the lubridate package
library(lubridate)

# Assuming your dataset is named "data"

# Convert "order.date" and "ship.date" columns to Date objects
df$order.date <- as.Date(df$order.date)
df$ship.date <- as.Date(df$ship.date)

# Handle missing values by replacing them with the most common date in each column
most_common_order_date <- as.Date(names(sort(table(df$order.date), decreasing = TRUE))[1])
most_common_ship_date <- as.Date(names(sort(table(df$ship.date), decreasing = TRUE))[1])

df$order.date[is.na(df$order.date)] <- most_common_order_date
df$ship.date[is.na(df$ship.date)] <- most_common_ship_date

# Extract day, month, and year components for "order.date"
df$order_day <- day(df$order.date)
df$order_month <- month(df$order.date)
df$order_year <- year(df$order.date)

# Extract day, month, and year components for "ship.date"
df$ship_day <- day(df$ship.date)
df$ship_month <- month(df$ship.date)
df$ship_year <- year(df$ship.date)

# Now, data contains separate columns for day, month, and year for both order and ship dates

# Average Sales per Day (Monthly)
df$monthly_sales <- as.yearmon(df$order.date)
df$avg_sales_per_day <- ave(df$sales, df$monthly_sales, FUN = mean) / days_in_month(df$monthly_sales)
df$order_processing_time <- as.numeric(difftime(df$ship.date, df$order.date, units = "days"))
names(df)
# Assuming your dataset is named "df"

# Create dummy variables for ship.date
df <- cbind(df, model.matrix(~ ship.date - 1, data = df))

# Create dummy variables for segment
df <- cbind(df, model.matrix(~ segment - 1, data = df))

# Create dummy variables for region
df <- cbind(df, model.matrix(~ region - 1, data = df))

# Create dummy variables for category
df <- cbind(df, model.matrix(~ category - 1, data = df))

# Create dummy variables for sub.category
df <- cbind(df, model.matrix(~ sub.category - 1, data = df))

df$sales <- scale(df$sales)
df$quantity <- scale(df$quantity)
df$discount <- scale(df$discount)
df$profit <- scale(df$profit)
df$profit_margin <- scale(df$profit_margin)


df$interaction_term <- df$quantity * df$discount
str(df)



# Check for duplicate records
# Count the number of duplicate rows
num_duplicates <- sum(duplicated(df))
print(num_duplicates)
# Remove duplicate records if any
df <- df[!duplicated(df), ]
print(num_duplicates)
# Check for missing values in each column
# Display summary of missing values
summary(is.na(df))
print(is.na(df))
# Impute missing values if necessary
# For numeric columns, replace missing values with median or mean
# For categorical columns, replace missing values with the mode or a specific category

# Check for outliers in numeric columns
# Define a function to detect outliers using IQR
detect_outliers <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

# Identify outliers in numeric columns
outliers_sales <- detect_outliers(df$sales)
outliers_quantity <- detect_outliers(df$quantity)
outliers_discount <- detect_outliers(df$discount)
outliers_profit <- detect_outliers(df$profit)
outliers_profit_margin <- detect_outliers(df$profit_margin)

# Remove outliers from the dfset
df <- df[!df$sales %in% outliers_sales, ]
df <- df[!df$quantity %in% outliers_quantity, ]
df <- df[!df$discount %in% outliers_discount, ]
df <- df[!df$profit %in% outliers_profit, ]
df <- df[!df$profit_margin %in% outliers_profit_margin, ]

# Check for unique values in categorical columns
# Display unique values of each categorical column
unique_values_ship_mode <- unique(df$ship.mode)
unique_values_segment <- unique(df$segment)
unique_values_region <- unique(df$region)
unique_values_category <- unique(df$category)
unique_values_sub_category <- unique(df$sub.category)

# Address any inconsistent values or df entry errors manually if necessary
# Update the dfset accordingly
print(unique_values_category)
print(unique_values_region)
# Check df integrity after cleaning
names(df)
str(df)
summary(df)
# Check for duplicated rows in the dfset
duplicates <- duplicated(df)

# Count the number of duplicates
num_duplicates <- sum(duplicates)

# Print the number of duplicate rows
print(paste("Number of duplicate rows:", num_duplicates))

# Remove duplicate rows from the dfset
df_unique <- df[!duplicates, ]

# Print the dimensions of the unique dfset
print(paste("Dimensions of unique dfset:", nrow(df_unique), "rows,", ncol(df_unique), "columns"))
names(df)
# Identify duplicate column names
dup_col_names <- names(df)[duplicated(names(df))]

# Print duplicate column names
print(dup_col_names)

# Remove duplicate column names
df <- df[, !duplicated(names(df))]

# Replot the density graph
ggplot(df, aes(x = sales)) +
  geom_density() +
  labs(title = "Density Plot of Sales")


# Convert order date to Date format
df$order.date <- as.Date(df$order.date)

# Plot time series of sales with attractive colors
ggplot(df, aes(x = order.date, y = sales, color = "Sales")) +
  geom_line() +
  labs(title = "Time Series of Sales Over Time",
       x = "Order Date",
       y = "Sales") +
  theme_minimal() +
  scale_color_manual(values = c("Sales" = "#FF5733"))
library(plotly)



# Box plot of sales by category with attractive colors
ggplot(df, aes(x = category, y = sales, fill = category)) +
  geom_boxplot() +
  labs(title = "Box Plot of Sales by Category",
       x = "Category",
       y = "Sales") +
  scale_fill_manual(values = c("#33FF57", "#FF5733", "#33A7FF"))
# Density plot of order processing time with attractive colors
ggplot(df, aes(x = order_processing_time, fill = "Order Processing Time")) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Order Processing Time",
       x = "Order Processing Time (days)",
       y = "Density") +
  scale_fill_manual(values = c("#33FF57")) +
  theme_minimal()
# Stacked bar chart of segment distribution with attractive colors
ggplot(df, aes(x = region, fill = segment)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of Segment Distribution",
       x = "Region",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#33FF57", "#FF5733", "#33A7FF"))
# Plot time series of average sales per day with attractive colors
ggplot(df, aes(x = order.date, y = avg_sales_per_day, color = "Average Sales per Day")) +
  geom_line() +
  labs(title = "Time Series of Average Sales per Day",
       x = "Order Date",
       y = "Average Sales per Day") +
  theme_minimal() +
  scale_color_manual(values = c("Average Sales per Day" = "#33FF57"))
# Select numerical variables for correlation analysis
numerical_vars <- c("sales", "quantity", "discount", "profit", "profit_margin")

# Encode categorical variables (example using one-hot encoding)
df_encoded <- model.matrix(~ . - 1, data = df[, c("ship.mode", "segment", "region", "category", "sub.category")])

# Combine numerical and encoded categorical variables
df_all <- cbind(df[, numerical_vars], df_encoded)

# Calculate the correlation matrix
correlation_matrix_all <- cor(df_all)

# Print correlation matrix
print(correlation_matrix_all)
library(corrplot)
# Plot heatmap
corrplot(correlation_matrix_all, method = "color")
names(df)

# Correlation Analysis
correlation_matrix <- cor(df[, c("sales", "quantity", "discount", "profit")])
print(correlation_matrix)


# Segment Analysis
segment_summary <- df %>%
  group_by(segment) %>%
  summarise(avg_sales = mean(sales), avg_profit = mean(profit))

print(segment_summary)

product_summary <- df %>%
  group_by(category, sub.category) %>%
  summarise(total_sales = sum(sales), total_profit = sum(profit)) %>%
  arrange(desc(total_sales))

print(product_summary)

# Perform PCA
pca_result <- prcomp(df[, c("sales", "quantity", "discount", "profit")], scale. = TRUE)

# Summary of PCA
print(summary(pca_result))

# Scree Plot
screeplot(pca_result, type = "line", main = "Scree Plot")

# Biplot
biplot(pca_result, scale = 0)


# Load required libraries
library(forecast)
library(prophet)



# Preprocess df
# Assuming 'order.date' is the date column
df$order.date <- as.Date(df$order.date)

# Create a time series object
ts_df <- ts(df$sales, start = c(min(df$order_year), 1), frequency = 12)

# Split df into training and test sets
train_size <- 0.8  # 80% training df, 20% test df
train_index <- 1:round(train_size * length(ts_df))
train_df <- ts_df[train_index]
test_df <- ts_df[-train_index]

# ARIMA Model
arima_model <- auto.arima(train_df)
arima_forecast <- forecast(arima_model, h = length(test_df))
arima_accuracy <- accuracy(arima_forecast, test_df)

# Compare models
print("ARIMA Model Accuracy:")
print(arima_accuracy)


# Install and load necessary packages

library(forecast)

# Prepare the data
df$order.date <- as.Date(df$order.date)
df_region <- split(df, df$region)

# Define a function to train ARIMA model and forecast sales for each region
forecast_sales <- function(region_data) {
  # Prepare time series data
  ts_data <- ts(region_data$sales, frequency = 7)
  
  # Train ARIMA model
  arima_model <- auto.arima(ts_data)
  
  # Forecast sales for the next 30 days
  sales_forecast <- forecast(arima_model, h = 30)
  
  # Return the forecasted sales
  return(sales_forecast)
}

# Forecast sales for each region
region_forecasts <- lapply(df_region, forecast_sales)

# Display forecasted sales for each region
for (i in 1:length(region_forecasts)) {
  region <- names(region_forecasts)[i]
  cat("Region:", region, "\n")
  print(region_forecasts[[i]])
}


# Convert 'order.date' and 'ship.date' to Date objects
df$order.date <- as.Date(df$order.date)
df$ship.date <- as.Date(df$ship.date)

# Convert 'order_month' and 'ship_month' to factors
df$order_month <- factor(df$order_month)
df$ship_month <- factor(df$ship_month)

# Convert 'segment', 'country', 'city', 'state', 'region', 'category', 'sub.category' to factors if needed

# Fit a TBATS model
ts_sales <- ts(df$sales, frequency = 7)  # Assuming daily data
tbats_model <- tbats(ts_sales)

# Forecast with TBATS for the next 30 days
forecast_tbats <- forecast(tbats_model, h = 30)

# Plot the TBATS forecast
plot(forecast_tbats, main = "TBATS Sales Forecast", xlab = "Days", ylab = "Sales")

# Step 9: Extracting and Visualizing Seasonal Components with TBATS
tbats_components <- tbats_model$seasonal
print("TBATS Seasonal Components:")
print(tbats_components)

# Step 10: Model Validation
# Calculate MAE for TBATS
actual_values <- ts_sales[(length(ts_sales) - 29):length(ts_sales)]
forecasted_values <- forecast_tbats$mean

tbats_mae <- mean(abs(forecasted_values - actual_values))
print("Mean Absolute Error (MAE) for TBATS:")
print(tbats_mae)

# Plot the TBATS forecast with colorful components
#plot(forecast_tbats, main = "TBATS Sales Forecast", xlab = "Days", ylab = "Sales", col = c("blue", "red", "green"))



# Forecast sales for each region and plot the graph
region_forecasts <- lapply(df_region, forecast_sales)

# Plot forecasted sales for each region
par(mfrow=c(2, 2))  # Set up a 2x2 grid for plotting

for (i in 1:length(region_forecasts)) {
  region <- names(region_forecasts)[i]
  sales_forecast <- region_forecasts[[i]]
  
  plot(sales_forecast, main = paste("Sales Forecast for", region), xlab = "Date", ylab = "Sales")
}


# Prepare the data--splitting the region from the data
df_region <- split(df, df$region)

# Define a function to train TBATS model and forecast sales for each region
forecast_sales_tbats <- function(region_data) {
  # Prepare time series data
  ts_data <- ts(region_data$sales, frequency = 7)
  
  # Train TBATS model
  tbats_model <- tbats(ts_data)
  
  # Forecast sales for the next 30 days
  sales_forecast_tbats <- forecast(tbats_model, h = 30)
  
  # Return the forecasted sales
  return(sales_forecast_tbats)
}

# Forecast sales for each region using TBATS
region_forecasts_tbats <- lapply(df_region, forecast_sales_tbats)

# Extract maximum forecast value across all regions
max_forecast_value <- max(sapply(region_forecasts_tbats, function(x) max(x$mean)))
# Define colors for each region
region_colors <- c("blue", "red", "green", "orange", "purple")  # Add more colors if needed

# Plot forecasted sales for each region with TBATS
plot(region_forecasts_tbats[[1]], main = "TBATS Sales Forecast for Different Regions", xlab = "Days", ylab = "Sales", col = region_colors[1], ylim = c(0, max_forecast_value))

for (i in 2:length(region_forecasts_tbats)) {
  lines(region_forecasts_tbats[[i]]$mean, col = region_colors[i])
}

legend("topright", legend = names(region_forecasts_tbats), col = region_colors[1:length(region_forecasts_tbats)], lty = 1)
