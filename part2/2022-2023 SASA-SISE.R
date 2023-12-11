library(anytime)
library(dplyr)
library(lubridate)

directory_path <- "/Users/ibrahimemrekoksal/Desktop/423 data2"

# Get a list of all CSV files in the directory
csv_files <- list.files(directory_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each CSV file and combine the data
for (file in csv_files) {
  # Read the CSV file
  current_data <- read.csv(file)
  
  # Combine the data
  combined_data <- bind_rows(combined_data, current_data)
}
Training_Data <- combined_data %>% #Data we use to form the control charts
  filter(year(timestamp)==2022)
Test_data <- combined_data %>% #Data we do the simulation on according to the control chart
  filter(year(timestamp) == 2023, quarter(timestamp) %in% c(1,2))

data <- Test_data
data$timestamp <- anytime(data$timestamp)
Training_Data$timestamp <- anytime(Training_Data$timestamp)

##Linear Regression part for the Test Data
unique_stocks <- unique(data$short_name)
# Create an empty list to store data frames for each stock
stock_data_list <- list()
# Loop through each stock and subset the data
for (stock in unique_stocks) {
  stock_data_list[[stock]] <- subset(data, short_name == stock)
}

# Access a specific stock's data using stock_data_list$stockname
SASA_data <- stock_data_list[["SASA"]]
sise_data <- stock_data_list[["SISE"]]
merged_data <- merge(SASA_data, sise_data, by = "timestamp")
regression_model <- lm(price.x ~ price.y, data = merged_data)

##Linear Regression part for the Training Data

unique_stocks_Training <- unique(Training_Data$short_name)
# Create an empty list to store data frames for each stock
stock_data_list <- list()
# Loop through each stock and subset the data
for (stock in unique_stocks_Training) {
  stock_data_list[[stock]] <- subset(Training_Data, short_name == stock)
}

# Access a specific stock's data using stock_data_list$stockname
SASA_data_training <- stock_data_list[["SASA"]]
sise_data_training <- stock_data_list[["SISE"]]
merged_data_training <- merge(SASA_data_training, sise_data_training, by = "timestamp")
regression_model_training <- lm(price.x ~ price.y, data = merged_data_training)
residuals_training <- residuals(regression_model_training)

# Create a data frame with timestamp and residuals
residual_data_training <- data.frame(timestamp = merged_data_training$timestamp, residuals = residuals_training)
######
residual_data_training$timestamp <- as.Date(residual_data_training$timestamp)

# Get the names of stocks
stock_names <- names(stock_data_list)

# Create an empty data frame to store correlation results
correlation_results <- data.frame()

# Iterate through all pairs of stocks and calculate correlation
for (i in 1:(length(stock_names)-1)) {
  for (j in (i+1):length(stock_names)) {
    stock1 <- stock_data_list[[stock_names[i]]]
    stock2 <- stock_data_list[[stock_names[j]]]
    
    # Merge data on the 'timestamp' column
    mrgd_data <- merge(stock1, stock2, by = "timestamp")
    
    # Calculate correlation
    correlation <- cor(mrgd_data$price.x, mrgd_data$price.y)
    
    # Store the results in the data frame
    correlation_results <- rbind(correlation_results, data.frame(
      Stock1 = stock_names[i],
      Stock2 = stock_names[j],
      Correlation = correlation
    ))
  }
}

# Filter pairs with strong correlation (you can adjust the threshold)
strong_correlations <- correlation_results[order(-correlation_results$Correlation), ]

# Print the pairs with strong correlation
print(strong_correlations)

# Create a data frame with timestamp and residuals
residuals <- residuals(regression_model)
residual_data <- data.frame(timestamp = merged_data$timestamp, residuals = residuals)

# Print the residual data
print(residual_data)
residual_data$timestamp <- as.Date(residual_data$timestamp)


library(ggplot2)

# Calculate control limits 
upper_limit <- mean(residual_data_training$residuals) + 2 * sd(residual_data_training$residuals)
lower_limit <- mean(residual_data_training$residuals) - 2 * sd(residual_data_training$residuals)

ggplot(residual_data, aes(x = timestamp, y = residuals)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = upper_limit, linetype = "solid", color = "red") +
  geom_hline(yintercept = lower_limit, linetype = "solid", color = "red") +
  ggtitle("X-bar Chart of Residuals") +
  xlab("Date") +
  ylab("Daily Average Residuals")


# Highlight points outside control limits
out_of_control <- subset(residual_data, residuals > upper_limit | residuals < lower_limit)
print(out_of_control)


# Create a column in merged_data for residuals
merged_data$residual <- residuals(regression_model)

# Signal Generation
merged_data$signal <- ifelse(merged_data$residual > upper_limit, -1, 
                             ifelse(merged_data$residual < lower_limit, 1, 0))


# Initialization
long_position_SASA<- FALSE
short_position_SASA<- FALSE
long_position_SISE <- FALSE
short_position_SISE <- FALSE
total_earnings <- 0
row_number <- nrow(merged_data)
# Variables to store entry dates
entry_date_SASA <- NA
entry_date_SISE <- NA

# Loop through each timestamp
for (i in 2:nrow(merged_data)) {
  
  # Check for signal
  if (merged_data$signal[i - 1] == 1 && !long_position_SASA && !short_position_SASA) {
    # Open long position in SASA and short position in SISE
    long_position_SASA <- TRUE
    short_position_SISE <- TRUE
    entry_price_SASA <- merged_data$price.x[i]
    entry_price_SISE <- merged_data$price.y[i]
    entry_date_SASA <- merged_data$timestamp[i]
    entry_date_SISE <- merged_data$timestamp[i]
    print(paste("Opened long position in SASA on", entry_date_SASA))
    print(paste("Opened short position in SISE on", entry_date_SISE))
  } else if (merged_data$signal[i - 1] == -1 && !long_position_SASA && !short_position_SASA) {
    # Open long position in SISE and short position in SASA
    long_position_SISE <- TRUE
    short_position_SASA <- TRUE
    entry_price_SASA <- merged_data$price.x[i]
    entry_price_SISE <- merged_data$price.y[i]
    entry_date_SASA <- merged_data$timestamp[i]
    entry_date_SISE <- merged_data$timestamp[i]
    print(paste("Opened long position in SISE on", entry_date_SISE))
    print(paste("Opened short position in SASA on", entry_date_SASA))
  }
  
  if (long_position_SASA) {
    # Close long SASA position if residual reverts to zero
    if (abs(merged_data$residual[i]) < sd(residual_data_training$residuals)) {
      long_position_SASA <- FALSE
      earning_percentage <- (merged_data$price.x[i] - entry_price_SASA) / entry_price_SASA
      total_earnings <- total_earnings + earning_percentage
      print(paste("Closed long position in SASA on", merged_data$timestamp[i], "with earnings:", earning_percentage))
    }
  }
  
  # Check for closing short SASA position
  if (short_position_SASA) {
    # Close short SASA position if residual reverts to zero
    if (abs(merged_data$residual[i]) < sd(residual_data_training$residuals)) {
      short_position_SASA <- FALSE
      earning_percentage <- (entry_price_SASA - merged_data$price.x[i]) / entry_price_SASA
      total_earnings <- total_earnings + earning_percentage
      print(paste("Closed short position in SASA on", merged_data$timestamp[i], "with earnings:", earning_percentage))
    }
  }
  
  # Check for closing long SISE position
  if (long_position_SISE) {
    # Close long SISE position if residual reverts to zero
    if (abs(merged_data$residual[i]) < sd(residual_data_training$residuals)) {
      long_position_SISE <- FALSE
      earning_percentage <- (merged_data$price.y[i] - entry_price_SISE) / entry_price_SISE
      total_earnings <- total_earnings + earning_percentage
      print(paste("Closed long position in SISE on", merged_data$timestamp[i], "with earnings:", earning_percentage))
    }
  }
  
  # Check for closing short SISE position
  if (short_position_SISE) {
    # Close short SISE position if residual reverts to zero
    if (abs(merged_data$residual[i]) < sd(residual_data_training$residuals)) {
      short_position_SISE <- FALSE
      earning_percentage <- (entry_price_SISE - merged_data$price.y[i]) / entry_price_SISE
      total_earnings <- total_earnings + earning_percentage
      print(paste("Closed short position in SISE on", merged_data$timestamp[i], "with earnings:", earning_percentage))
    }
  }
  }


# Print total earnings
print(paste("Total Earnings:", total_earnings))


########### Task2

library(forecast)

firstHour <- 24*(as.Date("2022-01-02 18:00:00")-as.Date("2022-01-02 10:00:00"))
ts_residuals_training <- ts(residuals_training,start=c(2022,firstHour),frequency=5*10*52)
plot(ts_residuals_training, main = "Residuals over Time", ylab = "Residual Value", xlab = "Time")

# Use auto.arima to automatically select the best ARIMA model
auto_arima_model <- auto.arima(ts_residuals_training)

# Print the summary of the selected model
summary(auto_arima_model)
tsdisplay(auto_arima_model$residuals) 

# Create an X-bar chart using ggplot2
library(ggplot2)

# Calculate control limits (you can customize these based on your requirements)
upper_limit_ts <- mean(auto_arima_model$residuals) + 2 * sd(auto_arima_model$residuals)
lower_limit_ts <- mean(auto_arima_model$residuals) - 2 * sd(auto_arima_model$residuals)

# X-bar chart
# According to the predefined 'upper_limit' and 'lower_limit'
ggplot(residual_data, aes(x = timestamp, y = residuals)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = upper_limit_ts, linetype = "solid", color = "red") +
  geom_hline(yintercept = lower_limit_ts, linetype = "solid", color = "red") +
  ggtitle("X-bar Chart of Residuals") +
  xlab("Date") +
  ylab("Daily Average Residuals")


# Highlight points outside control limits
out_of_control <- subset(residual_data, residuals > upper_limit_ts | residuals < lower_limit_ts)
print(out_of_control)

# Create a column in merged_data for residuals
merged_data$residual <- residuals(regression_model)

# Signal Generation
merged_data$signal <- ifelse(merged_data$residual > upper_limit_ts, -1, 
                             ifelse(merged_data$residual < lower_limit_ts, 1, 0))


# Initialization
long_position_SASA<- FALSE
short_position_SASA<- FALSE
long_position_SISE <- FALSE
short_position_SISE <- FALSE
total_earnings <- 0
row_number <- nrow(merged_data)
# Variables to store entry dates
entry_date_SASA <- NA
entry_date_SISE <- NA

# Loop through each timestamp
for (i in 2:nrow(merged_data)) {
  
  # Check for signal
  if (merged_data$signal[i - 1] == 1 && !long_position_SASA && !short_position_SASA) {
    # Open long position in SASA and short position in SISE
    long_position_SASA <- TRUE
    short_position_SISE <- TRUE
    entry_price_SASA <- merged_data$price.x[i]
    entry_price_SISE <- merged_data$price.y[i]
    entry_date_SASA <- merged_data$timestamp[i]
    entry_date_SISE <- merged_data$timestamp[i]
    print(paste("Opened long position in SASA on", entry_date_SASA))
    print(paste("Opened short position in SISE on", entry_date_SISE))
  } else if (merged_data$signal[i - 1] == -1 && !long_position_SASA && !short_position_SASA) {
    # Open long position in SISE and short position in SASA
    long_position_SISE <- TRUE
    short_position_SASA <- TRUE
    entry_price_SASA <- merged_data$price.x[i]
    entry_price_SISE <- merged_data$price.y[i]
    entry_date_SASA <- merged_data$timestamp[i]
    entry_date_SISE <- merged_data$timestamp[i]
    print(paste("Opened long position in SISE on", entry_date_SISE))
    print(paste("Opened short position in SASA on", entry_date_SASA))
  }
  
  if (long_position_SASA) {
    # Close long SASA position if residual reverts to zero
    if (abs(merged_data$residual[i]) < 0.1) {
      long_position_SASA <- FALSE
      earning_percentage <- (merged_data$price.x[i] - entry_price_SASA) / entry_price_SASA
      total_earnings <- total_earnings + earning_percentage
      print(paste("Closed long position in SASA on", merged_data$timestamp[i], "with earnings:", earning_percentage))
    }
  }
  
  # Check for closing short SASA position
  if (short_position_SASA) {
    # Close short SASA position if residual reverts to zero
    if (abs(merged_data$residual[i]) < 0.1) {
      short_position_SASA <- FALSE
      earning_percentage <- (entry_price_SASA - merged_data$price.x[i]) / entry_price_SASA
      total_earnings <- total_earnings + earning_percentage
      print(paste("Closed short position in SASA on", merged_data$timestamp[i], "with earnings:", earning_percentage))
    }
  }
  
  # Check for closing long SISE position
  if (long_position_SISE) {
    # Close long SISE position if residual reverts to zero
    if (abs(merged_data$residual[i]) < 0.1) {
      long_position_SISE <- FALSE
      earning_percentage <- (merged_data$price.y[i] - entry_price_SISE) / entry_price_SISE
      total_earnings <- total_earnings + earning_percentage
      print(paste("Closed long position in SISE on", merged_data$timestamp[i], "with earnings:", earning_percentage))
    }
  }
  
  # Check for closing short SISE position
  if (short_position_SISE) {
    # Close short SISE position if residual reverts to zero
    if (abs(merged_data$residual[i]) < 0.1) {
      short_position_SISE <- FALSE
      earning_percentage <- (entry_price_SISE - merged_data$price.y[i]) / entry_price_SISE
      total_earnings <- total_earnings + earning_percentage
      print(paste("Closed short position in SISE on", merged_data$timestamp[i], "with earnings:", earning_percentage))
    }
  }
}


# Print total earnings
print(paste("Total Earnings:", total_earnings))






