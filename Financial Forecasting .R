library(readxl)

# Read Excel data
data <- read_excel("ExchangeUSD.xlsx")

data

# Check for missing values
check_for_missing_values <- any(is.na(data))
print(paste("Missing values check:", check_for_missing_values))
data <- na.omit(data) 
data<- data[,3]
summary(data)

data<-as.data.frame(data)
data

#checking whether there are missing values in the 3rd column
missing_data_in_column_three <- sum(is.na(data))
missing_data_in_column_three

boxplot(data,col="green")
# Function to remove outliers using IQR and replace with NA if necessary
remove_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  x[x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)] <- NA
  return(x)
}

# Identify numeric columns
numeric_columns <- sapply(data, is.numeric)

# Apply the function to numeric columns
data[numeric_columns] <- lapply(data[numeric_columns], remove_outliers)

# Handle NA values by removing rows with NA
data <- na.omit(data)

# Check for missing values after outlier removal and NA handling
check_for_missing_values_after_removal <- any(is.na(data))
print(paste("Missing values check after outlier removal and NA handling:", check_for_missing_values_after_removal))

# Summarize the data after outlier removal and NA handling
summary(data)
boxplot(data,col="red")
colnames(data)
colnames(data) <- c('ActualExchange')
colnames(data)
library(dplyr)

# see here how we can create 4 columns in this I/O matrix. Check the time-delayed variables!
time_lagged_data <- bind_cols(lag_1 = lag(data,1),
                              lag_2 = lag(data,2),
                              lag_3 = lag(data,3),
                              lag_4 = lag(data,4),
                              lag = data)
# see here the existence of NA values due to that shifting
summary(time_lagged_data)
time_lagged_data 
colnames(time_lagged_data)
colnames(time_lagged_data) <- c('Exchange1','Exchange2','Exchange3','Exchange4','Exchange')

#checking for NA values
sum(is.na(time_lagged_data))

#removing NA values
time_lagged_data <- na.omit(time_lagged_data)
colnames(time_lagged_data)

#Again checking for NA values
sum(is.na(time_lagged_data))

boxplot(time_lagged_data,col="blue")

#Normalize using minmax 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalize_time_lagged_data<-normalize(time_lagged_data)
normalize_time_lagged_data
summary(normalize_time_lagged_data)



#spliting data (train, test) 
train_data <- normalize_time_lagged_data[1:400, ] 
test_data <- normalize_time_lagged_data[401:nrow(normalize_time_lagged_data), ] 

train_data
test_data

colnames(train_data)
colnames(test_data)

summary(train_data)
summary(test_data)

#rows in the train_data 
rows_train <- nrow(train_data)


print(paste("Number of rows in train_data:", rows_train))

#rows in the test_data
rows_test <- nrow(test_data)


print(paste("Number of rows in test_data:", rows_test))

#training the model
library(neuralnet)
library(grid)
library(MASS)
colnames(train_data)
colnames(test_data)

set.seed(123) 
#model 1
exchangeRates_model_1 <- neuralnet(formula = Exchange ~ Exchange1, 
                            data = train_data, 
                            hidden = c(5), 
                            linear.output = TRUE)


plot(exchangeRates_model_1)

#predictions
train_pred <- predict(exchangeRates_model_1, train_data[, -5])  
test_pred <- predict(exchangeRates_model_1, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 1
RMSE_train_1 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_1 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_1 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_1 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_1 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_1, MAE_train_1, MAPE_train_1, SMAPE_train_1) 
) 
print(results_model_train_1) 



# Testing set for model 1
RMSE_test_1 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_1 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_1 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_1 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_1 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_1, MAE_test_1, MAPE_test_1, SMAPE_test_1) 
) 
print(results_model_test_1) 




# For model 1
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 1
print(head(test_actual_denorm))
print(head(test_prediction_denorm))

# Determine the minimum length of the two vectors
min_length <- min(length(test_actual_denorm), length(test_prediction_denorm))

# Plot only the common elements
plot(test_actual_denorm[1:min_length], test_prediction_denorm[1:min_length], 
     col = 'green', main = 'Actual vs. Predicted Exchange Rates (USD/EUR)', 
     xlab = 'Actual Exchange Rate', ylab = 'Predicted Exchange Rate',
     pch = 18, cex = 0.7)
abline(a = 0, b = 1, col = 'blue')  






#model2
exchangeRates_model_2 <- neuralnet(formula = Exchange ~ Exchange1, 
                                   data = train_data, 
                                   hidden = c(5,4), 
                                   linear.output = TRUE)


plot(exchangeRates_model_2)

#predictions
train_pred <- predict(exchangeRates_model_2, train_data[, -5])  
test_pred <- predict(exchangeRates_model_2, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 2
RMSE_train_2 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_2 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_2 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_2 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_2 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_2, MAE_train_2, MAPE_train_2, SMAPE_train_2) 
) 
print(results_model_train_2) 



# Testing set for model 2
RMSE_test_2 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_2 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_2 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_2 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_2 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_2, MAE_test_2, MAPE_test_2, SMAPE_test_2) 
) 
print(results_model_test_2) 




# For model 2
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 2
print(head(test_actual_denorm))
print(head(test_prediction_denorm))


#model3
exchangeRates_model_3 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2, 
                                   data = train_data, 
                                   hidden = c(5,5), 
                                   linear.output = TRUE)


plot(exchangeRates_model_3)

#predictions
train_pred <- predict(exchangeRates_model_3, train_data[, -5])  
test_pred <- predict(exchangeRates_model_3, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 3
RMSE_train_3 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_3 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_3 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_3 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_3 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_3, MAE_train_3, MAPE_train_3, SMAPE_train_3) 
) 
print(results_model_train_3) 



# Testing set for model 3
RMSE_test_3 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_3 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_3 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_3 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_3 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_3, MAE_test_3, MAPE_test_3, SMAPE_test_3) 
) 
print(results_model_test_3) 




# For model 3
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 3
print(head(test_actual_denorm))
print(head(test_prediction_denorm))


#model4
exchangeRates_model_4 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2, 
                                   data = train_data, 
                                   hidden = c(4), 
                                   linear.output = TRUE)


plot(exchangeRates_model_4)

#predictions
train_pred <- predict(exchangeRates_model_4, train_data[, -5])  
test_pred <- predict(exchangeRates_model_4, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 4
RMSE_train_4 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_4 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_4 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_4 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_4 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_4, MAE_train_4, MAPE_train_4, SMAPE_train_4) 
) 
print(results_model_train_4) 



# Testing set for model 4
RMSE_test_4 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_4 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_4 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_4 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_4 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_4, MAE_test_4, MAPE_test_4, SMAPE_test_4) 
) 
print(results_model_test_4) 




# For model 4
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 4
print(head(test_actual_denorm))
print(head(test_prediction_denorm))

#model5
exchangeRates_model_5 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2+Exchange3, 
                                   data = train_data, 
                                   hidden = c(8,10), 
                                   linear.output = TRUE)


plot(exchangeRates_model_5)

#predictions
train_pred <- predict(exchangeRates_model_5, train_data[, -5])  
test_pred <- predict(exchangeRates_model_5, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 5
RMSE_train_5 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_5 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_5 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_5 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_5 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_5, MAE_train_5, MAPE_train_5, SMAPE_train_5) 
) 
print(results_model_train_5) 



# Testing set for model 5
RMSE_test_5 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_5 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_5 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_5 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_5 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_5, MAE_test_5, MAPE_test_5, SMAPE_test_5) 
) 
print(results_model_test_5) 




# For model 5
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 5
print(head(test_actual_denorm))
print(head(test_prediction_denorm))


#model6
exchangeRates_model_6 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2+Exchange3, 
                                   data = train_data, 
                                   hidden = c(8), 
                                   linear.output = TRUE)


plot(exchangeRates_model_6)

#predictions
train_pred <- predict(exchangeRates_model_6, train_data[, -5])  
test_pred <- predict(exchangeRates_model_6, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 6
RMSE_train_6 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_6 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_6 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_6 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_6 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_6, MAE_train_6, MAPE_train_6, SMAPE_train_6) 
) 
print(results_model_train_6) 



# Testing set for model 6
RMSE_test_6 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_6 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_6 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_6 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_6 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_6, MAE_test_6, MAPE_test_6, SMAPE_test_6) 
) 
print(results_model_test_6) 




# For model 6
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 6
print(head(test_actual_denorm))
print(head(test_prediction_denorm))


#model7
exchangeRates_model_7 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2+Exchange3, 
                                   data = train_data, 
                                   hidden = c(5,10), 
                                   linear.output = TRUE)


plot(exchangeRates_model_7)

#predictions
train_pred <- predict(exchangeRates_model_7, train_data[, -5])  
test_pred <- predict(exchangeRates_model_7, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 7
RMSE_train_7 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_7 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_7 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_7 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_7 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_7, MAE_train_7, MAPE_train_7, SMAPE_train_7) 
) 
print(results_model_train_7) 



# Testing set for model 7
RMSE_test_7 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_7 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_7 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_7 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_7 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_7, MAE_test_7, MAPE_test_7, SMAPE_test_7) 
) 
print(results_model_test_7) 




# For model 7
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 7
print(head(test_actual_denorm))
print(head(test_prediction_denorm))


#model8
exchangeRates_model_8 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2+Exchange3, 
                                   data = train_data, 
                                   hidden = c(6,8), 
                                   linear.output = TRUE)


plot(exchangeRates_model_8)

#predictions
train_pred <- predict(exchangeRates_model_8, train_data[, -5])  
test_pred <- predict(exchangeRates_model_8, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 8
RMSE_train_8 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_8 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_8 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_8 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_8 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_8, MAE_train_8, MAPE_train_8, SMAPE_train_8) 
) 
print(results_model_train_8) 



# Testing set for model 8
RMSE_test_8 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_8 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_8 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_8 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_8 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_8, MAE_test_8, MAPE_test_8, SMAPE_test_8) 
) 
print(results_model_test_8) 




# For model 8
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 8
print(head(test_actual_denorm))
print(head(test_prediction_denorm))



#model9
exchangeRates_model_9 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2+Exchange3+Exchange4, 
                                   data = train_data, 
                                   hidden = c(6), 
                                   linear.output = TRUE)


plot(exchangeRates_model_9)

#predictions
train_pred <- predict(exchangeRates_model_9, train_data[, -5])  
test_pred <- predict(exchangeRates_model_9, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 9
RMSE_train_9 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_9 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_9 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_9 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_9 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_9, MAE_train_9, MAPE_train_9, SMAPE_train_9) 
) 
print(results_model_train_9) 



# Testing set for model 9
RMSE_test_9 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_9 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_9 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_9 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_9 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_9, MAE_test_9, MAPE_test_9, SMAPE_test_9) 
) 
print(results_model_test_9) 




# For model 9
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 9
print(head(test_actual_denorm))
print(head(test_prediction_denorm))


#model10
exchangeRates_model_10 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2+Exchange3+Exchange4, 
                                   data = train_data, 
                                   hidden = c(6,8), 
                                   linear.output = TRUE)


plot(exchangeRates_model_10)

#predictions
train_pred <- predict(exchangeRates_model_10, train_data[, -5])  
test_pred <- predict(exchangeRates_model_10, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 10
RMSE_train_10 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_10 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_10 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_10 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_10 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_10, MAE_train_10, MAPE_train_10, SMAPE_train_10) 
) 
print(results_model_train_10) 



# Testing set for model 10
RMSE_test_10 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_10 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_10 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_10 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_10 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_10, MAE_test_10, MAPE_test_10, SMAPE_test_10) 
) 
print(results_model_test_10) 




# For model 10
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 10
print(head(test_actual_denorm))
print(head(test_prediction_denorm))

#model11
exchangeRates_model_11 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2+Exchange3+Exchange4, 
                                    data = train_data, 
                                    hidden = c(8), 
                                    linear.output = TRUE)

plot(exchangeRates_model_11)

#predictions
train_pred <- predict(exchangeRates_model_11, train_data[, -5])  
test_pred <- predict(exchangeRates_model_11, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 11
RMSE_train_11 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_11 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_11 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_11 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_11 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_11, MAE_train_11, MAPE_train_11, SMAPE_train_11) 
) 
print(results_model_train_11) 



# Testing set for model 11
RMSE_test_11 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_11 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_11 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_11 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_11 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_11, MAE_test_11, MAPE_test_11, SMAPE_test_11) 
) 
print(results_model_test_11) 




# For model 11
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 11
print(head(test_actual_denorm))
print(head(test_prediction_denorm))

#model12
exchangeRates_model_12 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2+Exchange3+Exchange4, 
                                    data = train_data, 
                                    hidden = c(8,10), 
                                    linear.output = TRUE)


plot(exchangeRates_model_12)

#predictions
train_pred <- predict(exchangeRates_model_12, train_data[, -5])  
test_pred <- predict(exchangeRates_model_12, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 12
RMSE_train_12 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_12 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_12 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_12 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_12 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_12, MAE_train_12, MAPE_train_12, SMAPE_train_12) 
) 
print(results_model_train_12) 



# Testing set for model 12
RMSE_test_12 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_12 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_12 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_12 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_12 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_12, MAE_test_12, MAPE_test_12, SMAPE_test_12) 
) 
print(results_model_test_12) 




# For model 12
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 12
print(head(test_actual_denorm))
print(head(test_prediction_denorm))


#model13
exchangeRates_model_13 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2+Exchange3+Exchange4, 
                                    data = train_data, 
                                    hidden = c(10), 
                                    linear.output = TRUE)


plot(exchangeRates_model_13)

#predictions
train_pred <- predict(exchangeRates_model_13, train_data[, -5])  
test_pred <- predict(exchangeRates_model_13, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 13
RMSE_train_13 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_13 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_13 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_13 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_13 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_13, MAE_train_13, MAPE_train_13, SMAPE_train_13) 
) 
print(results_model_train_13) 



# Testing set for model 13
RMSE_test_13 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_13 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_13 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_13 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_13 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_13, MAE_test_13, MAPE_test_13, SMAPE_test_13) 
) 
print(results_model_test_13) 




# For model 13
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 13
print(head(test_actual_denorm))
print(head(test_prediction_denorm))



#model14
exchangeRates_model_14 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2+Exchange3+Exchange4, 
                                    data = train_data, 
                                    hidden = c(10,5), 
                                    linear.output = TRUE)


plot(exchangeRates_model_14)

#predictions
train_pred <- predict(exchangeRates_model_14, train_data[, -5])  
test_pred <- predict(exchangeRates_model_14, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 14
RMSE_train_14 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_14 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_14 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_14 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_14 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_14, MAE_train_14, MAPE_train_14, SMAPE_train_14) 
) 
print(results_model_train_14) 



# Testing set for model 14
RMSE_test_14 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_14 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_14 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_14 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_14 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_14, MAE_test_14, MAPE_test_14, SMAPE_test_14) 
) 
print(results_model_test_14) 




# For model 14
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 14
print(head(test_actual_denorm))
print(head(test_prediction_denorm))

# Determine the minimum length of the two vectors
min_length <- min(length(test_actual_denorm), length(test_prediction_denorm))

# Plot only the common elements
plot(test_actual_denorm[1:min_length], test_prediction_denorm[1:min_length], 
     col = 'green', main = 'Actual vs. Predicted Exchange Rates (USD/EUR)', 
     xlab = 'Actual Exchange Rate', ylab = 'Predicted Exchange Rate',
     pch = 18, cex = 0.7)
abline(a = 0, b = 1, col = 'blue')  

#model15
exchangeRates_model_15 <- neuralnet(formula = Exchange ~ Exchange1+Exchange2+Exchange3+Exchange4, 
                                    data = train_data, 
                                    hidden = c(8,6), 
                                    linear.output = TRUE)


plot(exchangeRates_model_15)

#predictions
train_pred <- predict(exchangeRates_model_15, train_data[, -5])  
test_pred <- predict(exchangeRates_model_15, test_data[, -5])    
# denormalizing function
denormalize <- function(x, min, max) { 
  return((max - min) * x + min)
}

# Min and Max for denormalizing
denormalize_min <- min(data, na.rm = TRUE)
denormalize_max <- max(data, na.rm = TRUE)

# denormalizing the values
train_prediction_denorm <- denormalize(train_pred, denormalize_min, denormalize_max)
test_prediction_denorm <- denormalize(test_pred, denormalize_min, denormalize_max)
train_actual_denorm <- denormalize(train_data$Exchange, denormalize_min, denormalize_max)
test_actual_denorm <- denormalize(test_data$Exchange, denormalize_min, denormalize_max)



# RMSE calculation function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# MAE calculation function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# MAPE calculation function
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# sMAPE calculation function
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
}


#Training set for model 15
RMSE_train_15 <- rmse(train_actual_denorm, train_prediction_denorm)
MAE_train_15 <- mae(train_actual_denorm, train_prediction_denorm)
MAPE_train_15 <- mape(train_actual_denorm, train_prediction_denorm)
SMAPE_train_15 <- smape(train_actual_denorm, train_prediction_denorm)


results_model_train_15 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_train_15, MAE_train_15, MAPE_train_15, SMAPE_train_15) 
) 
print(results_model_train_15) 



# Testing set for model 15
RMSE_test_15 <- rmse(test_actual_denorm, test_prediction_denorm)
MAE_test_15 <- mae(test_actual_denorm, test_prediction_denorm)
MAPE_test_15 <- mape(test_actual_denorm, test_prediction_denorm)
SMAPE_test_15 <- smape(test_actual_denorm, test_prediction_denorm)

results_model_test_15 <- data.frame( 
  Metric = c("RMSE", "MAE", "MAPE", "sMAPE"), 
  Value = c(RMSE_test_15, MAE_test_15, MAPE_test_15, SMAPE_test_15) 
) 
print(results_model_test_15) 




# For model 15
print(head(train_actual_denorm))
print(head(train_prediction_denorm))

# For model 15
print(head(test_actual_denorm))
print(head(test_prediction_denorm))








