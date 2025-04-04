#Installing Packages
install.packages("quantmod")
install.packages("mltools")
install.packages("tseries")
install.packages("forecast")
install.packages("lubridate")
install.packages("sjPlot")

#Loading Libraries
library(ggplot2)
library(quantmod)
library(tidyverse)
library(mltools)
library(tseries)
library(forecast)
library(lubridate)
library(zoo)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(dplyr)
library(tidyr)

#DATA PREPARATION

# Importing the dataset
#appl_data <- read.csv("C:\\Users\\Vijay\\Downloads\\AAPL.csv")
appl_data <- read.csv("C:\\Users\\Vijay\\Downloads\\archive\\AAPL.csv")

#DATA PREPROCESSING & EXPLORATORY DATA ANALYSIS

# Display the first few rows of the data frame
head(appl_data) 

# Display the last few rows of the data frame
tail(appl_data) 

# Statistical summary of the data
summary(appl_data)

#Structure of the data
str(appl_data)

# Display the structure of the data frame
glimpse(appl_data)

#Display the dimensions of the data frame
dim(appl_data)

# Check for missing values in each column
missing_values <- colSums(is.na(appl_data))
print(missing_values)

# Check for duplicate rows
duplicate_count <- sum(duplicated(appl_data))
print(duplicate_count)

# Drop duplicate rows
appl_data <- appl_data %>% distinct()
ls()
rm(class)

# Check the data types of the columns
data_types <- sapply(appl_data, class)
print(data_types)

# Convert 'Date' column to Date format
appl_data$Date <- as.Date(appl_data$Date)

appl_data['Date'] <- as.Date(appl_data$Date, "%d-%b-%y")
str(appl_data)

#MISSING VALUES CHECK
colSums(is.na(appl_data))

#SUBSETTING THE DATA

# Now let us consider the tech era of the dates and select the required rows
appl_tech_era <- appl_data[appl_data$Date >= '2012-01-17',]
View(appl_tech_era)

# Finding the standard deviation of the closing price of the apple stock price
sd(appl_tech_era$Close)

#Finding the range of the closing price of the apple stock price
range(appl_tech_era$Close)

#OUTLIERS CHECK

# Checking for the presence of outliers
par(mfrow = c(2,3), oma = c(1,1,0,0) + 0.1, mar = c(1,1,1,1) + 0.1)
boxplot(appl_tech_era$Open,main = "Open")
boxplot(appl_tech_era$High,main = "High")
boxplot(appl_tech_era$Low,main = "Low")
boxplot(appl_tech_era$Close,main = "Close")
boxplot(appl_tech_era$Volume,main = "Volume")

#DATA VISUALIZATION

# Checking for the normal distribution of data
hist(appl_tech_era$Close, main = "Apple Stock Closing Price in Tech Era(2012-2022)",
     xlab = "Closing Price", xlim = c(min(appl_tech_era$Close) - 1 ,max(appl_tech_era$Close) + 1), 
     col = "#90A4AE", border = "#263238", breaks = 50)

#QQ Plot (Quantile-Quantile plot)
ggplot(data = appl_tech_era, aes(sample = Close)) + stat_qq() + 
  stat_qq_line(col = '#90A4AE') + 
  ggtitle('QQ plot of Apple Stock Closing Price')

# Apple Stock Closing Price Visualization Across Years
ggplot(data = appl_tech_era, aes(x = Date, y = Close)) +
  geom_line(col = "#455A64") +
  labs(title = "Apple Stock Price Visualization Across Years",
       subtitle = "2012-2022", x = "Time Frame", y = "Closing Price ($)")


#LINEAR REGRESSION MODEL BUILDING: Using lm and glm (generalized linear model),fitting the latter with a log function. These models are used as the stock prices grow and is evaluated as an exponential function.

# Using lm and glm Models

ggplot(data = appl_tech_era, aes(x = Date, y = Close)) + 
  geom_line(col = "#455A64") + geom_smooth(method = "lm", color = "#90A4AE", lwd = 1, linetype = 'dashed') + 
  geom_smooth(method = "glm", method.args = list(family = gaussian(link = "log")), color = "7986CB")


#FITTING THE LINEAR REGRESSION

# Testing and Training the data for 75:25

pm <- 1:(nrow(appl_tech_era)*0.75)
train_data <- appl_tech_era[pm,]
test_data <- appl_tech_era[-pm,]
asp.lm <- lm(Close ~ Date, data = train_data)
summary(asp.lm)

# Finding the RMSE Value

predicted_closing_price <- predict(asp.lm, newdata = test_data[c('Date')])
pred_test_data <- test_data
pred_test_data['predicted_closing_price'] <- predicted_closing_price
rmse_asp <- rmse(actuals = pred_test_data$Close, pred = pred_test_data$predicted_closing_price)
print(rmse_asp)

# Plotting the Final Predicted Linear Regression Model

ggplot(data = train_data, aes(x = Date, y = Close)) + 
  geom_line(colour = "#90A4AE", show.legend = TRUE) + 
  geom_line(data = test_data, aes(x = Date, y = Close),colour = "#66BB6A") + 
  geom_line(data = pred_test_data, aes(x = Date, y = predicted_closing_price),colour = "#BF360C") + 
  labs(title = "Predicted Apple Stock Price Visualization Across Years", subtitle = "2012-2022", x = "Time Frame", y = "Closing Price ($)")


#ARIMA MODEL BUILDING

#Finding the mean and standard deviation of rolling 6-month average Statistics for the past ten years

appl_rollstat <- appl_tech_era %>% mutate(mean_close = zoo::rollmean(Close, k = 180, fill = '#78909C'), 
                                          sd_close = zoo::rollapply(Close, FUN = sd, width = 180, fill = '#37474F'))
appl_rollstat

#MEAN OF THE APPLE STOCK

appl_rollmean <- appl_rollstat %>% ggplot() + 
  geom_line(aes(Date, mean_close)) + theme_bw() +  
  ggtitle("Apple Stock Mean Over the Past Ten-Year Period (Six Month Rolling Window)") + 
  ylab("Closing Price") + xlab("Date")

# Finding the mean of the Apple Stock
appl_rollmean

#STANDARD DEVIATION OF THE APPLE STOCK
appl_rollsd <- appl_rollstat %>% ggplot() + 
  geom_line(aes(Date, sd_close)) + theme_bw() + 
  ggtitle("Apple Stock Standard Deviation Over the Past Ten-Year Period (Six Month Rolling Window)") + 
  ylab("Closing Price") +  xlab("Date")

# Finding the Standard Deviation of the Apple Stock
appl_rollsd

##From the above statistics we can infer that this time series has both mean non-stationary and variance non-stationary


#AUGMENTED DICKEY FULLER TEST FOR CHECKING THE STATIONARITY
print(adf.test(appl_tech_era$Close))

##From the above ADF test we can infer that the p-value is 0.99 which is greater than 0.05. So, we fail to reject the Null Hypothesis and can conclude that this time series is non-stationary.


#KPSS TEST FOR CHECKING THE STATIONARITY
kpss.test(appl_tech_era$Close, null = "Trend")

##From the above KPSS test we can infer that the p-value is 0.01 which is lesser than 0.05. So, we can reject the Null Hypothesis and can conclude that this time series is non-stationary.

##Transform the time-series into variance stationary using the below two techniques

#LOG TRANSFORMATIONS FOR VARIANCE

appl_techlog = appl_tech_era %>% mutate(closing_log = log1p(Close))
appl_techlog %>% ggplot() + geom_line(aes(Date, closing_log)) + theme_bw() + 
  ggtitle("Apple Stock over Time (Log)") + 
  ylab("Closing Price (Log)") + xlab("Date")



#BOX-COX TRANSFORMATION

appl_boxcox = appl_tech_era %>% mutate(close_boxcox = forecast::BoxCox(Close,lambda = 'auto'))

appl_boxcox %>% ggplot() + geom_line(aes(Date, close_boxcox)) + theme_bw() + 
  ggtitle("Box-Cox Transformation for Apple Stock") + 
  ylab("Box-Cox Transformation of Closing Price") + xlab("Date")


##From the above two plots, log transformations for Variance Plot and the Box-Cox transformations plot are very similar. We can see that we got the same outputs for both the plots. Hence we can say that the variance of our time series is stationary.


#CALCULATING THE FIRST DIFFERENCE OF THE DATA BY USING THE HANDLING MEAN STATIONARITY
appl_fst_diff = appl_techlog %>% mutate(diff_close = Close - lag(Close))

appl_fst_diff %>% select(Date, Close, diff_close) %>% head()


#PLOTTING THE FIRST DIFFERENCE
appl_fst_diff %>% ggplot() + geom_line(aes(Date, diff_close)) + theme_bw() + 
  ggtitle("First Difference of the Apple Stock") + ylab("Difference in Closing Price)") + 
  xlab("Date")

##From the above plot, we can see that the variance is non-stationary during the time period 2020 through 2022. Hence let us use the First Difference, Log Values to make the variance stationary.


#FIRST DIFFERENCE, LOG VALUES

appl_fst_diff = appl_tech_era %>% mutate(close_log = log1p(Close)) %>% mutate(close_diff = close_log - lag(close_log))

appl_fst_diff %>% ggplot() + geom_line(aes(Date,close_diff)) + theme_bw() + 
  ggtitle("Apple Stock(Log; First Difference)") + ylab("Log Closing Price (Difference))") + 
  xlab("Date")


##Now, we can infer that the mean and variance both are stationary.

#ACF PLOT

acf(appl_fst_diff$Close)

##From the above ACF plot, we can infer that the given time series does not contain any seasonality. We can also see that there exists the Dampening effect in the ACF plot. Hence we can conclude that our time series is an “Auto-Regressive” process.


#PACF PLOT

pacf(appl_fst_diff$Close) #partial ACF on y axis and lag on x-axis

##From the above PACF plot, we can infer that there is only “one statistically significant autocorrelation”. Hence we can say that the order of our AR process is 1. It is denoted as ARIMA(1,1,0).



#LOG TRANSFORMATION AND DIFFERENCING
# For making the variance stationary, let us use the log transformation

appl_techlog_diff <- appl_tech_era %>% arrange(Date) %>% mutate(log_close = log1p(Close), close_diff = Close - lag(Close), close_log_diff = log_close - lag(log_close)) %>% drop_na()

# For making the mean stationary, let us use the difference

appl_techlog_diff %>% ggplot() + geom_line(aes(Date, close_log_diff)) + 
  theme_bw() + ggtitle("Difference in Log Values for Apple Stock Over the Past 10 Years") + 
  ylab("Closing Price (Difference))") + xlab("Date")


#REVERIFYING USING THE ADF TEST FOR RAW CLOSING VALUE

adf.test(appl_techlog_diff$Close)

#REVERIFYING USING THE ADF TEST FOR FIRST DIFFERENCE

adf.test(appl_techlog_diff$close_diff)

#REVERIFYING USING THE ADF TEST FOR LOG CLOSE VALUE
adf.test(appl_techlog_diff$log_close)

#REVERIFYING USING THE ADF TEST FOR DIFFERENCED LOG CLOSE VALUE
adf.test(appl_techlog_diff$close_log_diff)

##Now let us conduct the KPSS test also to conclude whether our time series transformations led to stationarity.

#REVERIFYING USING THE KPSS TEST FOR RAW CLOSING VALUE
kpss.test(appl_techlog_diff$Close)

#REVERIFYING USING THE KPSS TEST FOR FIRST DIFFERENCE
kpss.test(appl_techlog_diff$close_diff)

#REVERIFYING USING THE KPSS TEST FOR LOG CLOSE VALUE
kpss.test(appl_techlog_diff$log_close)

#REVERIFYING USING THE KPSS TEST FOR DIFFERENCED LOG CLOSE VALUE
kpss.test(appl_techlog_diff$close_log_diff)

##Now after conducting both the ADF and KPSS tests, we can conclude that from the Differenced Log Close p-values that our time series is stationary.

#FITTING ARIMA MODELS

par(mfrow = c(1,2))
acf(appl_techlog_diff$close_log_diff,lag.max = 20)
pacf(appl_techlog_diff$close_log_diff,lag.max = 20) #Partial ACF


#COMPARING AIC OF SEVERAL ARIMA MODELS

AIC(
  arima(appl_techlog_diff$close_log,order = c(0,1,1)),
  arima(appl_techlog_diff$close_log,order = c(0,1,2)),
  arima(appl_techlog_diff$close_log,order = c(0,1,3)),
  arima(appl_techlog_diff$close_log,order = c(1,1,0)),
  arima(appl_techlog_diff$close_log,order = c(1,1,1)),
  arima(appl_techlog_diff$close_log,order = c(2,1,0)),
  arima(appl_techlog_diff$close_log,order = c(3,1,0))
)

##From the above obtained AIC values, we can say that the ARIMA(0, 1, 2) is the best model with a AIC value = -13711.07

#COMPARING BIC OF SEVERAL ARIMA MODELS

BIC(
  arima(appl_techlog_diff$close_log,order = c(0,1,1)),
  arima(appl_techlog_diff$close_log,order = c(0,1,2)),
  arima(appl_techlog_diff$close_log,order = c(0,1,3)),
  arima(appl_techlog_diff$close_log,order = c(1,1,0)),
  arima(appl_techlog_diff$close_log,order = c(1,1,1)),
  arima(appl_techlog_diff$close_log,order = c(2,1,0)),
  arima(appl_techlog_diff$close_log,order = c(3,1,0))
)

##From the above obtained BIC values, we can say that the ARIMA(0, 1, 2) is the best model with a BIC value = -13693.46


#COMPUTING THE AUTO ARIMA FOR FINDING THE BEST MODEL
auto.arima(appl_techlog_diff$close_log,stationary = FALSE, allowdrift = TRUE, 
           seasonal = FALSE, stepwise = FALSE, approximation = FALSE)

##Now from the Auto ARIMA, we can conclude that the ARIMA(1,0,0) is the best model. Now we will use this ARIMA(1,0,0) for further analysis.

#LJUNG-BOX TEST
best_apple_techmodel = arima(appl_techlog_diff$close_log, order = c(1,0,0))
forecast::checkresiduals(best_apple_techmodel)

##From the ACF plot, we observe that residuals appear to be white noise as there is no particular pattern. Let us conduct the Ljung-Box Test Results to detect the auto correlation with the residuals

#LJUNG-BOX TEST RESULTS INTERPRETATION WITH RESIDUALS FOR AUTO CORRELATION

residual = best_apple_techmodel$residual
Box.test(residual, type = 'Ljung-Box', lag = 1)
Box.test(residual, type = 'Ljung-Box', lag = 2)
Box.test(residual, type = 'Ljung-Box', lag = 3)
Box.test(residual, type = 'Ljung-Box', lag = 4)
Box.test(residual, type = 'Ljung-Box', lag = 5)

##From the above test cases for Ljung-Box Test Results Interpretation with Residuals for auto correlation tells us that all the p-values are greater than 0.05, hence we can conclude that there exists “No Auto Correlation” at that lag.

#ACTUAL MODEL VS PREDICTED MODEL

best_apple_techmodel = arima(appl_techlog_diff$log_close, order = c(1,1,0))
residual = best_apple_techmodel$residuals
pred_apple_techmodel = residual + appl_techlog_diff$log_close
ggplot() + geom_line(aes(appl_techlog_diff$Date, appl_techlog_diff$log_close)) + 
  geom_line(aes(appl_techlog_diff$Date,pred_apple_techmodel),color = '#2196F3',alpha = 0.4) + 
  theme_bw() + xlab("Date") + ylab("Log Apple Stock")

##We can say that our model seems to be doing good as it is capturing proper trend


#MODEL PERFORMANCE ESTIMATION

RMSE = sqrt(mean((expm1(pred_apple_techmodel) - expm1(appl_techlog_diff$log_close))^2, na.rm = T))
RMSE

##Now, the obtained RMSE result suggests that our Apple Stock predicted model are within ~1 point on average in-sample


#BUILDING FORECASTS
best_apple_techmodel %>%  forecast(h = 150) %>% autoplot()

##As we performed ARIMA model on log values, we need to inverse those log values to the original values.


#BUILDING FORECASTS: FIVE-TIME PERIOD FORECAST

#We wanted to predict the prices for 150 days(5 time-period)

prediction = predict(best_apple_techmodel, n.ahead = 2624)

pred_data = data.frame(pred = prediction, date = appl_tech_era$Date)
pred_data = data.frame(pred = prediction, date = appl_tech_era$Date)

pred_merge = appl_tech_era %>% full_join(pred_data) %>% mutate(
  pred_high = expm1(pred.pred + 2*pred.se),
  pred_low = expm1(pred.pred - 2*pred.se),
  pred.pred = expm1(pred.pred),
  error = pred.pred - Close
)
##As I am using daily data, I am predicting forecast for 5 Months i.e., 150 days from the actual data.
plot(forecast::forecast(best_apple_techmodel, h = 150), main = "ARIMA(1,1,0) using Multi-Steps Forecast", ylab = "Closing Price", xlab = "Date")

#INVERSE TRANSFORMATION OF LOG VALUES
forecast = best_apple_techmodel %>% forecast(h = 150)

forecast$mean = exp(forecast$mean) - 1
forecast$fitted = exp(forecast$fitted) - 1
forecast$x = exp(forecast$x) - 1

autoplot(forecast, main = 'Forecast of Apple Stock Price Over Time',
         ylab = 'Actual Apple Stock Price')

##The above forecast indicates that there might slight decrease in the Apple Stock Price in the near future.


#FITTING A FACEBOOK PROPHET MODEL
#Building Prophet Model with train and test data
#install.packages("prophet")
library(prophet)
prophet_data = appl_fst_diff %>% rename(ds = Date, # Have to name our date variable "ds"
                                        y = Close) # Have to name our time series "y"

train = prophet_data %>% filter(ds < ymd("2021-01-01"))
test = prophet_data %>% filter(ds >= ymd("2021-01-01"))

model = prophet(train)
future = make_future_dataframe(model, periods = 365)
forecast = predict(model, future)

#PLOTTING THE FORECAST
plot(model, forecast) + ylab("Closing Price of Apple Stock Price") + 
  xlab("Date") + theme_bw()

prophet_plot_components(model, forecast)

##From the above plots, without yearly and trend seasonality, trend is increasing gradually over the period.

#INTERACTIVE CHART PLOT
dyplot.prophet(model, forecast)


##TIME SERIES DECOMPOSITION
#VISUALIZING THE COMPONENTS
##In this section, let us decompose the elements of the time series into trend, weekly, yearly and daily seasonality
prophet_plot_components(model, forecast)


#TWO YEAR FORECAST
#Identifying the Saturation Points: Here, we will make a forecast for two-year period to identify the need for saturation points
two_yr_future = make_future_dataframe(model, periods = 730)
two_yr_forecast = predict(model, two_yr_future)
plot(model, two_yr_forecast) + theme_bw() + xlab("Date") + ylab("Closing Price")


##We can see from the graph above that there has been no abnormal rise in the price of Apple Stock Price. Stock prices, also, continue to rise. As a result, no saturation points are required for this model.

#ESTIMATING THE TREND AND CHANGEPOINTS
#CHANGEPOINTS PLOTTING
#The prophet algorithm looks at 25 equally spaced potential changepoints as candidates for a change in trend. This examines the actual rate of change at each prospective changepoint and eliminates those with low change rates.
plot(model, forecast) + add_changepoints_to_plot(model) + theme_bw() +
  xlab("Date") + ylab("Closing price")

#CHANGEPOINT HYPERPARAMETERS
#In this section, we can manually specify a greater number of changepoints to improve performance of the model.
model = prophet(train, n.changepoints = 50)
forecast = predict(model, future)
plot(model, forecast) + add_changepoints_to_plot(model) + theme_bw() + xlab("Date") + ylab("Closing Price")


#VISUALIZING THE COMPONENTS
#Decomposition of the elements of the time series into trend, weekly and seasonality as below:
prophet_plot_components(model, forecast)

#OUT-OF-SAMPLE COMPARISON
forecast_plot_data = forecast %>% as_tibble() %>% mutate(ds = as.Date(ds)) %>% filter(ds >= ymd("2021-01-01"))

forecast_not_scaled = ggplot() + geom_line(aes(test$ds,test$y)) + xlab("Date") + ylab("Closing Price from Test Data") + geom_line(aes(forecast_plot_data$ds, forecast_plot_data$yhat), color = 'green')
forecast_scaled = forecast_not_scaled + ylim(0, 200) 
forecast_scaled

##I believe we can tell from the plot that the trend is increasing and that the pattern appears to be wrong from the actual data

#SEASONALITY ASSESSMENT
prophet_plot_components(model, forecast)


#SEASONALITY - ADDITIVE
additive = prophet(train)
add_fcst = predict(additive, future)
plot(additive, add_fcst) + ylim(0, 200)
prophet_plot_components(additive, add_fcst)


#MULTIPLICATIVE SEASONALITY
multi = prophet(train, seasonality.mode = 'multiplicative', yearly.seasonality = TRUE)
multi_fcst = predict(multi, future)
plot(multi, multi_fcst) + ylim(0,200)
prophet_plot_components(multi, multi_fcst)

#INCLUSION AND ASSESSMENT OF HOLIDAYS
model = prophet(train, fit = FALSE, yearly.seasonality = TRUE)
model = add_country_holidays(model, country_name = 'US')
model = fit.prophet(model, train)
forecast = predict(model, future)
prophet_plot_components(model, forecast)

##The graph above shows that there appears to be a holiday effect in October of each year when the stock price rises. Let’s look at which holiday has the most impact.

#EXAMINING IMPACT OF HOLIDAYS

forecast %>% filter(holidays != 0) %>% dplyr::select(-ds:-additive_terms_upper, -holidays:-holidays_upper, -weekly:-yhat, -contains("upper"), 
                                                     -contains("lower")) %>% mutate_all(~ if_else(. == 0, as.numeric(NA), .)) %>% summarize_if(is.numeric, ~ max(., na.rm = T)) %>% pivot_longer(cols = `Christmas Day`:`Washington's Birthday`, names_to = 'holiday', values_to = 'effect') %>% ggplot() + 
  geom_col(aes(effect, holiday)) + theme_bw()


##Veterans Day and Columbus Day appear to be the most important holidays affecting the stock’s closing price, according to the above graph. Though we need investigate how Columbus Day affects Apple’s closing stock price further. However, it appears that holidays have little or no impact on the statistics, which makes sense given that markets are closed during this time of year.


#IN-SAMPLE PERFORMANCE OF THE PROPHET MODEL BASED ON RMSE
forecast_metric_data = forecast %>% as_tibble() %>% mutate(ds = as.Date(ds)) %>% filter(ds <= ymd("2021-01-01"))

RMSE = sqrt(mean((train$y - forecast_metric_data$yhat)^2))
MAE = mean(abs(train$y - forecast_metric_data$yhat))
MAPE = mean(abs((train$y - forecast_metric_data$yhat)/train$y))

print(paste("RMSE:", round(RMSE,2)))

print(paste("MAE:", round(MAE,2)))

print(paste("MAPE:", round(MAPE,2)))

##The in-sample model has an RMSE of 4.92 and an MAE of 2.77. Because forecast during the end of in-sample data appears to be away from original data due to outlier, RMSE is bigger than MAE. 
##In comparison to MAE, RMSE penalizes more. 
##MAPE is around 7%, indicating that we are only 7% off during the entire training period, indicating that the model is functioning well.



#ROLLING WINDOW CROSS-VALIDATION WITH PROPHET MODEL
df.cv <- cross_validation(model, initial = 365, period = 30, horizon = 30, units = 'days')
head(df.cv)

unique(df.cv$cutoff)

##We’ll be making 96 different models because we have 96 different cut-offs.


#PROPHET MODEL ASSESSMENT
#CROSS-VALIDATION ACTUAL VS PREDICTED
df.cv %>% ggplot() + geom_point(aes(ds,y)) + 
  geom_point(aes(ds, yhat, color = factor(cutoff))) + theme_bw() + 
  xlab("Date") + ylab("Closing Price") + scale_color_discrete(name = 'Cutoff')


##The model appears to be working well in the training model, but the data pattern appears to be a little odd at the end.

#PERFORMANCE METRICS TABLE OF CROSS-VALIDATION
#We will generate a table for various performance metrics for different time horizons

performance_metrics(df.cv)


#VARIOUS METRICS PLOTS
#We will plot visualizations for metrics like RMSE, MAE, MAPE, MDAPE, SMAPE.

plot_cross_validation_metric(df.cv, metric = 'rmse')


plot_cross_validation_metric(df.cv, metric = 'mae')
plot_cross_validation_metric(df.cv, metric = 'mape')
plot_cross_validation_metric(df.cv, metric = 'mdape')
plot_cross_validation_metric(df.cv, metric = 'smape')


#MODEL COMPARISON AND VALIDATION BETWEEN BEST MODELS OF ARIMA AND PROPHET
#ARIMA MODEL COMPARISON USING RMSE
train_log <- train %>% mutate(y_log = log1p(train$y)) %>% drop_na()
best_arima_mod <- auto.arima(train_log$y_log, stationary = FALSE, allowdrift = TRUE, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
checkresiduals(best_arima_mod)

#OUT-OF-SAMPLE RMSE
test_pred = forecast(best_arima_mod, 368)

# Extract predicted values
test_arima_pred <- test_pred$mean

# Ensure lengths match
if (length(test_arima_pred) > length(test$y)) {
  test_arima_pred <- test_arima_pred[1:length(test$y)]
} else if (length(test$y) > length(test_arima_pred)) {
  test$y <- test$y[1:length(test_arima_pred)]
}

# Calculate RMSE
arima_error <- test$y - expm1(test_arima_pred)
out_arima_rmse <- sqrt(mean(arima_error^2, na.rm = TRUE))

# Print RMSE
out_arima_rmse


#Out-of-sample RMSE for best arima model is 18.89076
#PROPHET MODEL COMPARISON USING RMSE
best_prophet_mod <- prophet(train)

future <- make_future_dataframe(model, periods = 262)
forecast <- predict(best_prophet_mod, future)

forecast_metric_data <- forecast %>% as_tibble() %>% mutate(ds = as.Date(ds)) %>% filter(ds >= ymd("2021-01-01"))

out_prophet_rmse <- sqrt(mean((test$y - forecast_metric_data$yhat)^2))
out_prophet_rmse

#OUT-OF-SAMPLE RMSE COMPARISON BETWEEN ARIMA AND PROPHET
tibble(`best_ARIMA` = round(out_arima_rmse,2), `best_Prophet` = round(out_prophet_rmse,2))














