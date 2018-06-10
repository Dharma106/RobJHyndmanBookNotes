library(forecast)

# To get time series data used in Rob J Hyndman book, one can usage
# "fpp" packages (Forecasting Principal and practice).
library(fpp)

# To download time series data online using API use "rdatamarket",
# "Quandl" package.

library(rdatamarket)
library(Quandl)
# To know the available data set in a package.
data(package = "fma")

#1.
# Graphs help to undersatnd data.
# It helps in visualizing patterns, unusual observations, changes over time
# and relationships between variables.
# Types of data determines what forecasting method to use, it also determines
# what graphs are appropriate.

#2.
# For time series data, obivious graph to start is with "time plot" 
plot()

# Time series data will have a trend or seasonality or cyclic pattern.
# A "Trend" exists in data when there is a long-term increase or decrease
# A "Seasonal" patter occurs when a time series is affeceted by seasonal 
# factor such as time of the year or the day of a week.
# A "Cycle" occurs when the data exhibit rise and falls that are not of a
# fixed period. These fluctuations are usually due to economic conditions
# and are often related to business cycle.

# To check sesonality in data we use season plot function
seasonplot(a10, ylab = "$ million", xlab = "Year",
           main = "Seasonal plot : antidiabetic drug sales", 
           year.labels = TRUE, #year.labels.left = TRUE,
           col = 1:20, pch = 19 )


# Seasonal subseries plots
# An alternate way that emphasize the seasonal patterns is where the data
# for each seasons are collected together in seperate mini time plots.

monthplot(a10, ylab = "$ million", xlab ="Month", xaxt = "n", 
          main = "Seasonal Deviation plot: antibiotic drug sales")
axis(1, at = 1:12, labels = month.abb)

# cross-sectional data

# Auto-Correlation measures the linear relationship between lagged values of
# a time series as an e.g. r1 measures the relationship between yt and yt-1
# and so on.

# beer2 <- window(ausbeer, start = 1992, end = 2006, frequency = 4)
beer2 <- window(ausbeer, start = 1992, end = 2006-.1)
lag.plot(beer2, lags = 9, do.lines = FALSE)

# Plotting of auto correlation coefficients against their crossponding lags
# forms autocorrelation function (ACF) and this plot is known as correlogram.

# The boundary is determined using +-Z(1-alpha/2)*SE(rh) with rh as the
# estimated auto correlation at lag h.
# Z(1-alpha/2) is the quantile of normal distribution. SE is standard error.
# SE(r1) = 1/sqrt(N)
# SE(rh) = sqrt((1 + 2 * sum(ri^2))/N) for h > 1, (1<= i <=n)

Acf(beer2)

# Simple forecast method
# 1. Average method- Forecast value is the average of historical data.
# 2. Naive method - Forecast for all future time period is simply the last
# observed value.
# 3. Seasonal Naive method - Forecast for all futrue time period is set to the
# last observed value from the same season of the year.
# Forecast value= yT+h−km where m= seasonal period, k=⌊(h−1)/m⌋+1,

# 4. Drift Method- A variation on the naive method (2) to allow the forecast
# to increase or decrease over time with the amount of change (called drift)
# which is considered as the average change seen in the historical data.
# So forecast for time T+h is
#  yT+(h/T−1)*∑(yt−y(t−1))  where 2<= t <=T
#  = yT+ h(yT−y1)/(T−1)

# Examples for mentioned method

beer2 <- window(ausbeer, start = 1992, end = 2006-.1)
average_fit <- meanf(beer2, h = 11)
naive_fit <- naive(beer2, h = 11)
snaive_fit <- snaive(beer2, h = 11)
plot(average_fit, plot.conf = FALSE,
     main = "Forecast for quaterly production")
lines(naive_fit$mean, col = 2)
lines(snaive_fit$mean, col = 3)
legend("topleft", lty = 1, col= c(4,2,3),
       legend = c("Mean Method","Naive Method", "Seasonal Naive Method"))

# Drift method illustration
dj_data <- window(dj, end = 250)
plot(dj_data, main = "DJ Index (daily ending till 15 july 1994)",
     ylab = "Closing Price", xlab = "Day", xlim = c(2, 250))
lines(meanf(dj_data, h = 42)$mean, col = 4)
lines(rwf(dj_data, h = 42)$mean, col =2)
lines(rwf(dj_data, drift = TRUE, h = 42)$mean, col = 3)

legend("topleft", lty = 1, col = c(4,2,3),
       legend = c("Mean Method", "Naive Method", "Drift Method"))


## Transformations and adjustments

# 1. Mathematical transformation such as logarithmic transformation, 
# power transformation can be used when data shows increase or decrease
# with the level of the series.
# Box-Cox Transformation that includes logarithms and power transformation based on
# parameter lambda.
# wt={log(yt) if λ=0; otherwise (yt^(λ)−1)/λ 

# BoxCox.lambda() function choose a best value of lamba.
lambda <- BoxCox.lambda(elec) 
plot(BoxCox(elec, lambda))

# 2. Calendar adjustments
# Some variation in ts data would be a result of simple calendar effects.
# As an e.g. for a monthly milk production of a farm, there will be variation 
# between the months simply because of the different numbers of days in each 
# month in addition to seasonal variation across the year
# e.g. milk data
monthdays <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31), 14)
monthdays[26+ (4*12)*(0:2)] <- 29 
par(mfrow= c(2,1))
plot(milk, main = "Monthly milk production per cow", 
     ylab =  "Pounds", xlab = "Years")
plot(milk/monthdays, main = "Average milk production per cow per day",
     ylab = "Pounds", xlab = "Years")

# 3. Population adjustments.
# for most data that are affected by population changes, it is best to use
# per-captia data rather than totals.


# 4. Inflation adjustments
# Those Data that are affected by the value of money should be well adjusted
# before modelling.
# Therefore financial time series are usually adjusted so all values are stated
# in dollar values from a particular year.
# To make adjustment for price affected, one can use price index and 
# Consumer Price Index (CPI) for consumer goods.
# If zt denotes price index and yt denotes the original price (say house) 
# in year t, then xt = yt/zt * z2000 gives the adjusted house price at year 
# 2000 dollar values.  


# Forecast Accuracy Evaluation
# 1. Scale dependent errors are MAE, RMSE
# uses :- When comparing forecast methods on a single data set, the MAE
# (Mean Absolute Errro) is popular as it is easy to understand and compute.

# 2. Percentage error
# Percentage errors have the advantage of being scale-independent, and so are
# frequently used to compare forecast performance between different data sets.
# commonly used method:-
#       Mean absolute percentage error: MAPE=mean(|pi|) where pi=100*ei/yi
#       and ei = yi−y^i


# # Points to be noted
#
# 1. A model which fits the data well does not necessarily forecast well.
# 2. A perfect fit can always be obtained by using a model with enough parameters.
# 3. Over-fitting a model to data is as bad as failing to identify the systematic
# pattern in the data.

# Residual Diagnostics
# residuals are defined as the difference of observed value and its forecast 
# based on other observations: ei = yi- ^yi
# 
# Good Forecasting model will yield residuals with properties:
#   1. The residuals are uncorrelated. If there are correlations between residuals,
#    then there is information left in the residuals which should be used in
#    computing forecasts.
#   2. The residuals have zero mean. If the residuals have a mean other than zero,
#    then the forecasts are biased.
#    (solution:This can be solved by adding the mean (m) of residuals to all the
#     forecast value to remove the biasness.
# In addition to these essential properties, it is useful (but not necessary)
# for the residuals to also have the following two properties.
#   1. The residuals have constant variance.
#   2. The residuals are normally distributed.

# Dow Jones Index (DJI) to illustrate residual diagnosis.
dj2 <- window(dj, end = 250)
plot(dj2, main = "DJI", ylab = "", xlab = "Day")
res <- residuals(naive(dj2))
plot(res, main ="Residuals from naive method", ylab = "", xlab ="Day")
Acf(res, main = "ACF of residuals")
hist(res, nclass = "FD", main = "Histogram of residuals")


# A test for a group of autocorrelations is called portmanteau test. One such
# test is Box-pierce test based on the statistice Q = T * (sum rk^2) over 
# 1<= k <= h, where T is the total number of observations. 
#   if each rk is close to zero, then Q will be small. If some rk is large(positive 
#   or negative), then Q will be large. suggested number of h = 10 for non seasonal
#   data and h = 2m for seasonal data, where m is the period of seasonality.
#   However, the test is not good when h is large, so if these values are larger
#   than T/5, then use h=T/5.


# Prediction Intervals Key notes
# 1. Assuming that the forecast errors are uncorrelated and normally distributed,
#    a simple 95% prediction interval for next observation in time series is
#   yt ± 1.96*σ where σ is an estimate of the standard deviation of forecast
#   distribution.
# 2. For forecasting one-step ahead, the sd of the forecast distribuiton is almost
#   the same as the sd of residuals.
# 3. A common feature of the prediction interval is that they increase in length as
#   the time forecast horizon increases.
# 4. If a transformation has been used, then the prediction interval should be 
#   computed on the transformed scale and end points back transformed to give
#   a prediction interval on the original scale.

# Functions that output a forecast object:
  meanf()
  naive(); snaive()
  rwf()
  croston()
  stlf()
  ses()
  holt(); hw()
  splinef()
  thetaf()

# forecast() function
# 1. Takes a time series or time series model as its main argument
# 2. If first argument is class ts, returns forecasts from automatic ETS algorithm
# 3. Methods for objects of class Arima, ar, HoltWinters, StructTS, etc.
# 4. Output as class forecast.     


## Judgemental Forecasts
#  Three general settings where judgmental forecasting is used
#  1. There are no available data so that statistical methods are not applicable
#  and judgmental forecasting is the only feasible approach.
#  2. Data are available, statistical forecasts are generated and these are then
#   adjusted using judgement
#  3. Data are available and statistical and judgmental forecasts are
#   independently generated and then combined

# Delphi Method:-
  # The Delphi method generally involves following stages:-
  # 1. A panel of experts is assembled.
  # 2. Forecasting tasks/challenges are set and distributed to the experts.
  # 3. Experts return initial forecasts and justifications. These are compiled and 
  #    summarised in order to provide feedback.
  # 4. Feedback is provided to the experts who now review their forecasts in light
  #    of the feedback. This step may be iterated until a satisfactory level of 
  #    consensus is reached.
  # 5. Final forecasts are constructed by aggregating the experts’ forecasts




# Simple Regression 
  # The slope coefficient β^1 can also be expressed as
  # β^1= r * Sy/Sx where r is correlation and Sy & Sx are standard deviation of y
  # and x observations respectively.

# Example code
  
plot(jitter(Carbon) ~ jitter(City), xlab = "City (mpg)", 
     ylab = "Carbon footprint (tons per year)", data = fuel)

fit <- lm(Carbon ~ City, data = fuel)
abline(fit)
summary(fit)

res <- residuals(fit)
plot(jitter(res)~ jitter(City), ylab = "Residuals", xlab = "City", data = fuel)

fitted(fit)[1]
fcast <- forecast(fit, newdata = data.frame(City = 30))
plot(fcast, xlab = "City (mpg)", ylab = "Carbon footprint (tons per year)")



# Regreesion with time series data
fit.ex3 <- tslm(consumption ~ income, data = usconsumption)
plot(usconsumption, ylab = "% change in the consumption and income", 
     plot.type = "single", col = 1:2, xlab = "year")
legend("topright", legend = c("Consumption", "Income"), lty =1,
       col = c(1:2), cex = 0.8)
plot(consumption ~ income, data = usconsumption, xlab = "% change in income",
     ylab = "% change in consumption")
abline(fit.ex3)
summary(fit.ex3)

estimate <- forecast(fit.ex3, newdata = data.frame(income = c(-1,1)))

# using regression models with time series data, we need to distinguish between
# two different types of forecasts that can be produced, depending on what is
# assumed to be known when the forecasts are computed.
# 1. Ex ante forecasts - These are those values that are made using only information
# that is avilable in advance.
# 2. Ex post forecasts are those that are made using later information on the 
# predictors.

# e.g.
fit.ex4 <- tslm(austa ~ trend)
fc <- forecast(fit.ex4, h = 5, level = c(80,95))
plot(fc, ylab = "International tourist arrivals to Australia (millions)", xlab = "t")
lines(fitted(fit.ex4), col = "blue")
summary(fit.ex4)

# Residual auto correlation.
# 1. with time series data it is highly likely that the value of a variable observed in
# current time period will be influenced by its value in previous time period or even
# the period before that, and so on.
# 2. This violates the assumption of no autocorrelation in the erros and the forecast may
# be inefficient. 
# 3. The forecast form a model with autocorrelated errors are still unbiased and so 
#  are not wrong, but usually they will have larger prediction intervals that they need

par(mfrow = c(2,2))
res3 <- ts(resid(fit.ex3), s=1970.25, frequency = 4)
plot.ts(res3, ylab = "Residual (consumption)")
abline(0,0)
Acf(res3)
res4 <- resid(fit.ex4)
plot(res4, ylab = "Residual (Tourism)")
abline(0,0)
Acf(res4)

# More often than not, time series data are “non-stationary”; that is, the
# values of the time series do not fluctuate around a constant mean or with
# a constant variance

# Regressing non-stationary time series can lead to spurious regressions.
# High r-square and high residual autocorrelation can be signs of spurious regression. 



# Chapter 5
# Multiple Regression

pairs(credit[, -c(4,5)], diag.panel = panel.hist())
# the panel.hist function is defined in help(pairs)

# Credit predictors seems to be highly skewed and few outlying observations
# are making it hard to see what is going on in the bulk of data.

# Approach to solve this problem is take transformation.


# Dummy Variable
# Interpreations of each of the coeffiecients associated with the dummy variable
# is that it is a measure of the effect of that category relative to the omitted
# category.








