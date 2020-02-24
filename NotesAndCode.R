# Required Package ####
library(forecast)
# To get time series data used in Rob J Hyndman book, one can usage
# "fpp" packages (Forecasting Principal and practice).
library(fpp)
# New version of package
library(fpp2)
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
seasonplot(
  a10,
  ylab = "$ million",
  xlab = "Year",
  main = "Seasonal plot : antidiabetic drug sales",
  year.labels = TRUE,
  #year.labels.left = TRUE,
  col = 1:20,
  pch = 19
)


# Seasonal subseries plots
# An alternate way that emphasize the seasonal patterns is where the data
# for each seasons are collected together in seperate mini time plots.

monthplot(
  a10,
  ylab = "$ million",
  xlab = "Month",
  xaxt = "n",
  main = "Seasonal Deviation plot: antibiotic drug sales"
)
axis(1, at = 1:12, labels = month.abb)

# cross-sectional data

# Auto-Correlation measures the linear relationship between lagged values of
# a time series as an e.g. r1 measures the relationship between yt and yt-1
# and so on.

# beer2 <- window(ausbeer, start = 1992, end = 2006, frequency = 4)
beer2 <- window(ausbeer, start = 1992, end = 2006 - .1)
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
# Forecast value= yT+hâkm where m= seasonal period, k=â(hâ1)/mâ+1,

# 4. Drift Method- A variation on the naive method (2) to allow the forecast
# to increase or decrease over time with the amount of change (called drift)
# which is considered as the average change seen in the historical data.
# So forecast for time T+h is
#  yT+(h/Tâ1)*â(ytây(tâ1))  where 2<= t <=T
#  = yT+ h(yTây1)/(Tâ1)

# Examples for mentioned method

beer2 <- window(ausbeer, start = 1992, end = 2006 - .1)
average_fit <- meanf(beer2, h = 11)
naive_fit <- naive(beer2, h = 11)
snaive_fit <- snaive(beer2, h = 11)
plot(average_fit, plot.conf = FALSE,
     main = "Forecast for quaterly production")
lines(naive_fit$mean, col = 2)
lines(snaive_fit$mean, col = 3)
legend(
  "topleft",
  lty = 1,
  col = c(4, 2, 3),
  legend = c("Mean Method", "Naive Method", "Seasonal Naive Method")
)

# Drift method illustration
dj_data <- window(dj, end = 250)
plot(
  dj_data,
  main = "DJ Index (daily ending till 15 july 1994)",
  ylab = "Closing Price",
  xlab = "Day",
  xlim = c(2, 250)
)
lines(meanf(dj_data, h = 42)$mean, col = 4)
lines(rwf(dj_data, h = 42)$mean, col = 2)
lines(rwf(dj_data, drift = TRUE, h = 42)$mean, col = 3)

legend(
  "topleft",
  lty = 1,
  col = c(4, 2, 3),
  legend = c("Mean Method", "Naive Method", "Drift Method")
)


## Transformations and adjustments

# 1. Mathematical transformation such as logarithmic transformation,
# power transformation can be used when data shows increase or decrease
# with the level of the series.
# Box-Cox Transformation that includes logarithms and power transformation based on
# parameter lambda.
# wt={log(yt) if Î»=0; otherwise (yt^(Î»)â1)/Î»

# BoxCox.lambda() function choose a best value of lamba.
lambda <- BoxCox.lambda(elec)
plot(BoxCox(elec, lambda))

# 2. Calendar adjustments
# Some variation in ts data would be a result of simple calendar effects.
# As an e.g. for a monthly milk production of a farm, there will be variation
# between the months simply because of the different numbers of days in each
# month in addition to seasonal variation across the year
# e.g. milk data
monthdays <-
  rep(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), 14)
monthdays[26 + (4 * 12) * (0:2)] <- 29
par(mfrow = c(2, 1))
plot(milk,
     main = "Monthly milk production per cow",
     ylab =  "Pounds",
     xlab = "Years")
plot(milk / monthdays,
     main = "Average milk production per cow per day",
     ylab = "Pounds",
     xlab = "Years")

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
#       and ei = yiây^i


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
plot(dj2,
     main = "DJI",
     ylab = "",
     xlab = "Day")
res <- residuals(naive(dj2))
plot(res,
     main = "Residuals from naive method",
     ylab = "",
     xlab = "Day")
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
#   yt Â± 1.96*Ï where Ï is an estimate of the standard deviation of forecast
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
naive()
snaive()
rwf()
croston()
stlf()
ses()
holt()
hw()
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
# 5. Final forecasts are constructed by aggregating the expertsâ forecasts




# Simple Regression
# The slope coefficient Î²^1 can also be expressed as
# Î²^1= r * Sy/Sx where r is correlation and Sy & Sx are standard deviation of y
# and x observations respectively.

# Example code

plot(
  jitter(Carbon) ~ jitter(City),
  xlab = "City (mpg)",
  ylab = "Carbon footprint (tons per year)",
  data = fuel
)

fit <- lm(Carbon ~ City, data = fuel)
abline(fit)
summary(fit)

res <- residuals(fit)
plot(
  jitter(res) ~ jitter(City),
  ylab = "Residuals",
  xlab = "City",
  data = fuel
)

fitted(fit)[1]
fcast <- forecast(fit, newdata = data.frame(City = 30))
plot(fcast, xlab = "City (mpg)", ylab = "Carbon footprint (tons per year)")



# Regreesion with time series data
fit.ex3 <- tslm(consumption ~ income, data = usconsumption)
plot(
  usconsumption,
  ylab = "% change in the consumption and income",
  plot.type = "single",
  col = 1:2,
  xlab = "year"
)
legend(
  "topright",
  legend = c("Consumption", "Income"),
  lty = 1,
  col = c(1:2),
  cex = 0.8
)
plot(consumption ~ income,
     data = usconsumption,
     xlab = "% change in income",
     ylab = "% change in consumption")
abline(fit.ex3)
summary(fit.ex3)

estimate <-
  forecast(fit.ex3, newdata = data.frame(income = c(-1, 1)))

# using regression models with time series data, we need to distinguish between
# two different types of forecasts that can be produced, depending on what is
# assumed to be known when the forecasts are computed.
# 1. Ex ante forecasts - These are those values that are made using only information
# that is avilable in advance.
# 2. Ex post forecasts are those that are made using later information on the
# predictors.

# e.g.
fit.ex4 <- tslm(austa ~ trend)
fc <- forecast(fit.ex4, h = 5, level = c(80, 95))
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

par(mfrow = c(2, 2))
res3 <- ts(resid(fit.ex3), s = 1970.25, frequency = 4)
plot.ts(res3, ylab = "Residual (consumption)")
abline(0, 0)
Acf(res3)
res4 <- resid(fit.ex4)
plot(res4, ylab = "Residual (Tourism)")
abline(0, 0)
Acf(res4)

# More often than not, time series data are ânon-stationaryâ; that is, the
# values of the time series do not fluctuate around a constant mean or with
# a constant variance

# Regressing non-stationary time series can lead to spurious regressions.
# High r-square and high residual autocorrelation can be signs of spurious regression.



# Chapter 5
# Multiple Regression

pairs(credit[, -c(4, 5)], diag.panel = panel.hist())
# the panel.hist function is defined in help(pairs)

# Credit predictors seems to be highly skewed and few outlying observations
# are making it hard to see what is going on in the bulk of data.

# Approach to solve this problem is take transformation.


# Dummy Variable
# Interpreations of each of the coeffiecients associated with the dummy variable
# is that it is a measure of the effect of that category relative to the omitted
# category.



#     ******** Chapter 6 *********
#       ********          ********

# Time series Components Trend, Seasonality and Cyclic
#1. Trend:- A trend exists when there is a long-term increase or decrease in the      data and it is not necessary that it will be linear it can be of any form.

#2. Seasonal: A seasonal pattern exists when a series is influenced by seasonal
# factors such as quarter of the year, the month, or day of the week. Seasonality is    always of a fixed and known period.

# 3. Cyclic: A cyclic pattern exists when data exihibits rises and falls that are
# not of fixed period. The duration of these fluctuations is usually of at least 2     years.

# Examples to distinguish time series component.

par(mfrow = c(2, 2))
plot(hsales, xlab = "Year", ylab = "Monthly housing sales (Millions)")
plot(ustreas, xlab = "Day", ylab = "US treasury bill contracts")
plot(elec, xlab = "Year", ylab = "Australian monthly elctricity production")
plot(diff(dj), xlab = "Day", ylab = "Daily change in Dow Jones index")

# One should think time series yt as comprisisng three components: a seasonal component, a trend-cycle component (containing both trend and cycle), and a remiander component (containing anything else in the time series). Depending upon the seasonality behaviour one can either assume a time series to be additvie or multiplicative.

# Additive model yt = St+ Tt + Et where yt is data at period t, St is the seasonsal component at period t, Tt is trend-cycle component at period at t and Et is the remainder (or irregular error) component at period t.
# Alternatively a multiplicative model can be written as
#  yt = St * Tt * Et

# Note: Additive Model use rule
#   If the magnitude of the seasonal fluctuation or the variation around the 
#   trend-cycle does not vary with the level of time series.

# Note: Multiplicative Model use rule
#   if the variation in the seasonal pattern or variation around the trend-cycle
#   appears to be proportional to the level of the time series.

# An alternative to using a multiplicative model, is to first trasform the data until
# variation in the series appears to be stable, then use an additive model using log 
# transforamtion.


# Example of Electrical Equipment manufacturing (Euro Area)

fit <- stl(elecequip, s.window = 5)
plot(elecequip, col= "grey", 
     main= "Electrical equipment manufacturing", 
     ylab ="New order Index",
     xlab = "")
lines(fit$time.series[,2], col = "red", ylab = "Trend")

# Plotting all the componet of time series
plot(fit)
# To visualize seasonal plots over each period (month)
monthplot(fit$t[,"seasonal"], main = "", ylab = "Seasonal")

# Seasonally adjusted data
# If seasosnal component is remvoved from the original data, the resulting
# values are called the "seasonally adjusted" data.
# For additive model, the seasonally adjusted data are given by yt-St, and for
# multiplicative data, the seasonally adjusted values are obtained using yt/St.

plot(elecequip, col = "grey", 
     main = "Electrical equipment manufacturing",
     xlab = "", ylab = "New order Index")

lines(seasadj(fit), col = "red", ylab ="Seasonally adjusted")


# 6.2  Moving Average

# First step in classical decomposition is to use a moving average method to
# estimate the trend-cycle.
    # A moving average of order m can be written as 
    # Tt =1/m(sum Yt+j over j= -k to k) where m = 2k+1
# The estimate of trend cycle at time t is obtained by averaging values of time
# series within k periods of t. 
#     Observations that are nearby in time are also likely to be close in value,
#     and the average removes some of the randomness in the data, leaving a smooth
#     trend-cycle component. This is called as m-MA meaning a moving average of ord m.

ma(elecsales, order = 5)




moving_average <- function(data, m=3){
  if(missing(data)){
    warning("Data is missing!")
    stop()
  }
  if(m <= 1)
    warning("Order must be of odd type. Choosing default order")
  k = (m-1)/2 # m must be odd number
  j = (-k:k)
  t = 0
  len = length(data)
  data1 <- vector()
  output_data <- vector()
  for (i in 1:(len-1)) {
    t = k + i
    for(y in 1:length(j)){
      data1[y] <-data[t+j[y]]
    }
    output_data[t] <- sum(data1)/m
  }
  return(output_data)
}

plot(elecsales, main = "Residential electricity sales", 
     ylab="Gwh", xlab ="Year")
lines(ma(elecsales, order = 5), col ="red")

# The order of moving average determines the smoothness of the trend-cycle estimate.
# A larger order means a smoother curve.

par(mfrow = c(2,2))
plot(elecsales, main = " Residential electricity sales",
     ylab= "Gwh", xlab = "Year")
lines(ma(elecsales, order = 3), col = "green")

plot(elecsales, main = " Residential electricity sales",
     ylab= "Gwh", xlab = "Year")
lines(ma(elecsales, order = 5), col = "red")

plot(elecsales, main = " Residential electricity sales",
     ylab= "Gwh", xlab = "Year")
lines(ma(elecsales, order = 7), col = "blue")

plot(elecsales, main = " Residential electricity sales",
     ylab= "Gwh", xlab = "Year")
lines(ma(elecsales, order = 9), col = "pink")


beer2 <- window(ausbeer, start = 1992)
ma4 <- ma(beer2, order = 4, centre = FALSE)
ma2x4 <- ma(beer2, order = 4, centre = TRUE)


# To calculate moving average of even order with centred value, one can use the
# even order(m) plus one observatons of data and calculate average by assiging weights 1/m for [yt+j+1 for j<0 and yt+j-1 for j>0] and 1/2m for yt+j where j =[-k,k] and
# k = (m-1)/2. This is done to make moving average of even order symmetric.

# In general, an even order MA should be followed by an even order MA to make it symmetric. Similarly, an odd order MA should be followed by odd order MA.

# Most common use of centred moving average is in estimating the trend cycle from 
# seasonal data. Considering 2 X 4-MA when applied to quaterly data, each quarter of 
# the year is given equal weight as the first and last terms apply to the same quarter
# in consecutive years. Consequently, the seasonal variation will be averaged out, 
# resulting values of estimated Tt will have little or no seasonal variation remaining

# Therefore, 
# If seasonal period is even and of order m, use 2 X m-MA to estimate the trend-cycle.
# If seasonal period is odd and of order m, use m-MA to estimate trend-cycle.
    # In particular, 2 X 12-MA can be used to estimate the trend-cycle of monthly data
    # 7-MA can be used to estimate trend-cycle of daily data.


# Example for even order i.e. 12 MA for monthly data
plot(elecequip, ylab = "New index order", col = "grey", 
     main = "Electrical Equipment manufacturing (Euro area)")
lines(ma(elecequip, order = 12), col = "red")


#  Classical Decomposition Method
# Two forms of classical decompostion : Additive and Multiplicative
# In classical decompositon, it is assumed that seasonal component is constant
# from year to year.

# ******* Additive Decomposition ********

# Step 1 :- Compute ther trend-cycle component Tt cap using (2 X m-MA) technique if m
# is even and m-MA concept if m is odd.

# Step 2 :- Calcuate the detrended series: yt-Tt cap.

# Step 3 :- Next to calculate seasonal component for each month, simply average the 
# detredned values for that month. As an e.g, the seasonal index for March is the average
# of all the detrended March values in the data. Once indces are obtained, it is adjusted 
# to ensure it adds up to zero. The seasonal componet is obtained by combinin together all 
# the seasonal indices for each year of data. This gives St cap.

# Step 4: The remainder component is calculated by subtracting the estimated seasonal and 
# trend-cycle components: Et=ytâTtâSt. 


# ******* Multiplicative Decomposition ********
# Steps remain same as additive only change is instead of subtracting division is done.
# There for Detrended series is calculated :- yt/Tt
# Error component is calculated Et = yt/(Tt*St)

# Classical decomposition methods assume that the seasonal component repeats from 
# year to year. For many series, this is a reasonable assumption, but for some 
# longer series it is not.
# The classical decomposition methods are unable to capture the change in seasonal # changes over time. 
# The classical method is not robust to kinds of unusual values.

# ******   X-12-ARIMA decomposition  ******
# X-12-ARIMA mehtod is very popular for decomposing quaterly and monthly data.
# The X-12-ARIMA method is based on classical decomposition, but with many extra 
# steps and features to overcome the drawbacks of classical decomposition.
  # In particular, the trend estimate is available for all observations including   # the end points, and the seasonal component is allowed to vary slowly over time

# X-12-ARIMA Methodology
# The algorithm begins in a similar way to classical decomposition, and then the 
# components are refined through several iterations.

# **Please see the sets involved in the algorithm in the book.


# STL Decompostion :- STL is a very versatile and robust decompostion time series
# STL is an acronym for "Seasonality and Trend Decomposition using Loess" while 
# loess is a method for estimating nonlinear relationships. 

# STL has several advantages over the classical decomposition method and X-12-ARIMA:

# Unlike X-12-ARIMA, STL will handle any type of seasonality, not only monthly and # quarterly data.
# The seasonal component is allowed to change over time, and the rate of change 
# can be controlled by the user.
# The smoothness of the trend-cycle can also be controlled by the user.
# It can be robust to outliers (i.e., the user can specify a robust decomposition
# ). So occasional unusual observations will not affect the estimates of the trend
# -cycle and seasonal components. They will, however, affect the remainder 
# component.

fit <- stl(elecequip, t.window = 15, s.window = "periodic", robust = TRUE)
# The two main parameters to be chosen when using STL are the 
# trend window (t.window) and seasonal window (s.window).
# These control how rapidly the trend and seasonal components can change.
# Small values allow more rapid change. 
# Setting the seasonal window to be infinite is equivalent to 
# forcing the seasonal component to be periodic (i.e., identical across years).

eeadj <- seasadj(fit)
plot(naive(eeadj), xlab= "New orders index", 
     main = "Naive Forecast of seasonally ajdusted data")
fcast <- forecast(fit, method = "naive")
plot(fcast, ylab = "New order index")


# ***** Chapter 7 - Exponential Smoothing ******

oildata <- window(oil, start = 1997, end = 2007)
plot(oildata, ylab = "Oil (millions of tonnes)", xlab = "Year")

# Exponential Smoothing : This method calculates forecast using weighted average
# where the weights decrease exponentially as observations come from further in
# the past-- the smallest weights are associated with the oldest observations.

#  y^T+1|T=Î±yT+Î±(1âÎ±)yTâ1+Î±(1âÎ±)2yTâ2+â¯, where 0â¤Î±â¤1  is the smoothing parameter
# The rate at which the weights decrease is controlled by the parameter Î±.

# Weighted average form
# The forecast at time t+1 is equal to a weighted average between the most 
# recent observation yt and the most recent forecast y^ t|tâ1.

# y T+1|T= âÎ±(1âÎ±)^j y^(Tâj) + (1âÎ±)^T * â0 where 0<= j <= T-1

# Optimization Method for SES
# Every exponential smoothing method we also need to choose the value for the
# smoothing parameters. For single exponential smoothing (SES), there is only 
# one smoothing parameter (Î±).
# As in regression model, the coeffiecients are estimated by minimizing the sum
# of squared errors (SSE), similarly, the unknown parameters and the inital 
# values for any exponential smoothing method can be estimated by minimizing
# SSE. Hence we find the values of the unknown parameters and the initial values # that minimize  
#       SSE=â(ytây^ t|tâ1)^2= âet^2 where t lies between 1& T.

# Unlike the regression case (where we have formula that return the values of
# the regression coefficients which minimize the SSE) this involves a non-linear
# minimization problem and need to use an optimization tool to perform this.

# examples
fit1 <- ses(oildata, alpha = 0.2, initial = "simple", h = 3)
fit2 <- ses(oildata, alpha = 0.6, initial = "simple", h = 3)
fit3 <- ses(oildata, h = 3)

plot(fit1,  ylab = "Oil (Millions of tonnes)", 
     xlab = "Year", main = "", fcol = "white", type = "o" )

lines(fitted(fit1), col = "blue", type = "o")
lines(fitted(fit2), col = "red", type = "o")
lines(fitted(fit3), col = "green", type = "o")
lines(fit1$mean, col = "blue", type = "o")
lines(fit2$mean, col = "red", type = "o")
lines(fit3$mean, col = "green", type = "o")

legend("topleft", lty = 1, col = c(1, "blue", "red", "green"),
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)), pch = 1)

# The larger the Î± the greater the adjustment that takes place in the next 
# forecast in the direction of the previous data point; smaller Î± leads to less # adjustment and so the series of one-step within-sample forecasts is smoother.


# Holt's Linear trend method
# Holt (1957) extended simple exponential smoothing to allow forecasting of data with a trend.
# This method involves a forecast equation and two smoothing equations (one for the level and
# one for the trend)

# To initialise the method we set â0=y1 and b0=y2ây1.
# Alternatively we could  fit a linear trend to the first few observations and 
# use the estimates of the intercept and the slope as the intial values for the 
# level and the trend respectively.

# Example
air <- window(ausair, start = 1990, end = 2004)
fit1 <- holt(air, alpha = 0.8, beta = 0.2, initial = "simple", h = 5)
fit2 <- holt(air, alpha = 0.8, beta = 0.2, initial = "simple",
             exponential = TRUE, h =5)
# Result of first model
fit1$model$states
fitted(fit1)
fit3 <- holt(air, alpha = 0.8, beta = 0.3, h =5,
             damped = TRUE, initial = "simple")

plot(fit2, type = "o", ylab = "Air passengers in Australia (millions)", 
     xlab = "Year", fcol = "white", PI = FALSE )
lines(fitted(fit1), col = 'blue')
lines(fitted(fit2), col = 'red')
lines(fitted(fit3), col = 'green')
lines(fit1$mean, col ="blue", type = "o")
lines(fit2$mean, col ="red", type = "o")
lines(fit3$mean, col ="green", type = "o")
legend("topleft", lty = 1, col = c("black", "blue", "red", "green"), 
       legend = c("Data", "Holt's linear trend",
                  "Exponential trend", "Additive damped trend"))


# Damping trend method: This method in conjunction with holt's smoothing parameter
# (alpha and beta) it uses another smoothing paramter (phi) which lies b/w (0, 1).

# Sheep in Asia example illustration of all the mentioned mthod.
livestock2 <- window(livestock, start = 1970, end = 2000)
fit1 <- ses(livestock2)
fit2 <- holt(livestock2)
fit3 <- holt(livestock2,exponential=TRUE, initial = "simple")
fit4 <- holt(livestock2, damped = TRUE)
fit5 <- holt(livestock2, exponential = TRUE, damped = TRUE)

# Results for the first model:
fit1$model
accuracy(fit1)  # training set
accuracy(fit1, x = livestock)
plot(fit2$model$states)
plot(fit4$model$states)

plot(fit3, type = "o", main ="Livestock, sheep in Asia (millions)", 
     flwd = 1, PI = FALSE)
lines(window(livestock, start = 2001), type = "o")
lines(fit1$mean, col =2)
lines(fit2$mean, col =3)
lines(fit4$mean, col =5)
lines(fit5$mean, col =6)
legend("topleft", lty = 1, pch = 1, 
       col = 1:6, c("Data", "SES", "Holt's", "Exponential",
                    "Additive Damped", "Multiplicative Damped"), cex = .6)


# When data is affected by seasonlity, one of the method used to understand the phenomena
# is Holt's winter method which includes an additional smoothing parameter lamda for 
# seasonal component.

# Example 7.4 International tourist visitor nights in Australia
aus <- window(austourists, start = 2005)
fit1 <- hw(aus, seasonal = "additive")
fit2 <- hw(aus, seasonal = "multiplicative")
plot(fit2, ylab = "International vistor night in Australia (millions)", 
     xlab = "Year", PI = FALSE, type ="o", fcol = "white")
lines(fitted(fit1), col = "red", lty = 2)
lines(fitted(fit2), col = "green", lty = 2)
lines(fit1$mean, col = "red", type = "o")
lines(fit2$mean, col = "green", type = "o")
legend("topleft", col = c(1:3), lty = 1, pch = 1, cex = 0.6, 
       c("Data", "Holt's Winters' Additive", "Holt's Winters' Multiplicative"))

states <- cbind(fit1$model$states[, 1:3], fit2$model$states[, 1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states, xlab = "year")
fit1$mean
fitted(fit1)


# ******* 
# Updated codes of Hyndman book for chapter 7.2

fc <- holt(air, h =5)
fc2 <- holt(air, damped = TRUE, phi = 0.9, h =15) 

autoplot(air) +
autolayer(fc, series = "Holt's method", PI = FALSE)+
autolayer(fc2, series = "Damped Holt's method", PI = FALSE) +
ggtitle("Forecast from Holt's method") + xlab("Year") +ylab("Air passengers in Australia (millions)") +
guides(colour = guide_legend(title = "Forecast"))


autoplot(livestock) +
xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

# time series cross-validation to compare one step forecast accuracy of three methods
# i.e. SES, Holt's and Holt's Damped

e1 <- tsCV(livestock, ses, h = 1 )
e2 <- tsCV(livestock, holt, h = 1)
e3 <- tsCV(livestock, holt, damped = TRUE, h = 1)

mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE)


# ets function in R from package "forecast".
ets()
# ets function returns an object of class "ets". Many other R function designed 
# to make working with ets objects easy are as follows

coef() # returns all fitted parameters
accuracy() # returns accuracy measures computed on training data.
summary() # prints some summary information about the fitted model
c(autoplot(), plot()) # produce time plots of time series.
residuals() # returns the residuals from the estimated model.
fitted() # returns one-step forecast for the training data.
simulate() # will simulate future sample paths from the fitted model
forecast() # computes point forecasts and prediction intervals.

# exmples to show the usages of these functions
# Example: International tourist visitor nights in Australia

aust <- window(austourists, start = 2005)
fit <- ets(aust)
summary(fit)
# graphical representation of the estimated states over time
autoplot(fit)

cbind('Residuals' = residuals(fit), 
      'Forecast errors' = residuals(fit, type = 'response')) %>%
  autoplot(facet = TRUE) + xlab("Year") + ylab(" ")

fit %>% forecast(h =8) %>%
  autoplot() +
  ylab("International visitor night in Australia (millions)")


# Exerceise to pe practiced for chapeter 7

ses(pigs)

#***********************************************************************

# Chapter 8: ARIMA Models ####

# Arima Models
# stationary :- A stationary time serie is one whose properties does not depend
# on time at which the series is observed.
# Therefore, time series with trends or with seasonality, are not stationary

# a white nosie is stationary as it does not matters when you observe it,
#  it should look much the same at any point of time.
# A series with cyclic behaviour is also stationary

# So, in general a stationary time series will have no predictable patterns 
# in the long-term.
# One way to make a non-stationary time series stationary is to compute the
# difference between the consecutive observations which is called as differencing

# Transformation such as logrithms can help to stabilize the variance of a 
# time series.

# Auto correlation factor (ACF) plot is also useful in identifying non-stationary
# time series. 
# For a stationary time-series, the ACF will drop to zero relatively
# quickly while non-stationary data decreases slowly.  Also, the r1 is often large
# and postive for non-stationary  time series.


# seriesACF
Box.test(diff(goog200), lag = 10, type = "Ljung-Box")
# The null hypothesis assumed here is essentially a random walk or it is stationary
# If p-value < 0.05 we will fail to accept Null hypothesis and draw conlcusion
# saying time series is not stationary.

# differencing
diff()
# In practice, it is almost never necessary to go beyond second order-differences.
# Seasonal differencing is the difference between an observation and the previous 
# observation from the same season.

# if seasonally differenced data appear to be white noise, then the appropriate 
# model for the original data is yt = yt-m + et

# example shwoing transformation and differencing makes a time series stationary
cbind("Sales ($million)" = a10, 
      "Monthly log sales" = log(a10),
      "Annual change in log sales" = diff(log(a10), 12)) %>%
  autoplot(facets = TRUE)+
  xlab("Year") + ylab("")+
  ggtitle("Antidiabetic drug sales")



# sometimes it is necessary to take both a seasonal difference and a first
# difference to obtain stationary data.
cbind("Billion KWh" = usmelec,
      "Logs" = log(usmelec),
      "Seasonally\n differenced logs" = diff(log(usmelec), 12),
      "Doubly\n differenced logs" =  diff(diff(log(usmelec),12),1)) %>%
  autoplot(facets = TRUE)+
  xlab("Year") + ylab("")+
  ggtitle("Monthly US net electricity generation")

# when both seasonal and first differences are applied, it makes no difference 
# which is done first. However, if the data have a strong seasonal effect, then 
# it is always good to do seasonal difference first because the resulting series 
# will sometimes be stationary and will not require any further first difference.

# *********
# Statistical way to determine whether a series requires differencing or not is to use
# "unit root test" ####
# there are number of unit root test avilable, based on different assumptions.
# widely used method is Kwiatkowski-Philips-Schmidt-Shin (KPSS). In this test 
# Null hypothesis is that the data is stationary and we look for evidence that
# the null hypothesis is false. Consequently, small p-value (e.g. < 0.05)
# suggest that differencing is required as we fail to accept null hypothesis.

# ur.kpss() from "urca" packages can be used for this ####
library(urca)
goog %>% ur.kpss() %>% summary()
# the test - statistic for the goog data is much bigger than 1 % critical value,
# indicating the null hypothesis is rejected.

goog %>% diff() %>% ur.kpss() %>% summary()


# Auto-regressive AR(p) Model of order p.####
# In auto-regressive model, we forecast the variable of interest using a linear 
# combination of past values of the variable. The term auto regressive indicates
# that it is regression of the variable against itself.

# Moving average model ####
# A moving average model uses past forecast errors rather than past values of 
# forecast variable in the regression-like model.
#  yt = c + et + p1 et-1 + p2 et-2 + ....+ pn et-n where et is white noise.
# Moving average model should not be confused with moving average smoothing.
# former is used to forecast the future values where as smoothing is used 
# for estimation trend-cycle of past value.
# it is possible to write any stationary AR(p) model as an MA(infinity) process.

# Non-seasonal ARIMA models ####
#  if we combine the differencing with autoregressive and a moving average,
# we obtain a non-seasonal ARIMA model. ARIMA stands for AutoRegressive Integrated
# Moving Average. In this context the "integration" is the reverse of differencing 

# ARIMA(p,d,q) where p = degree of autoregressive, 
#                    d = degree of first differencing
#                    q = degree of moving average

# ARIMA(0,0,0) = white noise
# ARIMA(0,1,0) without constant = Random walk
# ARIMA(0,1,0) with constant = Random walk with drift

# Examples : US consumption expenditure
p <- autoplot(uschange[, "Consumption"])+
  xlab("Year") + ylab("Quaterly Percentage Change")
plotly::ggplotly(p)

# On visualzing data it looks free from seasonality. Selecting model
# automatically.
fit <- auto.arima(uschange[, "Consumption"], seasonal = FALSE)
# the auto.arima evaluates to ARIMA(2,0,2) model with coefficients of ar1 & ar2
# and ma1 & ma2.
# the constant c is determined by 
# c = mean * (1-(coefficients of ar1 + coefficients of ar1))
# and et is white noise with a standard deviation of square root of sigma^2

fit %>% forecast(h = 10) %>% autoplot(inlcude = 80)
# The auto.arima() function is useful, but anything automated can be a little dangerous, 

# if c = 0 and d=0 the long-term forecast will go to zero.
# if c = 0 and d=1 the long-term forecast will go to non-zero.
# if c = 0 and d=2 the long-term forecast will follow a straight line.
# if c != 0 and d=0 the long-term forecast will go to mean of data.
# if c != 0 and d=1 the long-term forecast will a straight line.
# if c != 0 and d=2 the long-term forecast will follow a quadratic trend.

# ACF and PACF
# ACF and clsoely related PACF helps in determining appropriate value for p & q.
# ACF plot shows the autocorrelations which measures the relationship between 
# yt & yt-k for different values of k.
# Note:- if yt & yt-1, then yt-1 & yt-2 must also be correlated. However, then
# yt & yt-2 might be correlated, simply because they both are connected to yt-1,
# rather than becasuse of any new information contained in yt-2 that could be used
# in forecasting yt.

# To overcome this problem, we can use partial autocorrelations. These measures
# the relationship between yt and yt-k after removing the effect of lags 1,2,3,..,k-1

# The partial autocorrelations have the same critical values of +- 1.96/sqrt(T)


ggAcf(uschange[, "Consumption"], main= "")
ggPacf(uschange[, "Consumption"], main = "")

# Key POINTs using ACF & PACF
# If the data are of th form ARIMA(p,d,0) and ARIMA(0,d,q) model then, ACF & PACF
# plots can be helpful in determining the value of p or q.
# If p & q are both positive, then plots do not help in finding suitable values of p & q.

# The data series may follow ARIMA(p,d,0) model, if ACF and PACF pots of differenced
# data show the following patterns.
  # 1. the ACF is expontenially decaying or sinusoidal;
  # 2. there is a significant spike at lag p in the PACF, but none beyond lag p.

# The data series may follow ARIMA(0,d,q) model, if ACF and PACF pots of differenced
# data show the following patterns.
  # 1. the PACF is expontenially decaying or sinusoidal;
  # 2. there is a significant spike at lag q in the ACF, but none beyond lag q.

# In the PACF plot of the US consumption change, there is significant spike at
# lag but none beyond that. So ARIMA(3,0,0) can be thought of a model
(fit2 <- Arima(uschange[, "Consumption"], order = c(3,0,0)))

(fit3 <- auto.arima(uschange[, "Consumption"], seasonal = FALSE,
                    stepwise = FALSE, approximation = FALSE))




# The porcedure included in the auto.arima and  to start time series


# Example: Seasonally adjusted elctrical equipment orders
# data to be considered is "elecequip"

# stl() is used for seasonal decompostion of time series by LOESS
elecequip %>% stl(s.window = "periodic") %>% seasadj() -> eeadj
autoplot(eeadj)

# the plot show there are sudden big drop in 2008-09 and it was due to global 
# economic environment. Otherwise there is nothing unusual about the time plot
# The data is non-stationary, as the series wanders up and down for long period.
# Will apply first differencing to the series 

eeadj %>% diff() %>% ggtsdisplay(main = "")
# the plot of this shows that the PACF continuous three difference line is 
# outside is the thersold limit suggesting AR(3) model.
# so initial model can be considered as ARIMA(3,1,0)
# we will fit other ARIMA model such as ARIMA(4,1,0), ARIMA(2,1,0), ARIMA(2,1,1)
# and many other to see which has minimum AICc.

# the ARIMA(3,1,1) has the slightly minimum AICc value than the other
(fit <- Arima(eeadj, order = c(3,1,1)))
# a time-series residuals are considere to be white noise if all autocorrelations
# are withing the thershold limits
checkresiduals(fit, plot = TRUE)
# this function gives the Ljung-Box test with p-value > 0.05 suggesting we fail
# to reject Ho which is residuals are white noise.

# forecast using above model
autoplot(forecast(fit))


# SEASONAL Arima models
# seasonal arima model contains the additional terms for seasonal part which is similar
# to the notation used for non-seasonal part i.e. ARIMA(p,d,q). For a seasonal data the
# notation is ARIMA(p,d,q) (P,D,Q)m where the capitial P represents order of 
# auto-regression, D for differencing required and Q for order of moving-average required
# and m shows the number of observation per year.

# The seasonal part of the model consists of terms that are similar to the non-seasonal
# components of the model, but involve backshifts of the seasonal period. For example, 
# an ARIMA(1,1,1)(1,1,1)4 model (without a constant) is for quarterly data (m=4)

# The modelling procedure for seasonal data is same as for non-seasonal data, except
# that we need to select the AR and MA terms as well as the non-seasonal components 
# of the model.
# An illustration to a Europenan quateraly retail trade data

autoplot(euretail) + ylab("Retail index") + xlab("Year")
# the data are not non-stationary, with some seasonality, therefore we should first
# take a seasonal difference. 
# seasonlly differenced data
euretail %>% diff(lag = 4) %>% ggtsdisplay()
# still the data appears to be non-stationary, so we take an additional first difference.
# double differenced data
euretail %>% diff(lag =4) %>% diff() %>% ggtsdisplay()

# in the ACF & PACF chart we can see that there is a significant spike at lag 1 of ACF,
# which suggest a non-seasonal MA(1) component, and the significant spike at lag 4 of ACF 
# suggest seasonal MA(1).

euretail %>%
  Arima(order = c(0,1,1), seasonal = c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()


# Chapter 9: Regression with ARIMA errors ####
autoplot(usconsumption[])

# Granger Casuality Test 








