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


########################################

rm(list = ls())
# Loading required package
req_pkg <- c("readxl", "officer")
pkg_avail_status <- req_pkg[!(req_pkg %in% installed.packages())]
if(length(pkg_avail_status)){
  install.packages(pkg_avail_status)
}


library(readxl)
library(officer)
options(java.parameters = "-Xmx1024m", scipen = 100000)
default_dir <- getwd()
working_dir <- ""
cost_comp_data_loc <- ""

# Activate function in current environment.
source(paste(working_dir,"COP_Data_Handling_Functions.R", sep = "/"))
source(paste(working_dir,'Functions_For_ICO_Category.R', sep = "/"))
source(paste(working_dir,'Quaterly_Graphs_Function.R', sep = "/"))
source(paste(working_dir,'Origin_Cost_Components_graph_function.R', sep = "/"))
source(paste(working_dir,'monthly_data_compile_plot_functions.R', sep = "/"))
source(paste(working_dir,'functions_for_farm_type_bar_chart.R', sep = "/"))
source(paste(working_dir,'fuctions_for_cop_area_chart.R', sep = "/"))

# Read powerpoint format file as a pptx doc object.
# doc <- pptx(template = paste(working_dir,"factbook_format.pptx", sep = "/"))
doc <- read_pptx(path = paste(working_dir,"factbook_format_new.pptx", sep = "/"))
# Read Data for cost component chart
origin_cost_comp_file_loc <- paste(cost_comp_data_loc,
                                   "Origin_Cost_Components.xlsx", sep= "/")
ICO_cost_component_file_loc <- paste(cost_comp_data_loc,
                                     "ICO_Cost_Components.xlsx", sep = "/")

# Slide numbers which needs to be updated
# slide_num_ico <- c(6:25) + 2 # For ICO slide number.
# slide_num_country <- c(28,29, 34:38, 43:45, 47:48, 53:55, 
#                        57:58, 63:65, 67:68, 73:75, 77:78, 
#                        83:85, 87:88, 93:95, 97:98, 103:107, 
#                        112:114, 116:117, 122:124, 126:127, 
#                        132:134, 136:137, 142:144, 146:147, 
#                        152:154, 156:157, 162:164, 166:167, 
#                        172:176, 181:183) + 2

# slide_num_ico <- c(8:27)
# slide_num_country <- c(30:33, 36:42, 45:47, 49:52, 55:57,
#                        59:62, 65:67, 69:72, 75:77, 79:82,
#                        85:87, 89:92, 95:97, 99:102, 105:111,
#                        114:116, 118:121, 124:126, 128:131,
#                        134:136, 138:141, 144:146, 148:151,
#                        154:156, 158:161, 164:166, 168:171,
#                        174:180, 183:185)

slide_num_ico <- c(8:27)
slide_num_country <- c(30:47, 49:57, 59:67, 69:77, 79:87, 
                       89:97, 99:116, 118:126, 128:136,
                       138:146, 148:156, 158:166, 168:185)

# create an object of three month file names in ascending order and month_year
# object in the format ("mmm-yy") should be corresponding to the file order.

# Read three months backed up data for Factbook charts
message(cat("Chosse back up files (3 months) in the ascending order i.e. old to new file.\n",
            "This is done in order to maintain the color sequence."))

three_months_file_name <- c(message("Choose 1st excel file:"),file.choose(),
                            message("Choose 2nd excel file:"),file.choose(),
                            message("Choose 3rd excel file:"),file.choose())
# three month name in the format mmm-yy and in the order of file location.
message(cat("Provide month name in the same order as of file name provide\n",
            "e.g. 'Jun-18' for June file"))
# mon_year <- readline("Give 3 months name for three files provided location: ")
# mon_year <- c("May-18", "Jun-18", "Jul-18")
mon_year <- c(readline("Month name for 1st file location? "), 
              readline("Month name for 2nd file location? "), 
              readline("Month name for 3rd file location? "))
# Provide month to select latest data.
message(cat("Select the latest month order as provided above for charts creation\n"))
mon_to_select <- readline("Which month to select for charts (from 1,2,3)? ")
mon_to_select <- as.numeric(mon_to_select)
COP_month <- mon_year[mon_to_select]

# Compile three months backed up data
all_data <- worksheets_compiled_data(three_months_file_name, mon_year)
# Get Country name with coffee type 
country_name_list <- names(all_data)
# Year for each country according to their on-going crop year
country_cy_list <- structure(list(Crop_Year = c(2019, 2019, 2019, 2019, 2019,
                                                2019,2019, 2019, 2019, 2019,
                                                2019, 2019, 2019, 2019, 2019, 2019)),
                             class = "data.frame", 
                             row.names = c("Brazil_Arabica","Brazil_Robusta",
                                           "China_Arabica", "Colombia_Arabica",
                                           "Ethiopia_Arabica","Guatemala_Arabica",
                                           "Honduras_Arabica", "India_Arabica",
                                           "India_Robusta", "Indonesia_Robusta",
                                           "Mexico_Arabica", "Peru_Arabica",
                                           "Thailand_Robusta","Uganda_Robusta",
                                           "Vietnam_Arabica", "Vietnam_Robusta"))
# show the default year selected for each country
message(cat("The default year selected for each country are: "))
print(country_cy_list)
year_update_status <- readline("Do you want to change the year? (yes or no): ")
if(toupper(year_update_status) == "YES"){
  message("Provide year for country")
  for(i in 1:nrow(country_cy_list)){
    display_country <- paste0("Year for ", rownames(country_cy_list)[i], " : ")
    country_cy_list[i,1] <- readline(display_country)
  }
}
# Reading each country year for crop year.
year <- as.numeric(country_cy_list[, "Crop_Year"])

# Saving pptx in the local drive.
if(!(dir.exists(working_dir)))
  working_dir <- default_dir
save_file_loc <- readline(paste("Do you want to save the file in the",
                                default_dir, "?", "[yes or no] "))
if(toupper(save_file_loc) == "YES"){
  working_dir <- default_dir
}

# For ICO type Charts ####
# **********************************************************************************
# *******************  Part 1. Categorization by ICO    ****************************

colombian_milds_list = c("China_Arabica", "Colombia_Arabica","Guatemala_Arabica",
                         "Honduras_Arabica",  "India_Arabica", "Mexico_Arabica",
                         "Peru_Arabica", "Vietnam_Arabica")

milds_req_columns <- c("Crop Year", "Farmer Size", "State (Province)",
                       "District", "Total cost (c/lb)", 
                       "Production (K Bag)", "Date", "Country")

other_milds_list = c("China_Arabica", "Guatemala_Arabica","Honduras_Arabica",
                     "India_Arabica", "Mexico_Arabica", "Peru_Arabica",
                     "Vietnam_Arabica")

unwashed_arabica_list <- c("Brazil_Arabica", "Ethiopia_Arabica")

Robusta_list <- c( "Brazil_Robusta", "India_Robusta", "Indonesia_Robusta",
                   "Thailand_Robusta", "Uganda_Robusta", "Vietnam_Robusta")

robusta_req_columns <- c("Crop Year", "Farmer Size", "State (Province)",
                         "District", "Total cost (USD/MT)", 
                         "Production (K Bag)", "Date", "Country")

# Provide the "all_data" compiled above to function "data_compilation_by_ICO" 
# in "list_type_data" argument.
Milds_Including_Colombia <- 
  data_compilation_by_ICO(list_type_data = all_data,
                          grouping_country_list = colombian_milds_list,
                          columns_to_select = milds_req_columns)
Milds_Including_Colombia[, "Date"] <- as.character(Milds_Including_Colombia[,"Date"])

Other_Milds <- data_compilation_by_ICO(list_type_data = all_data,
                                       grouping_country_list = other_milds_list,
                                       columns_to_select = milds_req_columns)
Other_Milds[, "Date"] <- as.character(Other_Milds[, "Date"])

Unwashed_Arabica <- 
  data_compilation_by_ICO(list_type_data = all_data,
                          grouping_country_list = unwashed_arabica_list,
                          columns_to_select = milds_req_columns)
Unwashed_Arabica[, "Date"] <- as.character(Unwashed_Arabica[,"Date"])

Robusta <- data_compilation_by_ICO(list_type_data = all_data,
                                   grouping_country_list =Robusta_list,
                                   columns_to_select = robusta_req_columns)
Robusta[, "Date"] <- as.character(Robusta[, "Date"])

ICO_data_list <- list(Milds_Including_Colombia, Other_Milds, Unwashed_Arabica, Robusta)
# Create ICO name as per ICO_data_list
ICO_category_name<- c("Milds_Including_Colombia", "Other_Milds",
                      "Unwashed_Arabica", "Robusta")
names(ICO_data_list) <- ICO_category_name

# Create ICO categorized data for monthly evolution chart (three months).
ICO_data_for_quaterly_chart <- lapply(seq_along(ICO_data_list), 
                                      function(x) filter(ICO_data_list[[x]], 
                                                         Date == COP_month))
names(ICO_data_for_quaterly_chart) <- ICO_category_name
cur_cy_for_ICO <- country_cy_list[country_name_list[1],]
cur_cy_for_ICO <- readline(
  "Enter current crop year for ICO type charts e.g 2018(18/19) : ")
cur_cy_for_ICO <- as.numeric(cur_cy_for_ICO)
nxt_cy_for_ICO <- readline(
  "Enter next crop year for ICO type charts e.g 2019(19/20) : ")
nxt_cy_for_ICO <- as.numeric(nxt_cy_for_ICO)
cur_cy_ICO_data_for_3_months <- lapply(seq_along(ICO_data_list),
                                       function(x) 
                                         three_months_data(ICO_data_list[[x]],
                                                           year = cur_cy_for_ICO))
nxt_cy_ICO_data_for_3_months <- lapply(seq_along(ICO_data_list),
                                       function(x) 
                                         three_months_data(ICO_data_list[[x]],
                                                           year = nxt_cy_for_ICO))

ICO_cost_component_data <- lapply(seq_along(ICO_category_name), 
                                  function(x) readxl::read_xlsx(ICO_cost_component_file_loc,
                                                                ICO_category_name[x])) 

# ************************ Quateraly & Monthly Evolution Charts **********************
#                     ****************  Cost Components Chart   **************
# Keep the order of total as per the ICO_data_list created above.
message("Preparing charts by ICO Categories\n")

tc_unit_name_ICO <- c(rep("Total cost (c/lb)", 3), 
                      "Total cost (USD/MT)")

j <- 1
for(i in 1:length(ICO_data_list)){
  # four quatered charts:
  # In this section for argument "function_name_to_call" use "FourQuarteredGraphs"
  quaterly_charts_to_ppt(ppt_doc = doc, year = cur_cy_for_ICO, 
                         data = ICO_data_for_quaterly_chart[[i]],  
                         tc_unit_name = tc_unit_name_ICO[i], 
                         title_name = ICO_category_name[i], 
                         function_name_to_call = "FourQuarteredGraphs", 
                         slide_num = slide_num_ico[j])
  j <- j+1
  # percentile charts: In this call "percentile_chart" function.
  quaterly_charts_to_ppt(ppt_doc = doc, year = cur_cy_for_ICO, 
                         data = ICO_data_for_quaterly_chart[[i]],  
                         tc_unit_name = tc_unit_name_ICO[i], 
                         title_name = ICO_category_name[i], 
                         function_name_to_call = "percentile_chart", 
                         slide_num = slide_num_ico[j])
  j <- j+1
  cost_component_chart_ppt(ppt_doc = doc, 
                           data = ICO_cost_component_data[[i]],
                           year = cur_cy_for_ICO, 
                           tbl_req_on_ch = FALSE,
                           title_name = ICO_category_name[i], 
                           slide_num = slide_num_ico[j])
  j <- j+1
  # Three months chart 
  monthly_evolution_chart_ppt(ppt_doc = doc, year = cur_cy_for_ICO, 
                              data = cur_cy_ICO_data_for_3_months[[i]],  
                              tc_unit_name = tc_unit_name_ICO[i], 
                              title_name = ICO_category_name[i], 
                              slide_num = slide_num_ico[j])
  j <- j+1
  monthly_evolution_chart_ppt(ppt_doc = doc, year = nxt_cy_for_ICO, 
                              data = nxt_cy_ICO_data_for_3_months[[i]],  
                              tc_unit_name = tc_unit_name_ICO[i], 
                              title_name = ICO_category_name[i], 
                              slide_num = slide_num_ico[j])
  j <- j+1
}

message("Charts prepared for ICO category\n")
message("....................................\n")
message("Creating Individual Country Charts....")
# ******************************************************************************
# *******************  Part 2. For Individual Countries ************************

# Data: For Origin cost component chart data####
origin_cost_data <- lapply(seq_along(country_name_list), 
                           function(x) readxl::read_xlsx(origin_cost_comp_file_loc,
                                                         country_name_list[x]))
# Data: Filtering data for four quatered graphs for individual countries ####
country_data_for_quaterly_chart <- lapply(seq_along(all_data), 
                                          function(x) filter(all_data[[x]], Date == COP_month))

for(i in 1:length(country_data_for_quaterly_chart)){
  country_data_for_quaterly_chart[[i]][, "Date"] <- 
    as.character(country_data_for_quaterly_chart[[i]][, "Date"])
}

# to restrict vietnam data to plot y-asis at the level of
# 2000 USD/MT
vietnam_rob_pos = grep("Vietnam_Robusta", country_name_list)
tot_cop_filter_val = 2000
vietnam_data_actu <- country_data_for_quaterly_chart[[vietnam_rob_pos]]
vietnam_data <- 
  vietnam_data_actu[vietnam_data_actu[["Total cost (USD/MT)"]] <= 
                      as.name(tot_cop_filter_val), ]

country_data_for_quaterly_chart[[vietnam_rob_pos]] <- vietnam_data

# Data: Create Data for monthly evolution chart (three months) country level ####
country_data_for_3_months <- lapply(seq_along(all_data), 
                                    function(x) three_months_data(all_data[[x]], 
                                                                  year = year[x]))

vietnam_data_3_months <- country_data_for_3_months[[vietnam_rob_pos]]
vietnam_data_3_months <- 
  vietnam_data_3_months[vietnam_data_3_months[["Total cost (USD/MT)"]] 
                        <= as.name(tot_cop_filter_val), ]
country_data_for_3_months[[vietnam_rob_pos]] <- vietnam_data_3_months

# Read total cost column name (local) for all country
total_cost_unit_local <- lapply(seq_along(all_data), 
                                function(x) 
                                  names(all_data[[x]])
                                [grep("Total cost",
                                      names(all_data[[x]]),
                                      ignore.case = TRUE)][1])
total_cost_unit_local <- unlist(total_cost_unit_local)

# Read total cost column name (ICO) for all country
total_cost_unit_std <- lapply(seq_along(all_data), 
                              function(x) 
                                names(all_data[[x]])
                              [grep("Total cost",
                                    names(all_data[[x]]),
                                    ignore.case = TRUE)][2])
total_cost_unit_std <- unlist(total_cost_unit_std)

# Read local farm gate price column name for all country
farm_gate_name_local <- lapply(seq_along(all_data), 
                               function(x) 
                                 names(all_data[[x]])
                               [grep("Farm gate|Farmgate",
                                     names(all_data[[x]]),
                                     ignore.case = TRUE)][1])
farm_gate_name_local <- unlist(farm_gate_name_local)

# Read standard farm gate price column name for all country
farm_gate_name_std <- lapply(seq_along(all_data), 
                             function(x) 
                               names(all_data[[x]])
                             [grep("Farm gate|Farmgate",
                                   names(all_data[[x]]),
                                   ignore.case = TRUE)][2])
farm_gate_name_std <- unlist(farm_gate_name_std)

# Read local FOB column name for all country
fob_name_local <- lapply(seq_along(all_data), 
                         function(x) 
                           names(all_data[[x]])
                         [grep("fob",
                               names(all_data[[x]]),
                               ignore.case = TRUE)][1])
fob_name_local <- unlist(fob_name_local)
# Read standar FOB column name for all country
fob_name_std <- lapply(seq_along(all_data), 
                       function(x) 
                         names(all_data[[x]])
                       [grep("fob",
                             names(all_data[[x]]),
                             ignore.case = TRUE)][2])
fob_name_std <- unlist(fob_name_std)
# Read local direct cost column name for all country.
direct_cst_name_lcl <- lapply(seq_along(all_data),
                              function(x) 
                                names(all_data[[x]])
                              [grep("Direct",
                                    names(all_data[[x]]))][1])
direct_cst_name_lcl <- unlist(direct_cst_name_lcl)
indirect_cst_name_lcl <- lapply(seq_along(all_data),
                                function(x) 
                                  names(all_data[[x]])
                                [grep("Indirect",
                                      names(all_data[[x]]))][1])
indirect_cst_name_lcl <- unlist(indirect_cst_name_lcl)
# Read standard direct cost column name for all country.
direct_cst_name_std <- lapply(seq_along(all_data),
                              function(x) 
                                names(all_data[[x]])
                              [grep("Direct",
                                    names(all_data[[x]]))][2])
direct_cst_name_std <- unlist(direct_cst_name_std)
indirect_cst_name_std <- lapply(seq_along(all_data),
                                function(x) 
                                  names(all_data[[x]])
                                [grep("Indirect",
                                      names(all_data[[x]]))][2])
indirect_cst_name_std <- unlist(indirect_cst_name_std)

# Data : Arrange data for bar chart ####
# this function creates a variable named "region_farm_type" for x-axis,
# argument "col_name_for_values" is for y axis and
# "column_name_for_cols_to_row" is for fill type.
x  <-   "region_farm_type"
x_axis  <- "Cummulative_Production" 
y  <-  "cop_type_value"
fill_type  <-  "cop_type_name"

data_bar_type_lcl <- lapply(seq_along(all_data), function(x)
  arrange_data_from_col_to_row(data = all_data[[x]],
                               year = year[x],
                               mon_name_to_select = COP_month,
                               cols_name_for_row = c(fob_name_local[x],
                                                     indirect_cst_name_lcl[x],
                                                     direct_cst_name_lcl[x]),
                               column_name_for_cols_to_row = "cop_type_name",
                               col_name_for_values = "cop_type_value"))

data_bar_type_std <- lapply(seq_along(all_data), function(x)
  arrange_data_from_col_to_row(data = all_data[[x]],
                               year = year[x],
                               mon_name_to_select = COP_month,
                               cols_name_for_row = c(fob_name_std[x],
                                                     indirect_cst_name_std[x],
                                                     direct_cst_name_std[x]),
                               column_name_for_cols_to_row = "cop_type_name",
                               col_name_for_values = "cop_type_value"))

# Calling COPFactbook_pptx function to create & return pptx object named doc
k = 1
for(i in 1:length(country_data_for_quaterly_chart)){
  quaterly_charts_to_ppt(ppt_doc = doc, year = year[i],
                         data = country_data_for_quaterly_chart[[i]],
                         tc_unit_name =  total_cost_unit_local[i],
                         title_name = country_name_list[i],
                         function_name_to_call = "FourQuarteredGraphs", 
                         slide_num = slide_num_country[k])
  k <- k+1
  quaterly_charts_to_ppt(ppt_doc = doc, year = year[i],
                         data = country_data_for_quaterly_chart[[i]],
                         tc_unit_name =  total_cost_unit_std[i],
                         title_name = country_name_list[i],
                         function_name_to_call = "FourQuarteredGraphs", 
                         slide_num = slide_num_country[k])
  k <- k+1
  # monthly_evolution_chart_ppt(ppt_doc = doc, year = year[i], 
  #                             data = country_data_for_3_months[[i]],
  #                             tc_unit_name = total_cost_unit_local[i],
  #                             farm_gate_name = farm_gate_name_local[i],
  #                             title_name = country_name_list[i], 
  #                             slide_num = slide_num_country[k])
  # k <- k+1
  # monthly_evolution_chart_ppt(ppt_doc = doc, year = year[i], 
  #                             data = country_data_for_3_months[[i]],
  #                             tc_unit_name = total_cost_unit_std[i],
  #                             farm_gate_name = farm_gate_name_std[i],
  #                             title_name = country_name_list[i], 
  #                             slide_num = slide_num_country[k])
  
  cop_type_bar_chart_to_ppt(ppt_doc = doc, data = data_bar_type_lcl[[i]],
                            year = year[i], x = x, 
                            y =  y, fill_type = fill_type,
                            country_name = country_name_list[i], 
                            slide_num = slide_num_country[k])
  k <- k+1
  cop_type_bar_chart_to_ppt(ppt_doc = doc, data = data_bar_type_std[[i]],
                            year = year[i], x = x, 
                            y =  y, fill_type = fill_type,
                            country_name = country_name_list[i], 
                            slide_num = slide_num_country[k])
  k <- k +1
  cop_area_chart_to_ppt(ppt_doc = doc, data = data_bar_type_lcl[[i]],
                        year = year[i], 
                        x_axis = x_axis, y = y, 
                        fill_type = fill_type, 
                        fgp_name = farm_gate_name_local[i],
                        fob_name = fob_name_local[i],
                        country_name = country_name_list[i],
                        slide_num = slide_num_country[k])
  k <- k +1
  cop_area_chart_to_ppt(ppt_doc = doc, data = data_bar_type_std[[i]],
                        year = year[i], 
                        x_axis = x_axis, y = y, 
                        fill_type = fill_type, 
                        fgp_name = farm_gate_name_std[i],
                        fob_name = fob_name_std[i],
                        country_name = country_name_list[i],
                        slide_num = slide_num_country[k])
  k <- k +1
  monthly_evolution_chart_ppt(ppt_doc = doc, year = year[i],
                              data = country_data_for_3_months[[i]],
                              tc_unit_name = total_cost_unit_local[i],
                              fgp_name = farm_gate_name_local[i],
                              fob_name = fob_name_local[i],
                              title_name = country_name_list[i],
                              slide_num = slide_num_country[k])
  k <- k+1
  monthly_evolution_chart_ppt(ppt_doc = doc, year = year[i],
                              data = country_data_for_3_months[[i]],
                              tc_unit_name = total_cost_unit_std[i],
                              fgp_name = farm_gate_name_std[i],
                              fob_name = fob_name_std[i],
                              title_name = country_name_list[i],
                              slide_num = slide_num_country[k])
  
  k <- k+1
  cost_component_chart_ppt(ppt_doc = doc, 
                           data = origin_cost_data[[i]],
                           year = year[i], 
                           tbl_req_on_ch = TRUE,
                           title_name = country_name_list[i], 
                           slide_num = slide_num_country[k])
  k <- k+1
}

# doc1 <- doc
slide_num_for_note <- c(177,178,183,184)
three_yrs = c(year[16]-1, year[16], year[16]+1)
three_cy = sapply(seq_along(three_yrs), function(x)
  paste(three_yrs[x], substr(three_yrs[x]+1, 3,4), sep = "-"))


note_realted_data <- vietnam_data %>%
  filter(`Crop Year` %in% three_cy) %>%
  filter(Rank != "NA") %>%
  group_by(`Crop Year`)%>%
  summarise(prod =  sum(`Production (K Bag)`, na.rm = TRUE))
tot_prod_tbl <- vietnam_data_actu %>%
  filter(`Crop Year` %in% three_cy) %>%
  filter(Rank != "NA") %>%
  group_by(`Crop Year`)%>%
  summarise(prod =  sum(`Production (K Bag)`, na.rm = TRUE))

note_realted_data[["prod_diff"]] <- 
  tot_prod_tbl[["prod"]]- note_realted_data[["prod"]]
note_realted_data[["prod_diff"]] <- round(note_realted_data[["prod_diff"]],0)
main_note <- "data considers production where Total COP<="
note <- paste(main_note, tot_cop_filter_val, "USD/MT")
note1 <- paste(note, " and left out production are:")
note2 <- paste(note_realted_data[[1]], note_realted_data[[3]], sep = "=", collapse = ", ")
final_note <- paste(note1, note2, sep = " ")
note3 <- paste(note_realted_data[[1]][2],
               note_realted_data[[3]][2],
               sep = " : ")
final_note1 <- paste(note1, note3, sep = " ")

for (i in 1:2) {
  doc <- ph_with_text(on_slide(doc, slide_num_for_note[i]),
                      str = final_note,
                      type = "body", 
                      index = 3)
}

for (i in 3:4) {
  doc <- ph_with_text(on_slide(doc, slide_num_for_note[i]),
                      str = final_note1,
                      type = "body",
                      index = 3)
}

message("Charts are prepared and being saved...")
# Saving pptx in the local drive.
factbook_month <- format(Sys.Date()-30, "%Y%m")
factbook_file_name <- paste(factbook_month, "COPFactbook.pptx", sep = "_")
print(doc, paste(working_dir, factbook_file_name, sep = "/"))

# remove all the objects created
rm(list = ls())







