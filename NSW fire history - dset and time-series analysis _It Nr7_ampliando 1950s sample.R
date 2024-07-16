

libraries <- c('tidyverse', 'lubridate', 'readxl', 'MARSS', "forecast", 'stats', 'gridExtra', 'feasts', 'stats', 'fable', 'plotly', 'tseries', 'urca', 'reshape2', 'formatR')

invisible(lapply(libraries, require , character.only = TRUE))


#library(tidyverse)
#library(readxl)
#library(lubridate)
#library(MARSS)
#library(forecast)
#library(stats)
#library(gridExtra)
#library(reshape2)
#library(tseries)
#library(urca)
#library(fable)
#library(feasts)
#library(plotly)


BasicSummary <- function(df, dgts = 3){
  
  ## ################################################################
  ## #
  ## # Create a basic summary of variables in the data frame df,
  ## # a data frame with one row for each column of df giving the
  ## # variable name, type, number of unique levels, the most
  ## # frequent level, its frequency and corresponding fraction of
  ## # records, the number of missing values and its corresponding
  ## # fraction of records
  ## #
  ## ################################################################
  
  m <- ncol(df)
  varNames <- colnames(df)
  varType <- vector("character",m)
  topLevel <- vector("character",m)
  topCount <- vector("numeric",m)
  missCount <- vector("numeric",m)
  levels <- vector("numeric", m)
  
  for (i in 1:m)
  {
    x <- df[,i]
    varType[i] <- class(x)
    xtab <- table(x, useNA = "ifany")
    levels[i] <- length(xtab)
    nums <- as.numeric(xtab)
    maxnum <- max(nums)
    topCount[i] <- maxnum
    maxIndex <- which.max(nums)
    lvls <- names(xtab)
    topLevel[i] <- lvls[maxIndex]
    missIndex <- which((is.na(x)) | (x == "") | (x == " "))
    missCount[i] <- length(missIndex)
  }
  
  n <- nrow(df)
  topFrac <- round(topCount/n, digits = dgts)
  missFrac <- round(missCount/n, digits = dgts)
  #
  
  summaryFrame <- data.frame(variable = varNames, type = varType,
                             levels = levels, topLevel = topLevel,
                             topCount = topCount, topFrac = topFrac,
                             missFreq = missCount, missFrac = missFrac)
  return(summaryFrame)
  
}




setwd("D:/Rfiles_D_Drive/Datasets to try/Oz bushfires/firenpwsfirehistory")
D:\R files - D Drive\Datasets to try\Oz bushfires\firenpwsfirehistory

###  NSW_fire_history_orig <- read_excel("NSW_fire_history_Excel_Output3.xlsx", 
###                   col_types = c("text", "text", "text",  "date", "date", "numeric", "numeric", "date"))


NSW_fire_history <- read_csv("NSW_fire_history_2024.csv")


## 1.Analize data from 1953 onwards  ---> too little data prior to this year

## DO NOT USE - has a lot of missing dates thus Areas will not be summed #########
## for (i in 1:length(nsw_test$EndDate))
##    {  if (is.na(nsw_test[i, 5]))   
##      {nsw_test[i,5] <- nsw_test[i,4]}
##    }
## nsw_test$Year <- format(nsw_test$EndDate, format = "%Y")
## nsw_test_sum <- nsw_test %>% group_by(Year) %>% summarise(sum(AreaHa))


## 2. So will filter by "Label" variable
## First filter rows for "wildfire"  (removing prescribed burns)
## then by year   ---> extract the year from the string & convert to numeric. and filter


NSW_fire_history2 <- NSW_fire_history %>% filter(grepl('[Wd]ild', Label)) 

NSW_fire_history2 <- NSW_fire_history2 %>% 
                     mutate(derived_year = (as.numeric(sub(".*(\\d{4}).*", "\\1", NSW_fire_history2$Label)) +1) )  %>%
                     filter (derived_year > 1944) 


Total_Area_burned_year <- NSW_fire_history2%>%
                     group_by(derived_year) %>%
                     summarize(TotalAreaHa = sum(AreaHa, na.rm = TRUE))


Nr_fires_year <- NSW_fire_history2%>%
                 group_by(derived_year) %>%
                 count(derived_year)


Total_Area_burned_year <- as.data.frame(Total_Area_burned_year)
Total_Area_burned_year$derived_year <- as.integer(Total_Area_burned_year$derived_year)
Total_Area_burned_year$TotalAreaHa <- as.integer(Total_Area_burned_year$TotalAreaHa)


Total_Area_burned_year_ts <- as_tsibble(Total_Area_burned_year, index = year, key = TotalAreaHa)

Nr_fires_year_ts <- as_tsibble(Nr_fires_year, index = derived_year, key = n)

Total_Area_burned_year <- Total_Area_burned_year %>% mutate(Total_Area_million_Ha = TotalAreaHa/1000000)
graphNr1 <- ggplot(Total_Area_burned_year, aes(x=derived_year, y=Total_Area_million_Ha)) + geom_line(color='red')

graphNr1

ggplotly(graphNr1)

### Nr_fires_year_ts <- as_tsibble(Nr_fires_year, index = year)

graphNr2 <- ggplot(Nr_fires_year, aes(x=derived_year, y=n)) + geom_line()

graphNr2

graph1plotly <- plot_ly(Total_Area_burned_year, x=Total_Area_burned_year$year, y=Total_Area_burned_year$TotalAreaHa, type = "scatter", mode='lines',
                        lab)

graph1plotly %>% layout(title = 'Plot Title', xaxis = list(title = 'Year'), font=t, plot_bgcolor = "#e5ecf6",
                    yaxis = list(title = 'Total Area Burned (Ha)') )

### Graph #1:  no trend, there could be ciclic.
### Graph #2:  slight updward trend (for Nr. of fires/per year)




### QUESTION #1:
### Which are the statistical tests that can be used to determine if there is statistically significant increase in the data trend?

library(readxl)
library(tseries)
library(Kendall)
library(forecast)


#### Using Nr. of fires/year data
Nr_fires_year_ts <- ts(Nr_fires_year$n, start = min(Nr_fires_year$derived_year), frequency = 1)

# ADF Test
adf_test <- adf.test(Nr_fires_year_ts)
adf_test
### p > 0.05 , hence the Data is non-stationary (there is a trend)    :)     #####


# Mann-Kendall Trend Test
mk_test <- Kendall(Nr_fires_year$derived_year, Nr_fires_year$n)
### tau = 0.465, 2-sided pvalue =< 2.22e-16
### p < 0.05  -> meaning we reject the null, so there is sufficient evidence to say there is a trend.


# Linear Regression Analysis
lm_model <- lm(n ~ derived_year, data = Nr_fires_year)
summary(lm_model)

# Plot the trend
ggplot(Nr_fires_year, aes(x = derived_year, y = n)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Number of Fires per Year with Linear Trend", x = "Year", y = "Number of Fires")
### with LM, there is a lineal regression shown that is statistically significant



# STL Decomposition 
decomp <- stl(Nr_fires_year_ts, t.window = 1, deltat(1))
plot(decomp)



### QUESTION #2
##  To determine if the number and size of fires in the period 1920-1957 are statistically significantly different from the period 1980-2017, 

# Load necessary libraries
library(tseries)
library(zoo)



# Filter data for the two periods
period1 <- NSW_fire_history2 %>% filter(derived_year >= 1945 & derived_year <= 1957)
period2 <- NSW_fire_history2 %>% filter(derived_year > 1980 & derived_year <= 2023)

# Calculate number of fires and total area affected per year for both periods
# fires_per_year_p1 <- period1 %>% group_by(year = year(derived_year)) %>% summarize(count = n(), total_area = sum(AreaHa, na.rm = TRUE))
# fires_per_year_p2 <- period2 %>% group_by(year = year(derived_year)) %>% summarize(count = n(), total_area = sum(AreaHa, na.rm = TRUE))

fires_per_year_p1 <- period1 %>% group_by(derived_year) %>% summarize(count = n(), total_area = sum(AreaHa, na.rm = TRUE))
fires_per_year_p2 <- period2 %>% group_by(derived_year) %>% summarize(count = n(), total_area = sum(AreaHa, na.rm = TRUE))


# Descriptive statistics
desc_p1_fires <- summary(fires_per_year_p1$count)
desc_p2_fires <- summary(fires_per_year_p2$count)
desc_p1_area <- summary(fires_per_year_p1$total_area)
desc_p2_area <- summary(fires_per_year_p2$total_area)

# Normality test
normality_p1_fires <- shapiro.test(fires_per_year_p1$count)
normality_p2_fires <- shapiro.test(fires_per_year_p2$count)
normality_p1_area <- shapiro.test(fires_per_year_p1$total_area)
normality_p2_area <- shapiro.test(fires_per_year_p2$total_area)

# Variance test
var_test_fires <- var.test(fires_per_year_p1$count, fires_per_year_p2$count)
var_test_area <- var.test(fires_per_year_p1$total_area, fires_per_year_p2$total_area)

# Mean comparison tests
if (normality_p1_fires$p.value > 0.05 & normality_p2_fires$p.value > 0.05) {
  if (var_test_fires$p.value > 0.05) {
    ttest_fires <- t.test(fires_per_year_p1$count, fires_per_year_p2$count, var.equal = TRUE)
  } else {
    ttest_fires <- t.test(fires_per_year_p1$count, fires_per_year_p2$count, var.equal = FALSE)
  }
} else {
  ttest_fires <- wilcox.test(fires_per_year_p1$count, fires_per_year_p2$count)
}

if (normality_p1_area$p.value > 0.05 & normality_p2_area$p.value > 0.05) {
  if (var_test_area$p.value > 0.05) {
    ttest_area <- t.test(fires_per_year_p1$total_area, fires_per_year_p2$total_area, var.equal = TRUE)
  } else {
    ttest_area <- t.test(fires_per_year_p1$total_area, fires_per_year_p2$total_area, var.equal = FALSE)
  }
} else {
  ttest_area <- wilcox.test(fires_per_year_p1$total_area, fires_per_year_p2$total_area)
}




# Results
list(
  desc_p1_fires = desc_p1_fires,
  desc_p2_fires = desc_p2_fires,
  desc_p1_area = desc_p1_area,
  desc_p2_area = desc_p2_area,
  normality_p1_fires = normality_p1_fires,
  normality_p2_fires = normality_p2_fires,
  normality_p1_area = normality_p1_area,
  normality_p2_area = normality_p2_area,
  var_test_fires = var_test_fires,
  var_test_area = var_test_area,
  ttest_fires = ttest_fires,
  ttest_area = ttest_area
)


### #1: Data for fires and incidence is NOT NORMAL
### #2: There is a statistically significant difference between # and size of fires, between the 2 periods.

### Wilcoxon rank-test: First of all, p-values are nos strictly defined as a measure of the effect size.
### If the p-value is lower the significance level (usually 0.05) then we can say that we have statistically 
### significant evidences to reject the null hypothesis, and thus to accept that the data are different in your case.


####### DATA STRUCTURE                    #######################################
##
##
##  THE DATASETS HAVE TO HAVE THIS FORMAT:
##  A.  YEAR  MONTH(numeric of character)   VALUE
##  B.  YEARMONTH (numeric)                 VALUE
##  C.  YEAR                                VALUE
##
##
#################################################################################



#################################################################################
####### STATISTICAL ANALYSIS               ######################################

# 1. PLOT TO LOOK FOR TREND AND SEASONALITY    (https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-decomposition-of-time-series.html)
# 2. PERFORM AUGMENTED DICKEY FULLER TEST (ADF) in R to check if time-series is STATIONARY OR NOT
# 3. IF NOT STATIONARY, DIFFERENTIATE BY:
#        3.1. USING DIFF() function in R  (https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-differencing-to-remove-a-trend-or-seasonal-effects.html)
#        3.2 REMOVE 'NA' VALUES TO SEE IF THIS MAKES THE DIFFERENCE
#        3.3 SUBSTRACT TIME-SERIES FROM ITSELF, WITH A LAG OF 1 DAY.
#
# 4. ONCE STATIONARY, THEN WE CAN PROCEED WITH ARIMA, SARIMA OR OTHER METHOD:
#       CHAPTER 5 Box-Jenkins method: https://nwfsc-timeseries.github.io/atsa-labs/chap-boxjenkins-.html    
#       CHAPTER 6 Univariate state-space models:https://nwfsc-timeseries.github.io/atsa-labs/chap-boxjenkins-.html
#       (which have datasets to use as practice)
#       https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-autoregressive-moving-average-arma-models.html
#       https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-differencing-to-remove-a-trend-or-seasonal-effects.html
#     



###  1: we can't aggregate by month  --as there are many NAs in the START DATE Variable
###  So we can only aggregate by YEAR


Nr_fires_year_disag <- Nr_fires_year %>% ungroup()
Nr_fires_year_ts <- as_tsibble(Nr_fires_year_disag, index = derived_year)

fit <- Nr_fires_year_ts %>%
        model(auto = ARIMA(n, stepwise = FALSE, approx = FALSE)   )

fit |> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")


fit |>
  select(auto) |>
  gg_tsresiduals()


fitplot <- fit |>
  forecast(h=5) |>
  mutate(n = distributional::dist_truncated(n, 0))  %>%
  filter(.model=='auto') |>
  autoplot(Nr_fires_year_ts) + labs(title="Number of fires per year - NSW", x="Year") 
  

fitplot <- fitplot + theme(
    panel.background = element_rect(fill = "navajowhite2",
                                    colour = "darkorange",
                                    size = 0.5, linetype = "solid"),
      )



ggplotly(fitplot)


#### Forecasting AREA BURNT  --- doesn't really work -  the forecast is crap

Total_Area_burned_year_disag <- Total_Area_burned_year %>% ungroup()
Total_Area_burned_year_ts <- as_tsibble(Total_Area_burned_year_disag, index = derived_year)

area_fit <- Total_Area_burned_year_ts %>%
  model(auto = ARIMA(Total_Area_million_Ha, stepwise = FALSE, approx = FALSE)   )

area_fit |> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")


area_fit |>
  select(auto) |>
  gg_tsresiduals()


area_fit |>
  forecast(h=5) |>
  mutate(Total_Area_million_Ha = distributional::dist_truncated(Total_Area_million_Ha, 0))  %>%
  filter(.model=='auto') |>
  autoplot(Total_Area_burned_year_ts) + labs(title="Area burnt per year - NSW", x="Year") + theme_economist()





#### the ARIMA Modelling alone is shit   ####

### if we do Seasonal ARIMA by manual selection:

fit_manual <- Nr_fires_year_ts %>%
  model(manual014 = ARIMA(n ~ pdq(0,1,4), stepwise = FALSE, approx = FALSE)   )


fit_manual |>
  forecast(h=5) |>
  filter(.model=='manual014') |>
  autoplot(Nr_fires_year_ts)


fit_manual_plot <- fit_manual
ggplotly(fit_manual_plot)

### Interpretation: Prediction and prediction intervals are better (though far off still)

