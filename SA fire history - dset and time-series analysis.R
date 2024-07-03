
library(tidyverse)
library(readxl)
library(lubridate)
library(MARSS)
library(forecast)
library(stats)

library(gridExtra)
library(reshape2)
library(tseries)
library(urca)
library(forecast)



############  SA data   #####################
SA_fire_history_Excel_Output <- read_excel("D:/R files - D Drive/Datasets to try/Oz bushfires/firehistory_SA/FIREMGT_FireHistory_shp/SA_fire_history_Excel_Output.xlsx", 
                                           col_types = c("numeric", "text", "text", "date", "text", "numeric", "text", "numeric", "text", "numeric", "numeric", 
                                                        "numeric", "text", "numeric", "numeric", "numeric"))


## 5908 rows, 16 variables
## na_count
INCIDENTNU
0
INCIDENTNA
3732
INCIDENTTY
0
FIREDATE
0
FINANCIALY
0
FIREYEAR
0
SEASON
0
DATERELIAB
0
IMAGEINFOR
3532
FEATURESOU
0
CAPTUREMET
0
CAPTURESOU
0
COMMENTS
2014
HECTARES
0
SHAPE_Leng
0
SHAPE_Area
0

## Create Test dataset ##
SA_test <- SA_fire_history_Excel_Output

## SA1. Remove prescribed burns
SA_test <- filter(SA_fire_history_Excel_Output, INCIDENTTY != "Prescribed Burn")
## 4748 rows

## extract Year & Month from FIREDATE posixct variable)
SA_test <- SA_test %>% mutate(mes=month(FIREDATE))

## round decimals for HECTARES
SA_test$HECTARES <- round(SA_test$HECTARES, 0)
## start from 1953 onwards  (too little data prior to this)
SA_summary <- SA_test %>% filter(FIREYEAR > 1952) %>% group_by(FIREYEAR, mes) %>% summarise(sum(HECTARES))


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



lubridate::mdy(date_example) %>% lubridate::month(label = TRUE, abbr = FALSE)

SA_fires_ts <- ts(data = SA_summary$`sum(HECTARES)`, start = c(1953))

plot.ts(SA_fires_ts, ylab = expression(paste("Hectares")))


### Augmented Dickey-Fuller Test
adf.test(SA_fires_ts)

# data:  SA_fires_ts
# Dickey-Fuller = -3.3854, Lag order = 4, p-value = 0.06558
# alternative hypothesis: stationary

## The dataset set is NON-STATIONARY only by a minimum margin.


########### DIFFERENCING   ##################
SAfires_d1 <- diff(SA_fires_ts, differences = 1)
plot(SAfires_d1, ylab = expression(paste("delta hectares")))
SAfires_d1d12 <- diff(SAfires_d1, lag = 12)
plot(SAfires_d1d12, ylab = expression(paste("delta hectares")))

