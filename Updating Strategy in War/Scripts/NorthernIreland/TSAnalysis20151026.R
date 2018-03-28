rm(list = ls())
setwd('/Users/jesse/Dropbox/Dissertation/Data/NorthernIreland')
# setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland')
#setwd('/media/jesse/Files/Dropbox/Dissertation/Data/NorthernIreland/Census Data')
library(rgdal)
library(foreign)
library(data.table)
library(raster)
library(rgeos)
library(maptools)
library(spdep)
library(spatcounts)
library(pscl)
library(e1071)
library(reshape2)
library(psych)
library(TSA)
library(vars)
library(zoo)
library(lubridate)
library(xts)

## Read in data
deaths <- fread('GeoMonthlyDeaths.csv')

#################
##
## Generate and process a few more variables
##
#################
deaths$quarter <- as.Date(as.yearqtr(deaths$quarter))

input_data <- xts(deaths[, list(
  REPUB_CASRATIO, UNION_CASRATIO, STATE_CASRATIO
  , CATH_CASRATIO, PROT_CASRATIO
  , repub_indis_pct, union_indis_pct, state_indis_pct
  , repub_select_pct, union_select_pct, state_select_pct
  , repub_civ_pct, union_civ_pct, state_civ_pct
  , REPUBDISTRIB, UNIONDISTRIB, STATEDISTRIB)]
  , order.by = deaths$quarter
)


#############################################################################
## PRELIMINARY STEP: ARIMAX models to assess exogenous relationships
## These are not the most 'theoretically appropriate' models, but they may serve
## to indicate promising avenues for analysis.

#### REPUBLICAN ACTIONS
# Positive shock on CIVILIAN TARGETING from CASUALTY RATIOS
exog = cbind(lag(input_data$REPUB_CASRATIO),
             lag(input_data$REPUB_CASRATIO, 2))
exog[is.na(exog)] <- 0
armodel1 <- arima(input_data$repub_civ_pct, order = c(2,0,0), xreg = exog)
armodel1

#### UNIONIST ACTIONS
# Positive shock on INDISCRIMINATE WEAPONS USE from CASUALTY RATIOS
exog = cbind(lag(input_data$PROT_CASRATIO),
             lag(input_data$PROT_CASRATIO, 2))
exog[is.na(exog)] <- 0
armodel1 <- arima(input_data$union_indis_pct, order = c(2,0,0), xreg = exog)
armodel1


#################
##
## VAR models and diagnostics
##
#################
exog <- cbind(input_data[, c('CATH_CASRATIO')]
            , lag(input_data[, c('CATH_CASRATIO')], 2))
exog[is.na(exog)] <- 0

# Union indis - repub indis - union civ
model1 <- VAR(y = input_data[, c('union_indis_pct', 'repub_indis_pct', 'union_civ_pct')]
              , p = 3, type = 'both', exogen = exog)
summary(model1)

# 


