rm(list = ls())
# setwd('/Users/jesse/Dropbox/Dissertation/Data/NorthernIreland')
setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland')
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
library(forecast)

## Read in data
all_deaths <- fread('NI_quarter_deaths.csv')
belfast_deaths <- fread('belfast_quarter_deaths.csv')
rural_deaths <- fread('rural_quarter_deaths.csv')

#################
##
## Generate and process a few more variables
##
#################

## Nice function from Stackoverflow to deal with NA's
repNAmed <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
    x
  }  else {
    x[is.na(x)] <- names(which.max(table(x)))
    x
  }
}

replaceNaWithLatest <- function(dfIn, nameColNa = names(dfIn)[1]
){
  dtTest <- data.table(dfIn)
  setnames(dtTest, nameColNa, "colNa")
  dtTest[, segment := cumsum(!is.na(colNa))]
  dtTest[, colNa := colNa[1], by = "segment"]
  dtTest[, segment := NULL]
  setnames(dtTest, "colNa", nameColNa)
  return(dtTest)
}

for(name in names(all_deaths)){
  all_deaths <- replaceNaWithLatest(all_deaths, name)
  belfast_deaths <- replaceNaWithLatest(belfast_deaths, name)
  rural_deaths <- replaceNaWithLatest(rural_deaths, name)
}

all_deaths <- all_deaths[, lapply(.SD, repNAmed)]
belfast_deaths <- belfast_deaths[, lapply(.SD, repNAmed)]
rural_deaths <- rural_deaths[, lapply(.SD, repNAmed)]

arimax_function <- function(x){
  x$quarter <- as.Date(as.yearqtr(x$quarter))

  input_data <- xts(x[, list(
    REPUBKILLS, UNIONKILLS, STATEKILLS
    , REPUBDEATHS, UNIONDEATHS, STATEDEATHS
    , REPUB_CASRATIO, UNION_CASRATIO, STATE_CASRATIO
    , CATH_CASRATIO, PROT_CASRATIO
    , repub_indis_pct, union_indis_pct, state_indis_pct
    , repub_select_pct, union_select_pct, state_select_pct
    , repub_civ_pct, union_civ_pct, state_civ_pct
    , repub_mil_pct, union_mil_pct, state_mil_pct
    , REPUBKILLSBASES, UNIONKILLSBASES, STATEKILLSBASES
    , REPUBDISTRIB, UNIONDISTRIB, STATEDISTRIB)]
    , order.by = x$quarter
  )

  for(i in 1:ncol(input_data)){
    input_data[is.na(input_data[, i]), i] <- 0
  }

  #############################################################################
  ## PRELIMINARY STEP: ARIMAX models to assess exogenous relationships
  ## These are not the most 'theoretically appropriate' models, but they may serve
  ## to indicate promising avenues for analysis.

  #### REPUBLICAN ACTIONS
  exog = scale(cbind(lag(input_data$REPUB_CASRATIO),
                     lag(input_data$REPUB_CASRATIO, 2)))
  exog[is.na(exog)] <- 0

  # Increase in CIVILIAN TARGETING from CASUALTY RATIOS
  armodel1 <- arima(scale(input_data$repub_civ_pct), order = c(2,0,0), xreg = exog)
  print((armodel1))

  # Increase in DISTRIBUTION OF REPUBLICAN KILLS
  armodel2 <- arima(scale(input_data$REPUBDISTRIB), order = c(2,0,0), xreg = exog)
  print((armodel2))

  #### UNIONIST ACTIONS
  # Increase in INDISCRIMINATE WEAPONS USE from CASUALTY RATIOS
  armodel3 <- arima(scale(input_data$union_indis_pct), order = c(2,0,0), xreg = exog)
  print((armodel3))
}

arimax_function(all_deaths)
arimax_function(belfast_deaths)
arimax_function(rural_deaths)


#################
##
## VAR models and diagnostics
##
#################


var_function <- function(x){
  x$quarter <- as.Date(as.yearqtr(x$quarter))

  input_data <- xts(x[, list(
    REPUBKILLS, UNIONKILLS, STATEKILLS
    , REPUBDEATHS, UNIONDEATHS, STATEDEATHS
    , REPUB_CASRATIO, UNION_CASRATIO, STATE_CASRATIO
    , CATH_CASRATIO, PROT_CASRATIO
    , repub_indis_pct, union_indis_pct, state_indis_pct
    , repub_select_pct, union_select_pct, state_select_pct
    , repub_civ_pct, union_civ_pct, state_civ_pct
    , repub_mil_pct, union_mil_pct, state_mil_pct
    , REPUBKILLSBASES, UNIONKILLSBASES, STATEKILLSBASES
    , REPUBDISTRIB, UNIONDISTRIB, STATEDISTRIB)]
    , order.by = x$quarter
  )

  for(i in 1:ncol(input_data)){
    input_data[is.na(input_data[, i]), i] <- 0
  }

  exog <- scale(cbind(input_data[, c('CATH_CASRATIO')]
                      , lag(input_data[, c('CATH_CASRATIO')], 2)))
  exog[is.na(exog)] <- 0

  # Union kills - repub kills - state kills
  model1 <- VAR(y = scale(input_data[, c('UNIONKILLS', 'REPUBKILLS')])
                , p = 2, type = 'both')
  print(summary(model1))

  # Union indis - repub indis
  model2 <- VAR(y = scale(input_data[, c('union_indis_pct', 'repub_indis_pct')])
                , p = 2, type = 'both', exogen = exog)
  print(summary(model2))

  #
  model3 <- VAR(y = scale(input_data[, c('REPUBDEATHS', 'REPUBKILLSBASES')])
                , p = 2, type = 'both', exogen = exog)
  print(summary(model3))

  # Union civ - repub civ
  model3 <- VAR(y = scale(input_data[, c('REPUBKILLS', 'union_civ_pct', 'repub_civ_pct')])
                , p = 2, type = 'both', exogen = exog)
  print(summary(model3))
}

var_function(all_deaths)
var_function(belfast_deaths)
var_function(rural_deaths)



#####################################################################################
## SUPPLEMENTARY ANALYSIS: Is there evidence that PAL/ISR behavior changed after Defensive Shield?
#####################################################################################
library(changepoint)
library(bcp)
library(ecp)
library(ggplot2)

## Changepoint detection and plotting
# 1. Decompose time series of actor behavior and take 'trend' component
# 2. Use e.divisive change point analysis to find the major point(s) at which
#    actor behavior changes.
# 3. Use ggplot to show where changepoints occur, and regime-level means.

cp_function <- function(x, y){
  x$quarter <- as.Date(as.yearqtr(x$quarter))
  y_name <- y
  inputs <- data.table(x[,c('quarter', y), with = F])
  setnames(inputs, c('quarter', 'y'))

  inputs$y <- decompose(ts(inputs$y, start = c(1970, 1), frequency = 4))$trend 
  # + decompose(ts(inputs$y, start = c(1970, 1), frequency = 4))$random
  inputs$y[is.na(inputs$y)] <- 0
  ##### CP detection
  input_cp <- e.divisive(as.matrix(inputs[, 2, with = F]), R = 999)
  input_change_idx <- input_cp$estimates[c(2,3)]
  if(length(unique(input_cp$cluster)) == 1){
    stop('No changepoints detected.')
  }
  input_means <- data.frame('mean' = numeric(1)
                            , 'from' = structure(numeric(1), class = 'Date')
                            , 'to' = structure(numeric(1), class = 'Date')
                            , 'to2' = structure(numeric(1), class = 'Date'))
  idx <- c(input_change_idx, nrow(inputs))
  for(i in 1:length(idx)){
    i1 <- idx[i-1]
    if(length(i1) == 0){
      i1 <- 1
    }
    i2 <- idx[i]
    if(i2 > nrow(inputs)){
      i2 <- nrow(inputs)
    }
    input_means <- rbind(input_means, list(mean(inputs[i1:i2, y], na.rm = T)
                         , inputs[i1, quarter]
                         , inputs[, data.table::shift(quarter)][i2]
                         , inputs[i2, quarter]))
  }
  input_means <- input_means[-1, ]
  input_means <- input_means[complete.cases(input_means), ]
  print(input_means)
  ## Plot CP
  cp_plot <- ggplot() +
    geom_line(data = inputs, aes(quarter, y)) +
    geom_rect(data = input_means, aes(xmin = to, xmax = to2, ymin = -Inf, ymax = Inf)
              , alpha = 0.5, fill = c('blue')) +
    geom_segment(data = input_means, aes(x = from, xend = to
                     , y = mean, yend = mean), color = 'red') +
    xlab('Time') +
    # ylab('PAL indirect-fire attacks (%)') +
    theme(panel.background = element_blank()
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , axis.text=element_text(size=12)
          , axis.title=element_text(size=14))

  return(cp_plot)
}

# Interesting changes in 1978/79
cp_function(all_deaths, 'REPUBKILLSBASES')      # Change: 1978-10-01
cp_function(belfast_deaths, 'REPUBKILLSBASES')
cp_function(rural_deaths, 'REPUBKILLSBASES')

cp_function(all_deaths, 'repub_civ_pct')        # Change: 1978-01-01
cp_function(belfast_deaths, 'repub_civ_pct')    # Change: 1978-01-01
cp_function(rural_deaths, 'repub_civ_pct')      # Change: 1989-04-01


cp_function(all_deaths, 'union_indis_pct')      # Change: 1978-01-01
cp_function(belfast_deaths, 'union_indis_pct')  # Change: 1978-01-01
cp_function(rural_deaths, 'union_indis_pct')    # Change: 1978-01-01

# Interesting changes in 1990/91
cp_function(all_deaths, 'REPUBDISTRIB')         # Change: 1991-04-01
cp_function(rural_deaths, 'REPUBDISTRIB')       # Change: 1990-01-01

cp_function(all_deaths, 'repub_indis_pct')      # Change: 1991-04-01
cp_function(belfast_deaths, 'repub_indis_pct')  # Change: 1990-04-01
cp_function(rural_deaths, 'repub_indis_pct')    # Change: 1989-10-01

# Alternative changepoints
cp_function(belfast_deaths, 'REPUBDISTRIB')     # Change: 1984-04-01


cp_function(belfast_deaths, 'UNIONKILLSBASES')  # Change: 1980-01-01
cp_function(rural_deaths, 'UNIONKILLSBASES')    # Change: 1981-07-01

cp_function(all_deaths, 'UNIONDISTRIB')         # Change: 1977-07-01, 1987-01-01
cp_function(rural_deaths, 'UNIONDISTRIB')       # Change: 1978-01-01, 1988-04-01

# No discernable changepoints


cp_function(all_deaths, 'UNIONKILLSBASES')

cp_function(belfast_deaths, 'UNIONDISTRIB')

cp_function(all_deaths, 'union_civ_pct')
cp_function(belfast_deaths, 'union_civ_pct')


cp_function(rural_deaths, '')







