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
library(vars)

var_function <- function(x, y, p = 2){
  x$quarter <- as.Date(as.yearqtr(x$quarter))
  input_data <- xts(x[, -1, with = F], order.by = x$quarter)

  exog <- scale(cbind(input_data[, c('CATH_CASRATIO')]
                      , lag(input_data[, c('CATH_CASRATIO')], 2)))
  exog[is.na(exog)] <- 0

  # Union kills - repub kills - state kills
  model1 <- VAR(y = scale(input_data[, y])
                , p = p, type = 'both')

  print(arch.test(model1, lags.single = 4, lags.multi = 4))
  return(model1)
}

summary(var_function(all_deaths, c('repub_civ_pct', 'union_indis_pct', 'REPUBKILLSCATH')))    # No ARCH
summary(var_function(all_deaths, c('repub_indis_pct', 'union_indis_pct')))  # ARCH :()
summary(var_function(all_deaths, c('repub_indis_pct', 'union_civ_pct')))    # No ARCH
summary(var_function(all_deaths, c('repub_civ_pct', 'union_indis_pct')))    # No ARCH
summary(var_function(all_deaths, c('repub_civ_pct', 'union_civ_pct')))      # No ARCH

summary(var_function(all_deaths, c('repub_indis_pct', 'union_civ_pct', 'repub_civ_pct')))    # No ARCH
summary(var_function(belfast_deaths, c('repub_indis_pct', 'union_indis_pct')))  # ARCH :()
summary(var_function(belfast_deaths, c('repub_indis_pct', 'union_civ_pct')))    # No ARCH
summary(var_function(belfast_deaths, c('repub_civ_pct', 'union_civ_pct')))      # No ARCH


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

cp_function <- function(x, y, sig.lvl = 0.01){

  ## Read in data and format for easy processing
  x$quarter <- as.Date(as.yearqtr(x$quarter))
  y_name <- y
  inputs <- data.table(x[,c('quarter', y), with = F])
  setnames(inputs, c('quarter', 'y'))

  ## Decompose quarterly time series to remove periodic component
  inputs$y <- decompose(ts(inputs$y, start = c(1970, 1), frequency = 4))$trend
  ## Optional: include random component
  # + decompose(ts(inputs$y, start = c(1970, 1), frequency = 4))$random

  ## Drop missing entries
  inputs <- inputs[complete.cases(inputs), ]

  ##### CP detection
  input_cp <- e.divisive(as.matrix(inputs[, 2, with = F])
                         , R = 999, min.size = 20, sig.lvl = sig.lvl)
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

  ## Print means and change points
  print(input_means[, c('mean', 'from', 'to2')])

  ## Drop final date entry - we don't need a Vline on the last recorded date
  input_means[nrow(input_means), 'to2'] <- NA

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

cp_function(all_deaths, 'REPUBKILLSCATH')       # Change: 1986-07-01
cp_function(belfast_deaths, 'REPUBKILLSCATH')   # Change: 1987-07-01
cp_function(rural_deaths, 'REPUBKILLSCATH')     # Change: 1981-10-01, 1989-10-01

cp_function(all_deaths, 'UNIONKILLSCATH')       # Change: 1979-01-01, 1993-04-01
cp_function(belfast_deaths, 'UNIONKILLSCATH')   # Change: 1978-04-01
cp_function(rural_deaths, 'UNIONKILLSCATH')     # Change: 1976-10-01, 1982-10-01


cp_function(all_deaths, 'REPUBKILLSBASES')      # Change: 1979-01-01, 1993-07-01
cp_function(belfast_deaths, 'REPUBKILLSBASES')  # Change: 1978-01-01, 1984-04-01

cp_function(all_deaths, 'UNIONKILLSBASES')      # Change: 1981-07-01
cp_function(belfast_deaths, 'UNIONKILLSBASES')  # Change: 1982-10-01, 1988-01-01
cp_function(rural_deaths, 'UNIONKILLSBASES')    # Change: 1976-10-01, 1982-01-01


cp_function(all_deaths, 'repub_civ_pct')        # Change: 1978-10-01, 1985-07-01
cp_function(belfast_deaths, 'repub_civ_pct')    # Change: 1977-10-01
cp_function(rural_deaths, 'repub_civ_pct')      # Change: 1978-10-01, 1986-04-01

cp_function(all_deaths, 'union_civ_pct')        # Change: 1977-04-01, 1986-04-01
cp_function(belfast_deaths, 'union_civ_pct')    # Change: 1977-04-01, 1987-10-01
cp_function(rural_deaths, 'union_civ_pct')      # Change: 1977-07-01, 1988-04-01


cp_function(all_deaths, 'repub_indis_pct')      # Change: 1975-10-01, 1982-04-01
cp_function(belfast_deaths, 'repub_indis_pct')  # Change: 1977-01-01, 1990-10-01
cp_function(rural_deaths, 'repub_indis_pct')    # Change: 1976-04-01, 1987-04-01

cp_function(all_deaths, 'union_indis_pct')      # Change: 1977-07-01
cp_function(belfast_deaths, 'union_indis_pct')  # Change: 1977-10-01
cp_function(rural_deaths, 'union_indis_pct')    # Change: 1977-01-01, 1992-04-01


cp_function(all_deaths, 'REPUBDISTRIB')         # Change: 1984-01-01, 1993-07-01
cp_function(rural_deaths, 'REPUBDISTRIB')       # Change: 1977-01-01, 1984-01-01
cp_function(belfast_deaths, 'REPUBDISTRIB')     # Change: 1977-01-01, 1982-10-01

cp_function(all_deaths, 'UNIONDISTRIB')         # Change: 1977-10-01, 1988-07-01
cp_function(rural_deaths, 'UNIONDISTRIB')       # Change: 1977-10-01, 1983-04-01

# No discernable changepoints at p < 0.01
cp_function(rural_deaths, 'REPUBKILLSBASES')    # Change: 1979-04-01, 1985-04-01 p < 0.05
cp_function(belfast_deaths, 'UNIONDISTRIB')     # Change: 1977-04-01, 1992-07-01 p < 0.05




cp_function(rural_deaths, '')







