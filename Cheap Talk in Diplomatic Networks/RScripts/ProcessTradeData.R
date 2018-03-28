###############################################################################
##
## ProcessYearlyTrade.R
## 
## Purpose: Intakes a CSV with yearly dyadic trade data and subsets it by
##  year and column to return a set of undirected-dyad-year data from 1995-2010.
##  Each dyad-year's value is the MEAN trade flow 
##
## Data timespan: 1995 - 2009 undirected-dyad-year records (A -> B)
##
## Output: An .Rdata object containing a data.table with undirected-dyad-year
##  records indicating mean trade flows from states A to B
##
## Output files: 
##  - yearly_trade.Rdata
##
###############################################################################

rm(list=ls())

## Load a bunch of dependencies

if (!'pacman' %in% installed.packages()) install.packages('pacman')
if(!'phoenixNet' %in% installed.packages()) devtools::install_github('jrhammond/phoenixNet')

pacman::p_load(data.table, countrycode)

## Set working directory

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  # setwd('/Users/jesse/Dropbox/NetworkSignedComm')
  setwd('/Users/localadmin/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/NetworkSignedComm')
}

###############################################################################
##
## Load in data: yearly controls
##
###############################################################################

yearly_trade <- fread('./Data/Trade/dyadic_trade_3.0.csv')


###############################################################################
##
## Subset yearly data, convert state IDs, and restructure as directed dyads.
##
###############################################################################

## Convert state IDs to ISO-3c
yearly_trade[, statea := countrycode(
  ccode1
  , origin = 'cown'
  , destination = 'iso3c'
)]

yearly_trade[, stateb := countrycode(
  ccode2
  , origin = 'cown'
  , destination = 'iso3c'
)]

## Subset by year and variable
yearly_trade <- yearly_trade[
  year >= 1995
  , list(year, statea, stateb, flow1, flow2)]


## Convert missing (-9) to missing (NA)
yearly_trade[flow1 == -9, flow1 := NA]
yearly_trade[flow2 == -9, flow2 := NA]


## Convert to undirected-dyad format: take MEAN trade flow
yearly_trade[, trade_flow := rowMeans(
  yearly_trade[, list(flow1, flow2)], na.rm = T
  )
  ]



## Stack the data to include both A--B and B--A dyads (for later merging)
yearly_trade <- rbind(
  yearly_trade[, list(year, statea, stateb, trade_flow)]
  , yearly_trade[, list(year, stateb, statea, trade_flow)]
  , use.names = F
  )

setkeyv(yearly_trade, c('year', 'statea', 'stateb'))

###############################################################################
##
## Write yearly controls data to file
##
###############################################################################

save(yearly_trade, file = './Data/AnalysisData/yearly_trade.Rdata')


