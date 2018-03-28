###############################################################################
##
## ProcessYearlyControls.R
## 
## Purpose: Intakes a CSV with yearly controls data and subsets it by
##  year and column to return a set of undirected-dyad-year data from 1995-2010.
##
## Data timespan: 1995 - 2010 undirected-dyad-year records (A -- B)
##
## Output: An .Rdata object containing a data.table with undirected-dyad-year
##  records indicating several values:
##  - Joint democracy (0/1, undirected)
##  - Logged geographic distance (continuous, undirected)
##  - Capability ratio (continuous 0-1, undirected - strongest-to-weakest ratio)
##
## Output files: 
##  - yearly_controls.Rdata
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

yearly_controls <- fread('./Data/yearlycontrols/yearlycontrols.csv')


###############################################################################
##
## Subset yearly controls data
##
###############################################################################

## Subset by year and variable
yearly_controls <- yearly_controls[
  year >= 1995
  , list(year, statea, stateb, joindem, relcap, logdist)]

## Convert state IDs to ISO-3c
yearly_controls[, statea := countrycode(
  statea
  , origin = 'cown'
  , destination = 'iso3c'
)]

yearly_controls[, stateb := countrycode(
  stateb
  , origin = 'cown'
  , destination = 'iso3c'
)]

setkeyv(yearly_controls, c('year', 'statea', 'stateb'))


###############################################################################
##
## Write yearly controls data to file
##
###############################################################################

save(yearly_controls, file = './Data/AnalysisData/yearly_controls.Rdata')


