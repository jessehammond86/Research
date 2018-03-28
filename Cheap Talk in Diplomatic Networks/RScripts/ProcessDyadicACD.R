###############################################################################
##
## ProcessMonadicACD.R
## 
## Purpose: Intakes *monad-year* data on civil conflicts. This is used to
##  subset data on conflict intervention - since intervention isn't possible
##  when there is *no conflict*, it is necessary to create a flag indicating
##  whether any ongoing civil conflict is present within a given state.
##
## Data timespan: 1995 - 2014 state-year data recording when a given state
##  is experiencing a violent civil conflict.
##
## Output: An .Rdata object containing a data.table with state-year
##  records indicating the presence of a civil conflict within a state.
##
## Output files: 
##  - yearly_civil_acd.Rdata
##
###############################################################################

rm(list=ls())

## Load a bunch of dependencies

if (!'pacman' %in% installed.packages()) install.packages('pacman')
if(!'phoenixNet' %in% installed.packages()) devtools::install_github('jrhammond/phoenixNet')

pacman::p_load(data.table, countrycode, tidyr)

## Set working directory

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  setwd('/Users/jesse/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/NetworkSignedComm')
}

###############################################################################
##
## Load in data: dyadic ACD
##
###############################################################################

yearly_civil_acd <- fread('./Data/ACD/ucdp-dyadic-1-2016.csv')


###############################################################################
##
## Subset ACD data by year, and process state-ID and other variables.
##
###############################################################################

yearly_civil_acd <- yearly_civil_acd[as.Date(StartDate2) >= as.Date('1995-01-01)') & TypeOfConflict == 3]

## Generate ISO-3c codes for states
yearly_civil_acd[, stateb := countrycode(
  GWNoA
  , origin = 'cown'
  , destination = 'iso3c'
  )
]

## Subset columns
yearly_civil_acd <- yearly_civil_acd[, .(Year, stateb)]
setnames(yearly_civil_acd, 'Year', 'year')

## De-duplicate: I am not interested in the NUMBER or TYPE of conflict ongoing
##  within a given country, just the PRESENCE of conflict.
yearly_civil_acd <- unique(yearly_civil_acd)

## Generate flag variable indicating conflict presence
yearly_civil_acd[, civil_conflict := 1]

###############################################################################
##
## Write list of yearly matrices to file.
##
###############################################################################

save(yearly_civil_acd, file = './Data/AnalysisData/yearly_civil_acd.Rdata')