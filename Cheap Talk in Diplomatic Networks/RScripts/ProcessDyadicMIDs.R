###############################################################################
##
## ProcessDyadicMIDs.R
## 
## Purpose: Intakes directed-dyad-year data on MID INITIATION and returns a
##  list of yearly MID-OCCURRENCE matrices.
##
## Data timespan: 1995 - 2001 undirected-dyad-year records (A -- B)
##
## Output: An .Rdata object containing a data.table with undirected-dyad-year
##  records indicating the presence of a MID between states A and B
##
## Output files: 
##  - yearly_mids.Rdata
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
  setwd('/Users/localadmin/Dropbox/Research/CheapTalk')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/Research/CheapTalk')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/Research/CheapTalk')
}

###############################################################################
##
## Load in data: dyadic MID records based on MID v4 data (1816-2010)
##
###############################################################################

# yearly_mids <- fread('./Data/MID/MIDDyadic_v3.10.csv')
yearly_mids <- fread('./Data/MID/MIDDyadic_comparison.csv')


###############################################################################
##
## Process state-ID and other variables.
##
###############################################################################

## Convert COWN codes to ISO3c
yearly_mids[, statea := countrycode(
  statea
  , origin = 'cown'
  , destination = 'iso3c'
  )
]

yearly_mids[, stateb := countrycode(
  stateb
  , origin = 'cown'
  , destination = 'iso3c'
)
]

## Subset by only MID-years
yearly_mids <- yearly_mids[dichmid == 1]

## Subset to 1995-2010 year range
yearly_mids <- yearly_mids[year > 1994 & year < 2011]

## Create indicator of any MID that year (all observations)
# yearly_mids[, mid_start := 1]

## Create indicator for highest level of intensity during MID
# yearly_mids[, mid_highest_intensity := max(HostlevA, HostlevB), by = DyMIDNum]

## Create DATE column with date of MID initiation
# yearly_mids[, mid_startdate := as.Date(paste(StYear, StMon, StDay, sep = '-'))]

## Subset columns
# yearly_mids <- yearly_mids[
#   , list(StYear, mid_startdate, statea, stateb
#          , mid_start, mid_highest_intensity)]
yearly_mids <- yearly_mids[, list(year, statea, stateb, dichmid)]
setnames(yearly_mids, 'dichmid', 'mid_start')
# ## Standardize names
# setnames(yearly_mids, 'StYear', 'year')


## Self-merge to create UNDIRECTED measure of MID occurrence
# merge_data <- copy(yearly_mids)
# setnames(merge_data, c('statea', 'stateb'), c('stateb', 'statea'))
# setcolorder(merge_data, c(1, 3, 2, 4))
# yearly_mids <- rbind(yearly_mids, merge_data)
# setkeyv(yearly_mids, c('year', 'statea', 'stateb'))
# yearly_mids <- unique(yearly_mids)


###############################################################################
##
## Write output data to file.
##
###############################################################################

save(yearly_mids, file = './Data/AnalysisData/yearly_mids.Rdata')
