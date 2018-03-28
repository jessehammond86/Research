###############################################################################
##
## ProcessAllianceData.R
## 
## Purpose: Reads and processes yearly alliance membership data.
##
## Data timespan: 1995 - 2012 directed-dyad-year records (A -> B)
##
## Output: An .Rdata object containing a data.table with directed-dyad-year
##  records indicating the presence of some form of alliance between states
##  A and B.
##
## Output file name: yearly_alliances.Rdata
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
  setwd('/Users/jesse/Dropbox/NetworkSignedComm')
  # setwd('/Users/localadmin/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/NetworkSignedComm')
}

###############################################################################
##
## Load in data: directed-dyad-year alliance records
##
###############################################################################

yearly_alliances <- fread('./Data/Alliance/alliance_v4.1_by_directed_yearly.csv')


###############################################################################
##
## Subset and process alliance data
##
###############################################################################

## Only data from 1995 onwards
yearly_alliances <- yearly_alliances[year >= 1995]

## Convert COWN codes to ISO3C
yearly_alliances[, statea := countrycode(ccode1, origin = 'cown', destination = 'iso3c')]
yearly_alliances[, stateb := countrycode(ccode2, origin = 'cown', destination = 'iso3c')]
yearly_alliances <- yearly_alliances[!is.na(statea) & !is.na(stateb)]
setkeyv(yearly_alliances, c('year', 'statea', 'stateb'))

## Identify years in which some alliance was present (leaving out ententes)
yearly_alliances[, alliance_present := 0]
yearly_alliances[defense == 1 | neutrality == 1 | nonaggression == 1
          , alliance_present := 1]

## Identify years in which new yearly_alliances were created
yearly_alliances[, alliance_formed := 0L]
yearly_alliances[dyad_st_year == year, alliance_formed := 1]

## Identify years in which yearly_alliances were annulled or removed
yearly_alliances[, alliance_broken := 0L]
yearly_alliances[dyad_end_year == year & right_censor == 0, alliance_broken := 1]

## Subset columns
yearly_alliances <- yearly_alliances[
  , list(year, statea, stateb
         , alliance_present, alliance_formed, alliance_broken)
  ]

###############################################################################
##
## Save output data to file
##
###############################################################################

save(yearly_alliances, file = './Data/AnalysisData/yearly_alliances.Rdata')


