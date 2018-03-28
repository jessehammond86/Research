###############################################################################
##
## ProcessExternalSupport.R
## 
## Purpose: Intakes directed-dyad-year data on armed conflicts and outputs
##  yearly matrices of conflict.
##
## Data timespan: 1995 - 2002 directed-dyad-year records (A -> B)
##
## Output: An .Rdata object containing a data.table with directed-dyad-year
##  records of intervention by state A in state B's internal conflict.
##
## Output files: 
##  - yearly_intervention.Rdata
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
## Load in data: Disaggregated external support
##
###############################################################################

yearly_intervention <- fread('./Data/ExternalSupport/extsup_large.csv')


###############################################################################
##
## Load in data: UCDP actors because they're too special to use COWN
##
###############################################################################

ucdp_actors <- fread('./Data/UCDPActors/ucdp-actor-22-2015.csv')


###############################################################################
##
## Subset data by time, and process state ID columns and intervention type
##
###############################################################################

yearly_intervention <- yearly_intervention[ywp_year >= 1995]

## Reformat country codes.
## NOTE: here the *focal* state is country B, as it is the entity RECEIVING
##  intervention from a third party.
yearly_intervention[, stateb := countrycode(
  locationid1
  , origin = 'cown'
  , destination = 'iso3c'
  )
]

########################################
## IMPORTANT CHANGE:
##  FOR SOME REASON, GERMANY IS CODED AS COWN 260, IT SHOULD BE 255.
##  I AM MANUALLY RECODING IT TO 255 SO IT WILL PROPERLY CONVERT FROM
##  COWN TO ISO3C.
########################################
yearly_intervention[external_nameid == 260, external_nameid := 255]

yearly_intervention[, statea := countrycode(
  external_nameid
  , origin = 'cown'
  , destination = 'iso3c'
  )
]

## Keep only cases where external supporting actor is a state
yearly_intervention <- yearly_intervention[!is.na(statea)]

## Did yearly_intervention go to state government or insurgent group?
yearly_intervention[, support_state := 1L]
yearly_intervention[actorID != locationid1, support_state := -1L]

## Subset data columns
yearly_intervention <- yearly_intervention[, list(ywp_year, statea, stateb, support_state)]
setnames(yearly_intervention, 'ywp_year', 'year')


###############################################################################
##
## Save list object to file.
##
###############################################################################

save(yearly_intervention, file = './Data/AnalysisData/yearly_intervention.Rdata')
