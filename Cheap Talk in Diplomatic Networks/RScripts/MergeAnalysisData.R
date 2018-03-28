###############################################################################
##
## MergeICEWSData.R
## 
## Purpose: Merge a bunch of data together:
##  - Monthly and yearly data on nodewise community and centrality values
##  - Yearly data on alliance existence/formation/dissolution (1995 - 2012)
##  - Yearly data on dyadic MIDs (1995 - 2010)
##  - Yearly data on geographic distance (1995 - 2010)
##  - Yearly data on shared-democracy status (1995 - 2010)
##  - Yearly data on capability ratio (1995 - 2010)
##  - Yearly data on dyadic trade levels (1995 - 2009)
##  - Yearly data on shared IGO membership (1995 - 2005)
##  - Yearly (imputed) data on religious distance (1995 - 2015)
##  - Yearly data on international conflict intervention (1995 - 2009)
##  - Yearly data on political relevance (1995 - 2011)
##
## Function: Intakes a bunch of different yearly data and merges it all together
##  by year/statea/stateb ID variables.
##
## Output: A large data.table with yearly UNDIRECTED dyad entries for
##  255 states in the international system from 1995 to 2015. This includes
##  a LOT of missing data.
##
## Output files: 
##  - yearly_data.Rdata
##
###############################################################################

rm(list=ls())

## Load a bunch of dependencies

if (!'pacman' %in% installed.packages()) install.packages('pacman')
if(!'phoenixNet' %in% installed.packages()) devtools::install_github('jrhammond/phoenixNet')
pacman::p_load(data.table, igraph, interplot)

## Set working directory

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  setwd('/Users/jesse/Dropbox/NetworkSignedComm')
  # setwd('/Users/localadmin/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/Research/CheapTalk')
}

###############################################################################
##
## Load in a bunch of data
##
###############################################################################

##### Alliance data (1995 - 2012)
## Object: yearly_alliances (UNDIRECTED)
load('./Data/AnalysisData/yearly_alliances.Rdata')
setkeyv(yearly_alliances, c('year', 'statea', 'stateb'))
yearly_alliances <- unique(yearly_alliances)

##### Trade data (1995 - 2009)
## Object: yearly_trade (UNDIRECTED; MEAN TRADE FLOW)
load('./Data/AnalysisData/yearly_trade.Rdata')

##### IGO membership data (1995 - 2005)
## Object: yearly_igo_shared (UNDIRECTED)
load('./Data/AnalysisData/yearly_igo_shared.Rdata')

##### Religious distance data (1995 - 2015)
## Object: yearly_relig_distance (UNDIRECTED)
load('./Data/AnalysisData/yearly_relig_distance.Rdata')

##### POLITY IV regime differences (1995 - 2015)
## Object: yearly_politydiff (UNDIRECTED)
load('./Data/AnalysisData/yearly_politydiff.Rdata')

##### CINC ratio / geographic distance / shared-democracy status (1995 - 2010)
## Object: yearly_controls (UNDIRECTED)
load('./Data/AnalysisData/yearly_controls.Rdata')

##### Dyadic MIDs (1995 - 2001)
## Object: yearly_mids (UNDIRECTED; INCIDENCE, NOT INITIATION)
load('./Data/AnalysisData/yearly_mids.Rdata')

##### Dyadic interventions (1995 - 2009)
## Object: yearly_intervention
load('./Data/AnalysisData/yearly_intervention.Rdata')

##### State-year civil conflict (1995 - 2014)
## Object: yearly_civil_acd
load('./Data/AnalysisData/yearly_civil_acd.Rdata')

##### Yearly event-network data (1995 - 2015)
## Object 1: yearly_commdata_raw.Rdata (NOT USED)
## Object 2: yearly_commdata_gold.Rdata (DIRECTED; TAKE MEAN)
load('./Data/AppendixData/yearly_commdata_gold.Rdata')
load('./Data/AppendixData/yearly_arrays_gold.Rdata')

commtype <- 'gold'

###############################################################################
##
## Merge data by dyad-year, starting with yearly network data.
##
###############################################################################

## De-list network data to create single, large, directed-dyad-year data set.
years <- 1995:2015

yearly_fulldata <- list()

states <- unique(yearly_commdata_gold[[1]][,,1]$statea)
state_dyads <- data.table(
  expand.grid(
    states
    , states
    )
)
setnames(state_dyads, c('statea', 'stateb'))

for(this_year in 1:length(years)){
  yearly_fulldata[[this_year]] <- data.table(
    'year' = years[this_year]
    , state_dyads
    , 'weight_pos' = as.vector(yearly_arrays_gold[[this_year]][,,1])
    , 'weight_neg' = as.vector(yearly_arrays_gold[[this_year]][,,2])
  )
  
  yearly_commdata_gold[[this_year]]$year <- years[this_year]
  yearly_fulldata[[this_year]] <- merge(yearly_commdata_gold[[this_year]], yearly_fulldata[[this_year]], by = c('year', 'statea', 'stateb'), all.x = T)
}

yearly_data <- rbindlist(yearly_fulldata)

## Merge in alliance data
yearly_data <- merge(
  yearly_data
  , yearly_alliances
  , by = c('year', 'statea', 'stateb')
  , all.x = T
)
yearly_data[
  is.na(alliance_present) & year < 2012
  , alliance_present := 0
  ]
yearly_data[
  is.na(alliance_formed) & year < 2012
  , alliance_formed := 0
  ]
yearly_data[
  is.na(alliance_broken) & year < 2012
  , alliance_broken := 0
  ]


## Merge in trade data
yearly_data <- merge(
  yearly_data
  , yearly_trade
  , by = c('year', 'statea', 'stateb')
  , all.x = T
)


## Merge in IGO data
yearly_data <- merge(
  yearly_data
  , yearly_igo_shared
  , by = c('year', 'statea', 'stateb')
  , all.x = T
)


## Merge in religion data
yearly_data <- merge(
  yearly_data
  , yearly_relig_distance
  , by = c('year', 'statea', 'stateb')
  , all.x = T
)


## Merge in POLITY data
yearly_data <- merge(
  yearly_data
  , yearly_politydiff
  , by = c('year', 'statea', 'stateb')
  , all.x = T
)


## Merge in CINC/Distance/Joint-Dem data
yearly_data <- merge(
  yearly_data
  , yearly_controls
  , by = c('year', 'statea', 'stateb')
  , all.x = T
)


## Merge in MID data
yearly_data <- merge(
  yearly_data
  , yearly_mids
  , by = c('year', 'statea', 'stateb')
  , all.x = T
)
yearly_data[
  is.na(mid_start) & year < 2011
  , mid_start := 0
  ]


## Merge in intervention data
yearly_data <- merge(
  yearly_data
  , yearly_intervention
  , by = c('year', 'statea', 'stateb')
  , all.x = T
)


## Merge in state-civil-conflict-year data
yearly_data <- merge(
  yearly_data
  , yearly_civil_acd
  , by = c('year', 'stateb')
  , all.x = T
)



###############################################################################
##
## Save yearly merged data.
##
###############################################################################

save(yearly_data, file = './Data/AnalysisData/yearly_data.Rdata')

