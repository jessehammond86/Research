###############################################################################
##
## Processyearly_relevanceData.R
## 
## Purpose: Intakes a .CSV file (produced by EUgene) containing directed
##  dyadic records of politically relevant dyads: either share land borders,
##  or involve at least one major power.
##
## Data timespan: 1995 - 2001 undirected-dyad-year records (A -- B)
##
## Output: An .Rdata object containing a data.table with undirected-dyad-year
##  records indicating whether or not states A and B are politically relevant.
##
## Output files: 
##  - yearly_relevance.Rdata
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
  setwd('/Users/localadmin/Research/CheapTalk')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/Research/CheapTalk')
}

###############################################################################
##
## Load in data: yearly dyadic contiguity
##
###############################################################################

yearly_relevance <- fread('./Data/PoliticalRelevance/polrel_dyads.csv')


###############################################################################
##
## Subset: convert to undirected dyad format
##
###############################################################################

# yearly_relevance <- yearly_relevance[ccode1 > ccode2]

###############################################################################
##
## Convert state IDs to ISO3-C
##
###############################################################################

setnames(yearly_relevance, c('statea', 'stateb', 'year'))
setcolorder(yearly_relevance, c(3,1,2))

yearly_relevance[, statea := countrycode(
  statea
  , origin = 'cown'
  , destination = 'iso3c'
  )
]

yearly_relevance[, stateb := countrycode(
  stateb
  , origin = 'cown'
  , destination = 'iso3c'
)
]

###############################################################################
##
## Save output to file
##
###############################################################################

save(yearly_relevance, file = './Data/AnalysisData/yearly_relevance.Rdata')
