###############################################################################
##
## ProcessPolityData.R
## 
## Purpose: Intakes a CSV with yearly POLITY IV data and converts it to
##  dyadic-distance format (absolute distance between POLITY scores).
##
## Data timespan: 1995 - 2015 undirected-dyad-year records (A -- B)
##
## Output: An .Rdata object containing a data.table with directed-dyad-year
##  records indicating the absolute difference between state A's and B's
##  POLITY IV (2) scores.
##  NOTE: Country-years where POLITY scores are unavailable due to foreign
##    intervention (mostly Iraq/Afghanistan/Lebanon) are set to 0.
##
## Output files: 
##  - yearly_polity_distance.Rdata
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
## Load in data: yearly POLITY IV data
##
###############################################################################

polity <- fread('./Data/PolityIV/p4v2015.csv')

###############################################################################
##
## Subset: only country-years after 1995.
##
###############################################################################

polity <- polity[year >= 1995]

#### There are some mismatches in COWN country code. Fix manually.
## Ethiopia
polity[ccode == 529, ccode := 530]

## Montenegro
polity[ccode == 348, ccode := 341]

## Yugoslavia / Serbia & Montenegro / Serbia
polity[ccode %in% c(342, 347), ccode := 345]

## South Sudan
polity[ccode == 525, ccode := 626]

## Vietnam
polity[ccode == 818, ccode := 816]


polity[, state := countrycode(
  ccode
  , origin = 'cown'
  , destination = 'iso3c'
  )
]


###############################################################################
##
## Replace years that are MISSING due to FOREIGN INTERVENTION - Set to 0.
##  THIS IS AN EXPLICIT ASSUMPTION ON MY PART. I AM DOING THIS JUST SO THESE
##  OBSERVATIONS ARE NOT DROPPED FROM THE ANALYSIS.
## This largely effects Afghanistan, Bosnia, Iraq, and Lebanon.
##
###############################################################################

polity[is.na(polity2), polity2 := 0]


###############################################################################
##
## Extract POLITY2 scores by year, and convert to dyadic-distance format.
##
###############################################################################

yearly_politydiff <- data.table(
  year = integer()
  , statea = character()
  , stateb = character()
  , polity_diff = integer()
)
polity_years <- 1995:2015

for(this_year in 1:length(polity_years)){
  
  ## Extract this year's data
  this_polity <- polity[
    year %in% polity_years[this_year]
    , list(state, polity2)
    ]
  
  ## Ensure non-duplication
  this_polity <- this_polity[!duplicated(state)]
  
  ## Convert to dyadic format by merging with edgelist of all unique nodes
  state_dyads <- data.table(expand.grid(this_polity$state, this_polity$state))
  setnames(state_dyads, c('statea', 'stateb'))
  
  state_dyads <- merge(
    state_dyads, this_polity
    , by.x = 'statea', by.y = 'state'
    )
  
  state_dyads <- merge(
    state_dyads, this_polity
    , by.x = 'stateb', by.y = 'state'
  )
  
  ## Find absolute polity score difference and insert year variable
  state_dyads[, polity_diff := abs(polity2.x - polity2.y)]
  state_dyads[, year := polity_years[this_year]]
  state_dyads <- state_dyads[, list(year, statea, stateb, polity_diff)]
  
  ## Add to output object
  yearly_politydiff <- rbind(yearly_politydiff, state_dyads)
  
}

###############################################################################
##
## Write output to file
##
###############################################################################

save(yearly_politydiff
     , file = './Data/AnalysisData/yearly_politydiff.Rdata'
     )