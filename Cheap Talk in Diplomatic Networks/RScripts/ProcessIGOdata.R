###############################################################################
##
## ProcessIGOData.R
## 
## Purpose: Reads and processes yearly IGO membership data.
## 
## Data timespan: 1995 - 2005 directed-dyad-year records (A -> B)
##
## Output: An .Rdata object containing a data.table with directed-dyad-year
##  records indicating the level of IGO membership overlap between states
##  A and B.
##
## Output file name: yearly_igo_shared.Rdata
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
## Load in data: State-level IGO membership by year, 1995-2005
##
###############################################################################

igodata <- fread('./Data/IGO/IGO_stateunit_v2.3.csv')
igodata <- igodata[year >= 1995]


###############################################################################
##
## Convert yearly IGO data to sociomatrix where element (i,j) corresponds to
##  the number of IGOs in which states i and j are both full or conditional
##  members. Save sociomatrices to a list object.
###############################################################################

yearly_igo_shared <- data.table(
  year = integer()
  , statea = character()
  , stateb = character()
  , igo_overlap = numeric()
)

igo_years <- unique(igodata$year)


for(this_year in 1:length(igo_years)){
  
  ## Take this year's data
  this_igodata <- igodata[year %in% igo_years[this_year]]
  this_igodata[, state := countrycode(
    this_igodata$ccode
    , origin = 'cown'
    , destination = 'iso3c'
  )]
  
  igo_states <- unique(this_igodata$state)
  
  ## Record only cases where state A is a member of IGO X
  igomat <- as.matrix(this_igodata[, 5:533, with = F])
  igomat[igomat < 1] <- 0
  
  ## Take raw/undirected measure of shared IGO membership
  igo_socmat <- (igomat %*% t(igomat))
  
  ## Convert back to edge list and append to output data object
  igo_out <- data.table(expand.grid(igo_states, igo_states))
  setnames(igo_out, c('statea', 'stateb'))
  
  igo_out[, igo_overlap := as.vector(igo_socmat)]
  igo_out[, year := igo_years[this_year]]
  setcolorder(igo_out, c(4, 1, 2, 3))
  
  yearly_igo_shared <- rbind(yearly_igo_shared, igo_out)
  
}


###############################################################################
##
## Save yearly IGO sociomatrices to file.
##
###############################################################################

save(yearly_igo_shared, file = './Data/AnalysisData/yearly_igo_shared.Rdata')

