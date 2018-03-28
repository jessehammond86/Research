###############################################################################
##
## MergeYearlyICEWSData.R
## 
## Purpose: Merge a bunch of data together:
##  - Yearly data on nodewise community and centrality values (1995 - 2015)
##  - Yearly data on alliance existence and formation (1995 - 2012)
##  - Yearly data on dyadic MIDs (1995 - 2001)
##  - Yearly data on conflict interventions (1995 - 2014)
##  - Yearly (5-year imputed) data on religious similarity (1995 - 2010)
##  - Yearly data on geographic distance (1995 - 2010(?))
##  - Yearly data on shared-democracy status (1995 - 2010)
##  - Yearly data on capability ratio (1995 - 2007)
##  - Yearly data on dyadic trade levels (1995 - 2009)
##  - Yearly data on shared IGO membership (1995 - 2005)
##
## Function: Merge dyadic yearly data with network variables based on yearly
##  ICEWS network data.
##
## Output: A very large data table at the dyad-year level. A single row holds
##  data on a single state dyad in a given year, indicating their level of
##  similarity along multiple dimensions.
##
## Output files: 
##  - yearly_analysis_data.Rdata: data.tbale containing membership and dyadic 
##    data for yearly signed networks.
##
###############################################################################

rm(list=ls())

## Load a bunch of dependencies

if (!'pacman' %in% installed.packages()) install.packages('pacman')
pacman::p_load(data.table, igraph)

## Set working directory
os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  # setwd('/Users/localadmin/Dropbox/NetworkSignedComm')
  setwd('/Users/jesse/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/NetworkSignedComm')
}

