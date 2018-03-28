###############################################################################
##
## MergeICEWSData.R
## 
## Purpose: Merge a bunch of data together:
##  - Monthly and yearly data on nodewise community and centrality values
##  - Yearly data on alliance existence and formation (1995 - 2012)
##  - Yearly data on dyadic MIDs (1995 - 2001)
##  - Yearly data on geographic distance (1995 - 2010(?))
##  - Yearly data on shared-democracy status (1995 - 2010)
##  - Yearly data on capability ratio (1995 - 2007)
##  - Yearly data on dyadic trade levels (1995 - 2009)
##  - Yearly data on shared IGO membership (1995 - 2005)
##
## Function: Intakes temporal network arrays and identifies network communities
##  based on positive and negative ties. Returns information about community
##  membership and network descriptive statistics.
##
## Output: A list object of length K where K is the number of time periods in
##  the data set. Each element contains a dyadic data set containing information
##  on whether a given pair of states share community membership, as well as
##  their similarity in their respective community networks.
##
## Output files: 
##  - monthly_community_data.Rdata: list containing membership and dyadic data
##    for monthly signed networks.
##  - yearly_community_data.Rdata: list containing membership and dyadic data
##    for yearly signed networks.
##
###############################################################################

rm(list=ls())

## Load a bunch of dependencies

if (!'pacman' %in% installed.packages()) install.packages('pacman')
if(!'phoenixNet' %in% installed.packages()) devtools::install_github('jrhammond/phoenixNet')
pacman::p_load(phoenixNet, igraph, parallel)

## Set working directory

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  setwd('/Users/localadmin/Dropbox/NetworkSignedComm')
  # setwd('/Users/jesse/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/NetworkSignedComm')
}

