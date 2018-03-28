###############################################################################
##
## GetICEWSData.R
## 
## Purpose: downloads the available ICEWS data, cleans and processes the data,
##  and converts the dyadic event records to multi-layer temporal network
##  structures.
##
## NOTE: This function currently downloads two data sets:
##  1. ALL root-code network layers (20).
##  2. Pentaclass network layers (verbal/material conflict/cooperation)
##  The full root-code networks are gathered because later I'm going to recode 
##  these events with their CAMEO/Goldstein scores in order to capture more 
##  variation in cooperation and conflict.
##
## Function: Uses phoenixNet package to download and convert ICEWS data.
##
## Output: List objects containing monthly root-code temporal networks from
##  1995-01-01 through 2015-10-01. This list object is saved to disk.
##
## Output file names: 
##  eventcode_month_data.Rdata
##  rootcode_month_data.Rdata
##
###############################################################################

rm(list=ls())

## Load a bunch of dependencies

if (!'pacman' %in% installed.packages()) install.packages('pacman')
if(!'phoenixNet' %in% installed.packages()) devtools::install_github('jrhammond/phoenixNet')
pacman::p_load(phoenixNet, lubridate)

## Set working directory

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  setwd('/Users/localadmin/Dropbox/Research/CheapTalk')
  phoenixnet_dir <- '/Users/localadmin/Dropbox/Minerva'

} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkSignedComm')
  phoenixnet_dir <- '/media/jesse/Files/Dropbox/Minerva'

} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/NetworkSignedComm')
  phoenixnet_dir <- 'C:/Users/Jesse/Dropbox/Minerva'
}


###############################################################################
##
## Get MONTHLY data by root code and pentaclass.
##
###############################################################################

rootcode_month_data <- phoenix_net(
  start_date = 19950101
  , end_date = 20151001
  , level = 'rootcode'
  , phoenix_loc = paste0(phoenixnet_dir, '/phoenix')
  , icews_loc = paste0(phoenixnet_dir, '/icews')
  , codeset = 'all'
  , time_window = 'month'
  , tie_type = 'count'
  , update = F
  , sources = 'ICEWS')

pentaclass_month_data <- phoenix_net(
  start_date = 19950101
  , end_date = 20151001
  , level = 'pentaclass'
  , phoenix_loc = paste0(phoenixnet_dir, '/phoenix')
  , icews_loc = paste0(phoenixnet_dir, '/icews')
  , codeset = 'all'
  , time_window = 'month'
  , tie_type = 'count'
  , update = F
  , sources = 'ICEWS')


###############################################################################
##
## Get YEARLY data by root code and pentaclass.
##
###############################################################################

rootcode_year_data <- phoenix_net(
  start_date = 19950101
  , end_date = 20151001
  , level = 'rootcode'
  , phoenix_loc = paste0(phoenixnet_dir, '/phoenix')
  , icews_loc = paste0(phoenixnet_dir, '/icews')
  , codeset = 'all'
  , time_window = 'year'
  , tie_type = 'count'
  , update = F
  , sources = 'ICEWS')

pentaclass_year_data <- phoenix_net(
  start_date = 19950101
  , end_date = 20151001
  , level = 'pentaclass'
  , phoenix_loc = paste0(phoenixnet_dir, '/phoenix')
  , icews_loc = paste0(phoenixnet_dir, '/icews')
  , codeset = 'all'
  , time_window = 'year'
  , tie_type = 'count'
  , update = F
  , sources = 'ICEWS')


###############################################################################
##
## Save network data to file .
##
###############################################################################

save(rootcode_month_data, file = './rootcode_month_data.Rdata')
save(pentaclass_month_data, file = './pentaclass_month_data.Rdata')

save(rootcode_year_data, file = './rootcode_year_data.Rdata')
save(pentaclass_year_data, file = './pentaclass_year_data.Rdata')
