rm(list = ls())
setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel')
# setwd('/media/jesse/Files/Dropbox/Dissertation/Data/Israel')
library(rgdal)
library(foreign)
library(data.table)
library(raster)
library(rgeos)
library(maptools)
library(spdep)
library(spatcounts)
library(pscl)
library(e1071)
library(reshape2)
library(psych)
library(TSA)
library(vars)
library(zoo)
library(xts)
library(lubridate)
library(changepoint)
library(bcp)
library(devtools)
#devtools::install_github('twitter/BreakoutDetection')
#install_bitbucket('twinkle', 'alexiosg')
library(twinkle)
library(BreakoutDetection)
library(ecp)
# Read in data
data <- readOGR('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel\\IsraelGIS\\data'
                , 'events_20150731_2')
# data <- readOGR('/media/jesse/Files/Dropbox/Dissertation/Data/Israel/IsraelGIS/data'
#                 , 'events_20150731_2')
data$eventid <- seq(nrow(data))

# Merge in population data
popdata <- as.data.frame(fread('populations.csv', na.strings = ''))
data@data <- merge(data@data, popdata, by = 'location1', all.x = T)
data@data <- data@data[order(data@data$eventid), ]

# Merge in data on ISR/PAL general location
isr_shp <- readOGR('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel\\IsraelGIS\\data'
                   , 'ISR_admo0_proj')
# isr_shp <- readOGR('/media/jesse/Files/Dropbox/Dissertation/Data/Israel/IsraelGIS/data'
#                    , 'ISR_admo0_proj')

pal_shp <- readOGR('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel\\IsraelGIS\\data'
                   , 'PSE_admo0_proj')
# pal_shp <- readOGR('/media/jesse/Files/Dropbox/Dissertation/Data/Israel/IsraelGIS/data'
#                    , 'PSE_admo0_proj')

data@data$israel <- sapply(over(data, isr_shp, returnList = T), nrow)
data@data$palestine <- sapply(over(data, pal_shp, returnList = T), nrow)

# Merge in data on nearest border locations
borderpoints <- readOGR('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel\\IsraelGIS\\data'
                , 'border_points')
# borderpoints <- readOGR('/media/jesse/Files/Dropbox/Dissertation/Data/Israel/IsraelGIS/data'
#                         , 'border_points')

data$mindist_border <- NA
for(i in 1:nrow(data)){
  border_distances <- pointDistance(coordinates(data[i,]), coordinates(borderpoints)[,c(1:2)], lonlat = F)
  mindist <- min(border_distances) / 1000
  data$mindist_border[i] <- mindist
}
# Modify measure so that NEGATIVE distances are in palestine and POSITIVE distances are in israel
data@data$mindist_border[data@data$palestine == 1] <- data@data$mindist_border[data@data$palestine == 1] * -1

##### Generate some counts by week
# Data setup
old_data <- data

data <- data.table(old_data@data)
setkeyv(data, c('year', 'month', 'day'))
# Create a WEEK count
data$date <- as.Date(paste(data$year, data$month, data$day, sep = '-'), format = '%Y-%m-%d')
data <- data[date > as.Date('2001-01-01') & date < as.Date('2005-02-09')]

# data$week <- floor_date(data$date, 'month')
data$week <- floor_date(data$date, 'week')
# data$week <- data$week - (min(data$week)-1)
# Drop a couple of miscoded locations
data <- data[lat > 30]

# Deal with actions with UNKNOWN outcomes
data[isr_noncom == 555, isr_noncom := median(isr_noncom[isr_noncom != 555], na.rm = T)]
data[pal_milita == 555, pal_milita := median(pal_milita[pal_milita != 555], na.rm = T)]
data[isr_milita == 555, isr_milita := median(isr_milita[isr_milita != 555], na.rm = T)]
data[propertyda == 555, propertyda := median(propertyda[propertyda != 555], na.rm = T)]

# Actor setup
pal_milactors = c('palgun', 'palmil', 'hamas', 'ij', 'palgov', 'pflp', 'prc', 'dflp', 'pflp')
pal_civactors = c('palciv', 'palag', 'palres', 'palind')
isr_milactors = c('isrpol', 'idf', 'isrgov')
isr_civactors = c('isrciv', 'isrres', 'isrpol')
small_arms = c('anti-tank missile', 'anti-tank missiles', 'grenade', 'knife', 'medium arms', 'small arms',
               'stones', 'tear gas', 'rubber bullets', 'concussion grenade', 'concussion grenades')
big_arms = c('aircraft', 'artillery', 'belt', 'car bomb', 'drone', 'explosives', 'fighter jets',
             'heavy arms', 'helicopter', 'helicopters', 'land-land missile', 'land-land missiles',
             'mortar', 'rockets', 'shelling')
direct_fire = c('anti_tank missile', 'anti_tank missiles', 'grenade', 'knife', 'medium arms', 'small arms',
                'stones', 'tear gas', 'rubber bullets', 'concussion grenade', 'concussion grenades',
                'heavy arms')
indirect_fire = c('aircraft', 'artillery', 'belt', 'car bomb', 'drone', 'explosives', 'fighter jets',
                  'helicopter', 'helicopters', 'land-land missile', 'land-land missiles',
                  'mortar', 'rockets', 'shelling')


### Types of location
# Population
data[, mean_palevent_pop := mean(as.numeric(Population[actor1 %in% pal_milactors]), na.rm = T), by = week]
data[, sd_palevent_pop := sd(as.numeric(Population[actor1 %in% pal_milactors]), na.rm = T), by = week]
data[is.na(mean_palevent_pop), mean_palevent_pop := 0]
data[is.na(sd_palevent_pop), sd_palevent_pop := 0]
data[, mean_isrevent_pop := mean(as.numeric(Population[actor1 %in% isr_milactors]), na.rm = T), by = week]
data[, sd_isrevent_pop := sd(as.numeric(Population[actor1 %in% isr_milactors]), na.rm = T), by = week]
data[is.na(mean_isrevent_pop), mean_isrevent_pop := 0]
data[is.na(sd_isrevent_pop), sd_isrevent_pop := 0]

# Israel vs palestine location
data[, mean_palevent_pal := mean(palestine[actor1 %in% pal_milactors], na.rm = T), by = week]
data[is.na(mean_palevent_pal), mean_palevent_pal := mean(data$mean_palevent_pal, na.rm = T)]
data[, mean_isrevent_pal := mean(palestine[actor1 %in% isr_milactors], na.rm = T), by = week]
data[is.na(mean_isrevent_pal), mean_isrevent_pal := mean(data$mean_isrevent_pal, na.rm = T)]

# Distance from border
data[, mean_palevent_bdist := mean(mindist_border[actor1 %in% pal_milactors], na.rm = T), by = week]
data[, sd_palevent_bdist := sd(mindist_border[actor1 %in% pal_milactors], na.rm = T), by = week]
data[is.na(mean_palevent_bdist), mean_palevent_bdist := mean(data$mean_palevent_bdist, na.rm = T)]
data[is.na(sd_palevent_bdist), sd_palevent_bdist := max(data$sd_palevent_bdist, na.rm = T)]
data[, mean_isrevent_bdist := mean(mindist_border[actor1 %in% isr_milactors], na.rm = T), by = week]
data[, sd_isrevent_bdist := sd(mindist_border[actor1 %in% isr_milactors], na.rm = T), by = week]
data[is.na(mean_isrevent_bdist), mean_isrevent_bdist := max(data$mean_isrevent_bdist, na.rm = T)]
data[is.na(sd_isrevent_bdist), sd_isrevent_bdist := max(data$sd_isrevent_bdist, na.rm = T)]

### Casualties
data[, paldead_mo := sum(pal_fatali), by = week]
data[, paldead_civ_mo := sum(pal_nonc_1), by = week]
data[, paldead_milt_mo := sum(pal_mili_1), by = week]
data[, paldead_mili_mo := sum(pal_mili_2), by = week]
data[is.na(paldead_mo), paldead_mo := 0]
data[is.na(paldead_civ_mo), paldead_civ_mo := 0]
data[is.na(paldead_mili_mo), paldead_mili_mo := 0]
data[is.na(paldead_milt_mo), paldead_milt_mo := 0]
data[, paldead_mil_mo := paldead_milt_mo + paldead_mili_mo, by = week]
data[, paldead_mili_mo := NULL]
data[, paldead_milt_mo := NULL]

data[, isrdead_mo := sum(isr_fatali), by = week]
data[, isrdead_civ_mo := sum(isr_nonc_1), by = week]
data[, isrdead_mil_mo := sum(isr_combat), by = week]
data[is.na(isrdead_mo), isrdead_mo := 0]
data[is.na(isrdead_civ_mo), isrdead_civ_mo := 0]
data[is.na(isrdead_mil_mo), isrdead_mil_mo := 0]

data[, palwound_mo := sum(pal_combin), by = week]
data[, palwound_civ_mo := sum(pal_noncom), by = week]
data[, palwound_mil_mo := sum(pal_milita), by = week]
data[is.na(palwound_mo), palwound_mo := 0]
data[is.na(palwound_civ_mo), palwound_civ_mo := 0]
data[is.na(palwound_mil_mo), palwound_mil_mo := 0]

data[, isrwound_civ_mo := sum(isr_noncom), by = week]
data[, isrwound_mil_mo := sum(isr_milita), by = week]
data[, isrwound_pol_mo := sum(isr_police), by = week]
data[, isrwound_mil_mo := isrwound_mil_mo + isrwound_pol_mo, by = week]
data[, isrwound_pol_mo := NULL]
data[is.na(isrwound_civ_mo), isrwound_civ_mo := 0]
data[is.na(isrwound_mil_mo), isrwound_mil_mo := 0]
data[, isrwound_mo := isrwound_civ_mo + isrwound_mil_mo]
data[is.na(isrwound_mo), isrwound_mo := 0]

data[, palcas_mo := palwound_mo + paldead_mo]
data[, isrcas_mo := isrwound_mo + isrdead_mo]

data[, pal_casratio := 0.0]
data[(palcas_mo + isrcas_mo) > 0, pal_casratio := (palcas_mo) / (isrcas_mo + palcas_mo)]

### Interactions
data[, event := 1.0]
data[, pal_event := sum(event == 1 & actor1 %in% pal_milactors), by = week]
data[, isr_event := sum(event == 1 & actor1 %in% isr_milactors), by = week]
data[, mut_event := sum(event == 1 & interactio %in% 'firefight'), by = week]

# Shootings
data[, pal_shooting := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shooting')), by = week]
data[pal_event > 0, pal_shooting := pal_shooting / pal_event, by = week]
data[, isr_shooting := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shooting')), by = week]
data[isr_event > 0, isr_shooting := isr_shooting / isr_event, by = week]

# Firefights
data[, pal_firefight := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'firefight')), by = week]
data[pal_event > 0, pal_firefight := pal_firefight / pal_event, by = week]
data[, isr_firefight := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'firefight')), by = week]
data[isr_event > 0, isr_firefight := isr_firefight / isr_event, by = week]

# Shellings
data[, pal_shelling := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shelling')), by = week]
data[pal_event > 0, pal_shelling := pal_shelling / pal_event, by = week]
data[, pal_smallshelling := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shelling' & technolo_1 %in% small_arms)), by = week]
data[pal_event > 0, pal_smallshelling := pal_smallshelling / pal_event, by = week]
data[, pal_bigshelling := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shelling' & technolo_1 %in% big_arms)), by = week]
data[pal_event > 0, pal_bigshelling := pal_bigshelling / pal_event, by = week]

data[, isr_shelling := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shelling')), by = week]
data[isr_event > 0, isr_shelling := isr_shelling / isr_event, by = week]
data[, isr_smallshelling := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shelling' & technolo_1 %in% small_arms)), by = week]
data[isr_event > 0, isr_smallshelling := isr_smallshelling / isr_event, by = week]
data[, isr_bigshelling := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shelling' & technolo_1 %in% big_arms)), by = week]
data[isr_event > 0, isr_bigshelling := isr_bigshelling / isr_event, by = week]

# Bombings
data[, pal_bombing := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'bombing')), by = week]
data[pal_event > 0, pal_bombing := pal_bombing / pal_event, by = week]
data[, pal_suicidebombing := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'bombing' & (context %in% 'suicide' | technolo_1 %in% 'belt'))), by = week]
data[pal_event > 0, pal_suicidebombing := pal_suicidebombing / pal_event, by = week]
data[, isr_bombing := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'bombing')), by = week]
data[isr_event > 0, isr_bombing := isr_bombing / isr_event, by = week]

# Raids
data[, pal_raid := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'raid')), by = week]
data[pal_event > 0, pal_raid := pal_raid / pal_event, by = week]
data[, isr_raid := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'raid')), by = week]
data[isr_event > 0, isr_raid := isr_raid / isr_event, by = week]
data[, isr_smallraid := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'raid' & technolo_1 %in% small_arms)), by = week]
data[isr_event > 0, isr_smallraid := isr_smallraid / isr_event, by = week]
data[, isr_bigraid := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'raid' & technolo_1 %in% big_arms)), by = week]
data[isr_event > 0, isr_bigraid := isr_bigraid / isr_event, by = week]

# Beatings 
data[, pal_beating := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'beating')), by = week]
data[pal_event > 0, pal_beating := pal_beating / pal_event, by = week]

# Clashes
data[, pal_clash := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'crowd control')), by = week]
data[pal_event > 0, pal_clash := pal_clash / pal_event, by = week]

# Air strikes
data[, isr_airstrike := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'air strike')), by = week]
data[isr_event > 0, isr_airstrike := isr_airstrike / isr_event, by = week]

# Nonviolent/defensive
data[, isr_nonviol := as.numeric(sum(actor1 %in% isr_milactors & interactio %in% c('bulldozing', 'detainment', 'movement restriction'))), by = week]
data[isr_event > 0, isr_nonviol := isr_nonviol / isr_event, by = week]

### Type of technology
# Heavy vs light arms
data[, pal_bigtech := as.numeric(sum(actor1 %in% pal_milactors & technology %in% big_arms)), by = week]
data[, pal_smalltech := as.numeric(sum(actor1 %in% pal_milactors & technology %in% small_arms)), by = week]
data[pal_event > 0, pal_bigtech := pal_bigtech / pal_event, by = week]
data[, isr_bigtech := as.numeric(sum(actor1 %in% isr_milactors & technology %in% big_arms)), by = week]
data[, isr_smalltech := as.numeric(sum(actor1 %in% isr_milactors & technology %in% small_arms)), by = week]
data[isr_event > 0, isr_bigtech := isr_bigtech / isr_event, by = week]
# Direct vs indirect fire
data[, pal_direct := as.numeric(sum(actor1 %in% pal_milactors & technology %in% direct_fire)), by = week]
data[pal_event > 0, pal_direct := pal_direct / pal_event, by = week]
data[, pal_indirect := as.numeric(sum(actor1 %in% pal_milactors & technology %in% indirect_fire)), by = week]
data[pal_event > 0, pal_indirect := pal_indirect / pal_event, by = week]
data[, isr_direct := as.numeric(sum(actor1 %in% isr_milactors & technology %in% direct_fire)), by = week]
data[isr_event > 0, isr_direct := isr_direct / isr_event, by = week]
data[, isr_indirect := as.numeric(sum(actor1 %in% isr_milactors & technology %in% indirect_fire)), by = week]
data[isr_event > 0, isr_indirect := isr_indirect / isr_event, by = week]

### Targets
data[, pal_civtargeting := as.numeric(sum(actor1 %in% pal_milactors & actor2 %in% isr_civactors)), by = week]
data[pal_event > 0, pal_civtargeting := pal_civtargeting / pal_event, by = week]
data[, pal_miltargeting := as.numeric(sum(actor1 %in% pal_milactors & actor2 %in% isr_milactors)), by = week]
data[pal_event > 0, pal_miltargeting := pal_miltargeting / pal_event, by = week]

data[, isr_civtargeting := as.numeric(sum(actor1 %in% isr_milactors & actor2 %in% pal_civactors)), by = week]
data[isr_event > 0, isr_civtargeting := isr_civtargeting / isr_event, by = week]
data[, isr_miltargeting := as.numeric(sum(actor1 %in% isr_milactors & actor2 %in% pal_milactors)), by = week]
data[isr_event > 0, isr_miltargeting := isr_miltargeting / isr_event, by = week]

### Collapse to year-month level
old_data2 <- data
#data <- old_data2
# data <- data[!duplicated(data[, week]), ]
data <- data[!duplicated(data[, week]), ]


#################################################################
######## Preliminary models
######## Changepoint detection (CPT)
library(bcp)
library(forecast)
bcp_function <- function(inputs){
  for(i in 1:ncol(inputs)){
    inputs[, i] <- (inputs[, i] - mean(inputs[, i])) / sd(inputs[, i])
  }
#   inputs <- ts(matrix(c(0,HoltWinters(inputs, beta = F, gamma =F)$fitted[, 2]), ncol = 4))
  pal_combined1_ecp <- bcp(inputs, w0 = 0.01, p0 = 0.01, burnin = 1000, mcmc = 10000)
  plot(pal_combined1_ecp, separated = T)
  return(pal_combined1_ecp)
}
### Palestinian strategy
# Location
inputs <- xts(data[, list(log(mean_palevent_pop+1))], order.by = data$date)
pal_loc <- e.divisive(inputs, R = 999)

# Technology
inputs <- xts(data[, list(pal_indirect)], order.by = data$date)
pal_tech <- e.divisive(inputs, R = 999)

# Target
inputs <- xts(data[, list(pal_miltargeting)], order.by = data$date)
pal_targ <- e.divisive(inputs, R = 999)

plot(data$pal_event)
abline(v = pal_loc$estimates[-c(1, length(pal_loc$estimates))], col = 'red', lty = 1, lwd = 2)
abline(v = pal_tech$estimates[-c(1, length(pal_tech$estimates))], col = 'blue', lty = 2, lwd = 2)
abline(v = pal_targ$estimates[-c(1, length(pal_targ$estimates))], col = 'green', lty = 3, lwd = 2)

pal_changepoints <- unique(sort(c(pal_loc$estimates[-c(1, length(pal_loc$estimates))], 
                                  pal_tech$estimates[-c(1, length(pal_tech$estimates))], 
                                  pal_targ$estimates[-c(1, length(pal_targ$estimates))])))
### Israeli strategy
# Location
inputs <- xts(data[, list(log(mean_isrevent_pop+1))], order.by = data$date)
isr_loc <- e.divisive(inputs, R = 999)

# Technology
inputs <- xts(data[, list(isr_indirect)], order.by = data$date)
isr_tech <- e.divisive(inputs, R = 999)

# Target
inputs <- xts(data[, list(isr_miltargeting)], order.by = data$date)
isr_targ <- e.divisive(inputs, R = 999)


plot(data$isr_event)
abline(v = isr_loc$estimates[-c(1, length(isr_loc$estimates))], col = 'red', lty = 1, lwd = 2)
abline(v = isr_tech$estimates[-c(1, length(isr_tech$estimates))], col = 'blue', lty = 2, lwd = 2)
abline(v = isr_targ$estimates[-c(1, length(isr_targ$estimates))], col = 'green', lty = 3, lwd = 2)

isr_changepoints <- unique(sort(c(isr_loc$estimates[-c(1, length(isr_loc$estimates))], 
                           isr_tech$estimates[-c(1, length(isr_tech$estimates))], 
                           isr_targ$estimates[-c(1, length(isr_targ$estimates))])))



#############################################################################
## VAR TS modeling
var_function <- function(inputs, exog, response = NULL){
#   model1 <- VAR(inputs, exogen = exog, lag.max = 4, ic = 'AIC', type = 'both')
  model1 <- VAR(inputs, exogen = exog, p = 1, type = 'both')
  print(summary(model1))
  irf_m1 <- irf(model1, n.ahead = 5, runs = 500)
  plot(irf_m1)
  return(model1)
}

# Israeli and Palestinian actions
# Period 1
start_idx <- 1
stop_idx <- 62
inputs = xts(data[start_idx:stop_idx,list(isr_event, pal_event)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog)
# Period 2
start_idx <- 62
stop_idx <- 86
inputs = xts(data[start_idx:stop_idx,list(isr_event, pal_event)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog)
# Period 2
start_idx <- 86
stop_idx <- 146
inputs = xts(data[start_idx:stop_idx,list(isr_event, pal_event)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog)
# Period 3
start_idx <- 146
stop_idx <- 186
inputs = xts(data[start_idx:stop_idx,list(isr_event, pal_event)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog)
#############################################################################
# ARIMA TS modeling
############################ Israeli actions
# Any Israeli-initiated activity
# Period 1
isr_ts_fitting <- function(main_out, main_exog){
  start_idx <- 1
  stop_idx <- 64
  output = main_out[start_idx:stop_idx, ]
  exog = lag(main_exog[start_idx:stop_idx, ], 1)
  output <- scale(output); exog <- scale(exog)
  model1 <- auto.arima(output, max.p = 3, max.q = 3, max.P = 3, max.Q = 3, max.d = 2, max.D = 2,
                       stepwise = F, parallel = T, num.cores = 7,
                       xreg = exog)
  # Period 2
  start_idx <- 64
  stop_idx <- 124
  output = main_out[start_idx:stop_idx, ]
  exog = lag(main_exog[start_idx:stop_idx, ], 1)
  output <- scale(output); exog <- scale(exog)
  model2 <- auto.arima(output, max.p = 3, max.q = 3, max.P = 3, max.Q = 3, max.d = 2, max.D = 2,
                       stepwise = F, parallel = T, num.cores = 7,
                       xreg = exog)
  # Period 3
  start_idx <- 124
  stop_idx <- 155
  output = main_out[start_idx:stop_idx, ]
  exog = lag(main_exog[start_idx:stop_idx, ], 1)
  output <- scale(output); exog <- scale(exog)
  model3 <- auto.arima(output, max.p = 3, max.q = 3, max.P = 3, max.Q = 3, max.d = 2, max.D = 2,
                       stepwise = F, parallel = T, num.cores = 7,
                       xreg = exog)
  # Period 4
  start_idx <- 155
  stop_idx <- 172
  output = main_out[start_idx:stop_idx, ]
  exog = lag(main_exog[start_idx:stop_idx, ], 1)
  output <- scale(output); exog <- scale(exog)
  model4 <- auto.arima(output, max.p = 3, max.q = 3, max.P = 3, max.Q = 3, max.d = 2, max.D = 2,
                       stepwise = F, parallel = T, num.cores = 7,
                       xreg = exog)
  # Period 5
  start_idx <- 172
  stop_idx <- 215
  output = main_out[start_idx:stop_idx, ]
  exog = lag(main_exog[start_idx:stop_idx, ], 1)
  output <- scale(output); exog <- scale(exog)
  model4 <- auto.arima(output, max.p = 3, max.q = 3, max.P = 3, max.Q = 3, max.d = 2, max.D = 2,
                       stepwise = F, parallel = T, num.cores = 7,
                       xreg = exog)
  return(list(model1, model2, model3, model4))
}

output <- xts(data[,list(isr_event)], order.by = data$week)
exog <- xts(data[,list(pal_event, isrdead_mo, pal_indirect, mean_palevent_pop, pal_civtargeting)], order.by = data$week)
isr_ts_fitting(output, exog)

# Palestinian indirect violence
# Period 1
start_idx <- 1
stop_idx <- 62
output = xts(data[start_idx:stop_idx,list(pal_indirect)], order.by = data$week[start_idx:stop_idx])
exog = lag(xts(data[start_idx:stop_idx,list(isr_event, paldead_mo, isr_bigtech, mean_isrevent_pop, isr_civtargeting)], order.by = data$week[start_idx:stop_idx]), 1)
output <- scale(output); exog <- scale(exog)
model1 <- auto.arima(output, max.p = 3, max.q = 3, max.P = 3, max.Q = 3, max.d = 2, max.D = 2,
                     stepwise = F, parallel = T, num.cores = 7,
                     xreg = exog)
# Period 2
start_idx <- 62
stop_idx <- 86
output = xts(data[start_idx:stop_idx,list(pal_indirect)], order.by = data$week[start_idx:stop_idx])
exog = lag(xts(data[start_idx:stop_idx,list(isr_event, paldead_mo, isr_bigtech, mean_isrevent_pop, isr_civtargeting)], order.by = data$week[start_idx:stop_idx]), 1)
output <- scale(output); exog <- scale(exog)
model2 <- auto.arima(output, max.p = 3, max.q = 3, max.P = 3, max.Q = 3, max.d = 2, max.D = 2,
                     stepwise = F, parallel = T, num.cores = 7,
                     xreg = exog)
# Period 2
start_idx <- 86
stop_idx <- 146
output = xts(data[start_idx:stop_idx,list(pal_indirect)], order.by = data$week[start_idx:stop_idx])
exog = lag(xts(data[start_idx:stop_idx,list(isr_event, paldead_mo, isr_bigtech, mean_isrevent_pop, isr_civtargeting)], order.by = data$week[start_idx:stop_idx]), 1)
output <- scale(output); exog <- scale(exog)
model3 <- auto.arima(output, max.p = 3, max.q = 3, max.P = 3, max.Q = 3, max.d = 2, max.D = 2,
                     stepwise = F, parallel = T, num.cores = 7,
                     xreg = exog)
# Period 3
start_idx <- 146
stop_idx <- 186
output = xts(data[start_idx:stop_idx,list(pal_indirect)], order.by = data$week[start_idx:stop_idx])
exog = lag(xts(data[start_idx:stop_idx,list(isr_event, paldead_mo, isr_bigtech, mean_isrevent_pop, isr_civtargeting)], order.by = data$week[start_idx:stop_idx]), 1)
output <- scale(output); exog <- scale(exog)
model4 <- auto.arima(output, max.p = 3, max.q = 3, max.P = 3, max.Q = 3, max.d = 2, max.D = 2,
                     stepwise = F, parallel = T, num.cores = 7,
                     xreg = exog)
model1; model2; model3; model4


# Israeli heavy arms use and Palestinian targeting civilians
# Period 1
start_idx <- 1
stop_idx <- 62
inputs = xts(data[start_idx:stop_idx,list(isr_bigtech, pal_civtargeting)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog)
# Period 2
start_idx <- 62
stop_idx <- 86
inputs = xts(data[start_idx:stop_idx,list(isr_bigtech, pal_civtargeting)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog)
# Period 2
start_idx <- 86
stop_idx <- 146
inputs = xts(data[start_idx:stop_idx,list(isr_bigtech, pal_civtargeting)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog)
# Period 3
start_idx <- 146
stop_idx <- 186
inputs = xts(data[start_idx:stop_idx,list(isr_bigtech, pal_civtargeting)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog)

# Israeli defensive measures and Palestinian indirect fire
# Period 1
start_idx <- 1
stop_idx <- 62
inputs = xts(data[start_idx:stop_idx,list(isr_nonviol, pal_indirect)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'pal_indirect')
# Period 2
start_idx <- 62
stop_idx <- 86
inputs = xts(data[start_idx:stop_idx,list(isr_nonviol, pal_indirect)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'pal_indirect')
# Period 2
start_idx <- 86
stop_idx <- 146
inputs = xts(data[start_idx:stop_idx,list(isr_nonviol, pal_indirect)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'pal_indirect')
# Period 3
start_idx <- 146
stop_idx <- 186
inputs = xts(data[start_idx:stop_idx,list(isr_nonviol, pal_indirect)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'pal_indirect')


# Palestinian targeting civilians and Israeli heavy weaponry
# Period 1
start_idx <- 1
stop_idx <- 64
inputs = xts(data[start_idx:stop_idx,list(pal_civtargeting, isr_bigtech)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'isr_bigtech')
# Period 2
start_idx <- 64
stop_idx <- 131
inputs = xts(data[start_idx:stop_idx,list(pal_civtargeting, isr_bigtech)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'isr_bigtech')
# Period 2
start_idx <- 131
stop_idx <- 172
inputs = xts(data[start_idx:stop_idx,list(pal_civtargeting, isr_bigtech)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'isr_bigtech')
# Period 3
start_idx <- 131
stop_idx <- 172
inputs = xts(data[start_idx:stop_idx,list(pal_civtargeting, isr_bigtech)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'isr_bigtech')

# Palestinian targeting civilians and Israeli force projection
# Period 1
start_idx <- 1
stop_idx <- 64
inputs = xts(data[start_idx:stop_idx,list(pal_civtargeting, isr_raid)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'isr_raid')
# Period 2
start_idx <- 64
stop_idx <- 131
inputs = xts(data[start_idx:stop_idx,list(pal_civtargeting, isr_raid)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'isr_raid')
# Period 2
start_idx <- 131
stop_idx <- 172
inputs = xts(data[start_idx:stop_idx,list(pal_civtargeting, isr_raid)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'isr_raid')
# Period 3
start_idx <- 131
stop_idx <- 172
inputs = xts(data[start_idx:stop_idx,list(pal_civtargeting, isr_raid)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
var_function(inputs, exog, 'isr_raid')

# Palestinian targeting civilians and Israeli force projection
# Period 1
start_idx <- 1
stop_idx <- 64
inputs = xts(data[start_idx:stop_idx,list(pal_civtargeting, isr_airstrike)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'isr_airstrike')
# Period 2
start_idx <- 64
stop_idx <- 131
inputs = xts(data[start_idx:stop_idx,list(pal_civtargeting, isr_airstrike)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'isr_airstrike')
# Period 2
start_idx <- 131
stop_idx <- 172
inputs = xts(data[start_idx:stop_idx,list(pal_civtargeting, isr_airstrike)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'isr_airstrike')
# Period 3
start_idx <- 131
stop_idx <- 172
inputs = xts(data[start_idx:stop_idx,list(pal_civtargeting, isr_airstrike)], order.by = data$week[start_idx:stop_idx])
exog = xts(data[start_idx:stop_idx,list(isr_event, pal_event, isrdead_mo, paldead_mo)], order.by = data$week[start_idx:stop_idx])
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog, 'isr_airstrike')


