rm(list = ls())
# setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel')
setwd('/media/jesse/Files/Dropbox/Dissertation/Data/Israel')
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
# Read in data
# data <- readOGR('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel\\IsraelGIS\\data'
#                 , 'events_20150731_2')
data <- readOGR('/media/jesse/Files/Dropbox/Dissertation/Data/Israel/IsraelGIS/data'
                , 'events_20150731_2')
data$eventid <- seq(nrow(data))

# Merge in population data
popdata <- as.data.frame(fread('populations.csv', na.strings = ''))
data@data <- merge(data@data, popdata, by = 'location1', all.x = T)
data@data <- data@data[order(data@data$eventid), ]

# Merge in data on ISR/PAL general location
# isr_shp <- readOGR('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel\\IsraelGIS\\data'
#                    , 'ISR_admo0_proj')
isr_shp <- readOGR('/media/jesse/Files/Dropbox/Dissertation/Data/Israel/IsraelGIS/data'
                   , 'ISR_admo0_proj')

# pal_shp <- readOGR('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel\\IsraelGIS\\data'
#                    , 'PSE_admo0_proj')
pal_shp <- readOGR('/media/jesse/Files/Dropbox/Dissertation/Data/Israel/IsraelGIS/data'
                   , 'PSE_admo0_proj')

data@data$israel <- sapply(over(data, isr_shp, returnList = T), nrow)
data@data$palestine <- sapply(over(data, pal_shp, returnList = T), nrow)

# Merge in data on nearest border locations
# borderpoints <- readOGR('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel\\IsraelGIS\\data'
#                 , 'border_points')
borderpoints <- readOGR('/media/jesse/Files/Dropbox/Dissertation/Data/Israel/IsraelGIS/data'
                        , 'border_points')

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
data[, mean_palevent_pop := mean(as.numeric(Population[actor1 %in% pal_milactors]), na.rm = T), by = date]
data[, sd_palevent_pop := sd(as.numeric(Population[actor1 %in% pal_milactors]), na.rm = T), by = date]
data[is.na(mean_palevent_pop), mean_palevent_pop := 0]
data[is.na(sd_palevent_pop), sd_palevent_pop := 0]
data[, mean_isrevent_pop := mean(as.numeric(Population[actor1 %in% isr_milactors]), na.rm = T), by = date]
data[, sd_isrevent_pop := sd(as.numeric(Population[actor1 %in% isr_milactors]), na.rm = T), by = date]
data[is.na(mean_isrevent_pop), mean_isrevent_pop := 0]
data[is.na(sd_isrevent_pop), sd_isrevent_pop := 0]

# Israel vs palestine location
data[, mean_palevent_pal := mean(palestine[actor1 %in% pal_milactors], na.rm = T), by = date]
data[is.na(mean_palevent_pal), mean_palevent_pal := mean(data$mean_palevent_pal, na.rm = T)]
data[, mean_isrevent_pal := mean(palestine[actor1 %in% isr_milactors], na.rm = T), by = date]
data[is.na(mean_isrevent_pal), mean_isrevent_pal := mean(data$mean_isrevent_pal, na.rm = T)]

# Distance from border
data[, mean_palevent_bdist := mean(mindist_border[actor1 %in% pal_milactors], na.rm = T), by = date]
data[, sd_palevent_bdist := sd(mindist_border[actor1 %in% pal_milactors], na.rm = T), by = date]
data[is.na(mean_palevent_bdist), mean_palevent_bdist := mean(data$mean_palevent_bdist, na.rm = T)]
data[is.na(sd_palevent_bdist), sd_palevent_bdist := max(data$sd_palevent_bdist, na.rm = T)]
data[, mean_isrevent_bdist := mean(mindist_border[actor1 %in% isr_milactors], na.rm = T), by = date]
data[, sd_isrevent_bdist := sd(mindist_border[actor1 %in% isr_milactors], na.rm = T), by = date]
data[is.na(mean_isrevent_bdist), mean_isrevent_bdist := max(data$mean_isrevent_bdist, na.rm = T)]
data[is.na(sd_isrevent_bdist), sd_isrevent_bdist := max(data$sd_isrevent_bdist, na.rm = T)]

### Casualties
data[, paldead_mo := sum(pal_fatali), by = date]
data[, paldead_civ_mo := sum(pal_nonc_1), by = date]
data[, paldead_milt_mo := sum(pal_mili_1), by = date]
data[, paldead_mili_mo := sum(pal_mili_2), by = date]
data[is.na(paldead_mo), paldead_mo := 0]
data[is.na(paldead_civ_mo), paldead_civ_mo := 0]
data[is.na(paldead_mil_mo), paldead_mil_mo := 0]
data[is.na(paldead_milt_mo), paldead_milt_mo := 0]
data[, paldead_mil_mo := paldead_milt_mo + paldead_mili_mo, by = date]
data[, paldead_mili_mo := NULL]
data[, paldead_milt_mo := NULL]

data[, isrdead_mo := sum(isr_fatali), by = date]
data[, isrdead_civ_mo := sum(isr_nonc_1), by = date]
data[, isrdead_mil_mo := sum(isr_combat), by = date]
data[is.na(isrdead_mo), isrdead_mo := 0]
data[is.na(isrdead_civ_mo), isrdead_civ_mo := 0]
data[is.na(isrdead_mil_mo), isrdead_mil_mo := 0]

data[, palwound_mo := sum(pal_combin), by = date]
data[, palwound_civ_mo := sum(pal_noncom), by = date]
data[, palwound_mil_mo := sum(pal_milita), by = date]
data[is.na(palwound_mo), palwound_mo := 0]
data[is.na(palwound_civ_mo), palwound_civ_mo := 0]
data[is.na(palwound_mil_mo), palwound_mil_mo := 0]

data[, isrwound_civ_mo := sum(isr_noncom), by = date]
data[, isrwound_mil_mo := sum(isr_milita), by = date]
data[, isrwound_pol_mo := sum(isr_police), by = date]
data[, isrwound_mil_mo := isrwound_mil_mo + isrwound_pol_mo, by = date]
data[, isrwound_pol_mo := NULL]
data[is.na(isrwound_civ_mo), isrwound_civ_mo := 0]
data[is.na(isrwound_mil_mo), isrwound_mil_mo := 0]
data[, isrwound_mo := isrwound_civ_mo + isrwound_mil_mo]
data[is.na(isrwound_mo), isrwound_mo := 0]

### Interactions
data[, event := 1.0]
data[, pal_event := sum(event == 1 & actor1 %in% pal_milactors), by = date]
data[, isr_event := sum(event == 1 & actor1 %in% isr_milactors), by = date]
data[, mut_event := sum(event == 1 & interactio %in% 'firefight'), by = date]

# Shootings
data[, pal_shooting := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shooting')), by = date]
data[pal_event > 0, pal_shooting := pal_shooting / pal_event, by = date]
data[, isr_shooting := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shooting')), by = date]
data[isr_event > 0, isr_shooting := isr_shooting / isr_event, by = date]

# Firefights
data[, pal_firefight := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'firefight')), by = date]
data[pal_event > 0, pal_firefight := pal_firefight / pal_event, by = date]
data[, isr_firefight := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'firefight')), by = date]
data[isr_event > 0, isr_firefight := isr_firefight / isr_event, by = date]

# Shellings
data[, pal_shelling := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shelling')), by = date]
data[pal_event > 0, pal_shelling := pal_shelling / pal_event, by = date]
data[, pal_smallshelling := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shelling' & technolo_1 %in% small_arms)), by = date]
data[pal_event > 0, pal_smallshelling := pal_smallshelling / pal_event, by = date]
data[, pal_bigshelling := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shelling' & technolo_1 %in% big_arms)), by = date]
data[pal_event > 0, pal_bigshelling := pal_bigshelling / pal_event, by = date]

data[, isr_shelling := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shelling')), by = date]
data[isr_event > 0, isr_shelling := isr_shelling / isr_event, by = date]
data[, isr_smallshelling := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shelling' & technolo_1 %in% small_arms)), by = date]
data[isr_event > 0, isr_smallshelling := isr_smallshelling / isr_event, by = date]
data[, isr_bigshelling := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shelling' & technolo_1 %in% big_arms)), by = date]
data[isr_event > 0, isr_bigshelling := isr_bigshelling / isr_event, by = date]

# Bombings
data[, pal_bombing := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'bombing')), by = date]
data[pal_event > 0, pal_bombing := pal_bombing / pal_event, by = date]
data[, pal_suicidebombing := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'bombing' & (context %in% 'suicide' | technolo_1 %in% 'belt'))), by = date]
data[pal_event > 0, pal_suicidebombing := pal_suicidebombing / pal_event, by = date]
data[, isr_bombing := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'bombing')), by = date]
data[isr_event > 0, isr_bombing := isr_bombing / isr_event, by = date]

# Raids
data[, pal_raid := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'raid')), by = date]
data[pal_event > 0, pal_raid := pal_raid / pal_event, by = date]
data[, isr_raid := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'raid')), by = date]
data[isr_event > 0, isr_raid := isr_raid / isr_event, by = date]
data[, isr_smallraid := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'raid' & technolo_1 %in% small_arms)), by = date]
data[isr_event > 0, isr_smallraid := isr_smallraid / isr_event, by = date]
data[, isr_bigraid := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'raid' & technolo_1 %in% big_arms)), by = date]
data[isr_event > 0, isr_bigraid := isr_bigraid / isr_event, by = date]

# Beatings 
data[, pal_beating := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'beating')), by = date]
data[pal_event > 0, pal_beating := pal_beating / pal_event, by = date]

# Clashes
data[, pal_clash := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'crowd control')), by = date]
data[pal_event > 0, pal_clash := pal_clash / pal_event, by = date]

# Air strikes
data[, isr_airstrike := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'air strike')), by = date]
data[isr_event > 0, isr_airstrike := isr_airstrike / isr_event, by = date]

# Nonviolent/defensive
data[, isr_nonviol := as.numeric(sum(actor1 %in% isr_milactors & interactio %in% c('bulldozing', 'detainment', 'movement restriction'))), by = date]
data[isr_event > 0, isr_nonviol := isr_nonviol / isr_event, by = date]

### Type of technology
# Heavy vs light arms
data[, pal_bigtech := as.numeric(sum(actor1 %in% pal_milactors & technology %in% big_arms)), by = date]
data[, pal_smalltech := as.numeric(sum(actor1 %in% pal_milactors & technology %in% small_arms)), by = date]
data[pal_event > 0, pal_bigtech := pal_bigtech / pal_event, by = date]
data[, isr_bigtech := as.numeric(sum(actor1 %in% isr_milactors & technology %in% big_arms)), by = date]
data[, isr_smalltech := as.numeric(sum(actor1 %in% isr_milactors & technology %in% small_arms)), by = date]
data[isr_event > 0, isr_bigtech := isr_bigtech / isr_event, by = date]
# Direct vs indirect fire
data[, pal_direct := as.numeric(sum(actor1 %in% pal_milactors & technology %in% direct_fire)), by = date]
data[pal_event > 0, pal_direct := pal_direct / pal_event, by = date]
data[, pal_indirect := as.numeric(sum(actor1 %in% pal_milactors & technology %in% indirect_fire)), by = date]
data[pal_event > 0, pal_indirect := pal_indirect / pal_event, by = date]
data[, isr_direct := as.numeric(sum(actor1 %in% isr_milactors & technology %in% direct_fire)), by = date]
data[isr_event > 0, isr_direct := isr_direct / isr_event, by = date]
data[, isr_indirect := as.numeric(sum(actor1 %in% isr_milactors & technology %in% indirect_fire)), by = date]
data[isr_event > 0, isr_indirect := isr_indirect / isr_event, by = date]

### Targets
data[, pal_civtargeting := as.numeric(sum(actor1 %in% pal_milactors & actor2 %in% isr_civactors)), by = date]
data[pal_event > 0, pal_civtargeting := pal_civtargeting / pal_event, by = date]
data[, pal_miltargeting := as.numeric(sum(actor1 %in% pal_milactors & actor2 %in% isr_milactors)), by = date]
data[pal_event > 0, pal_miltargeting := pal_miltargeting / pal_event, by = date]

data[, isr_civtargeting := as.numeric(sum(actor1 %in% isr_milactors & actor2 %in% pal_civactors)), by = date]
data[isr_event > 0, isr_civtargeting := isr_civtargeting / isr_event, by = date]
data[, isr_miltargeting := as.numeric(sum(actor1 %in% isr_milactors & actor2 %in% pal_milactors)), by = date]
data[isr_event > 0, isr_miltargeting := isr_miltargeting / isr_event, by = date]

### Collapse to year-month level
old_data2 <- data
#data <- old_data2
# data <- data[!duplicated(data[, date]), ]
data <- data[!duplicated(data[, date]), ]


#################################################################
######## Preliminary models
######## Breakout Detection (Twitter) and changepoint detection (CPT)
### Palestinian strategy
## General activities
# Intensity
pal_int_breakout <- breakout(log(data$pal_event+1), min.size = 12, method = 'multi', beta = 0.01, plot = T)
pal_int_mvarcp <- cpt.meanvar(ts(log(data$pal_event+1)), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
# pal_int_mvarcp <- cpt.meanvar(ts(log(data$pal_event+1)), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
plot(pal_int_mvarcp)
abline(v = pal_int_breakout$loc, col = 'red', lty = 3)
# Distance to ISRPAL border
pal_mbdist_breakout <- breakout(data$mean_palevent_bdist, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# pal_mbdist_mvarcp <- cpt.meanvar(ts(data$mean_palevent_bdist), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
pal_mbdist_mvarcp <- cpt.meanvar(ts(data$mean_palevent_bdist), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(pal_mbdist_mvarcp)
abline(v = pal_mbdist_breakout$loc, col = 'red', lty = 3)
pal_sdbdist_breakout <- breakout(data$sd_palevent_bdist, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# pal_sdbdist_mvarcp <- cpt.meanvar(ts(data$sd_palevent_bdist), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
pal_sdbdist_mvarcp <- cpt.meanvar(ts(data$sd_palevent_bdist), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(pal_sdbdist_mvarcp)
abline(v = pal_sdbdist_breakout$loc, col = 'red', lty = 3)
# Population of targets
pal_mpop_breakout <- breakout(log(data$mean_palevent_pop+1), min.size = 12, method = 'multi', beta = 0.01, plot = T)
# pal_mpop_mvarcp <- cpt.meanvar(ts(log(data$mean_palevent_pop+1)), method = 'PELT', penalty = 'Manual', pen.value = '3.5*log(n)', minseglen = 12)
pal_mpop_mvarcp <- cpt.meanvar(ts(log(data$mean_palevent_pop+1)), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(pal_mpop_mvarcp)
abline(v = pal_mpop_breakout$loc, col = 'red', lty = 3)
pal_sdpop_breakout <- breakout(log(data$sd_palevent_pop+1), min.size = 12, method = 'multi', beta = 0.01, plot = T)
# pal_sdpop_mvarcp <- cpt.meanvar(ts(log(data$sd_palevent_pop+1)), method = 'PELT', penalty = 'Manual', pen.value = '3.5*log(n)', minseglen = 12)
pal_sdpop_mvarcp <- cpt.meanvar(ts(log(data$sd_palevent_pop+1)), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(pal_sdpop_mvarcp)
abline(v = pal_sdpop_breakout$loc, col = 'red', lty = 3)
# Targets in Israel vs Palestine
pal_loc_breakout <- breakout(data$mean_palevent_pal, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# pal_loc_mvarcp <- cpt.meanvar(ts(data$mean_palevent_pal), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
pal_loc_mvarcp <- cpt.meanvar(ts(data$mean_palevent_pal), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(pal_loc_mvarcp)
abline(v = pal_loc_breakout$loc, col = 'red', lty = 3)
# Civilian vs military targets
pal_civtarg_breakout <- breakout(data$pal_civtargeting, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# pal_civtarget_mvarcp <- cpt.meanvar(ts(data$pal_civtargeting), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
pal_civtarget_mvarcp <- cpt.meanvar(ts(data$pal_civtargeting), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(pal_civtarget_mvarcp)
abline(v = pal_civtarg_breakout$loc, col = 'red', lty = 3)
## Tactics
# Reliance on direct-fire weaponry
pal_direct_breakout <- breakout(data$pal_direct, min.size = 12, method = 'multi', beta = 0.00751, plot = T)
# pal_direct_mvarcp <- cpt.meanvar(ts(data$pal_direct), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
pal_direct_mvarcp <- cpt.meanvar(ts(data$pal_direct), method = 'BinSeg', Q = 4, penalty = 'BIC',, minseglen = 12)
plot(pal_direct_mvarcp)
abline(v = pal_direct_breakout$loc, col = 'red', lty = 3)
pal_indirect_breakout <- breakout(data$pal_indirect, min.size = 12, method = 'multi', beta = 0.00751, plot = T)
# pal_indirect_mvarcp <- cpt.meanvar(ts(data$pal_indirect), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
pal_indirect_mvarcp <- cpt.meanvar(ts(data$pal_indirect), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(pal_indirect_mvarcp)
abline(v = pal_indirect_breakout$loc, col = 'red', lty = 3)
# Reliance on shelling
pal_shelling_breakout <- breakout(data$pal_shelling, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# pal_shelling_mvarcp <- cpt.meanvar(ts(data$pal_shelling), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
pal_shelling_mvarcp <- cpt.meanvar(ts(data$pal_shelling), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(pal_shelling_mvarcp)
abline(v = pal_shelling_breakout$loc, col = 'red', lty = 3)
# Reliance on bombing
pal_bombing_breakout <- breakout(data$pal_bombing, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# pal_direct_mvarcp <- cpt.meanvar(ts(data$pal_bombing), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
pal_direct_mvarcp <- cpt.meanvar(ts(data$pal_bombing), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(pal_direct_mvarcp)
abline(v = pal_bombing_breakout$loc, col = 'red', lty = 3)

## Combined
library(bcp)
bcp_function <- function(inputs){
  for(i in 1:ncol(inputs)){
    inputs[, i] <- (inputs[, i] - mean(inputs[, i])) / sd(inputs[, i])
  }
  pal_combined1_ecp <- bcp(inputs, w0 = 0.5, p0 = 0.1, burnin = 1000, mcmc = 5000)
  plot(pal_combined1_ecp, separated = T)
  return(pal_combined1_ecp)
}
# All behavioral elements
inputs <- xts(data[, list(isrdead_mo, log(mean_palevent_pop+1), pal_indirect, pal_civtargeting)], order.by = data$date)
test <- bcp_function(inputs)

# Behavior breakdown (not event count)
inputs <- xts(data[, list(log(mean_palevent_pop+1), pal_indirect, pal_civtargeting)], order.by = data$date)
test <- bcp_function(inputs)

# Location and target
inputs <- xts(data[, list(log(mean_palevent_pop+1), pal_civtargeting)], order.by = data$date)
test <- bcp_function(inputs)

# Technology and target
inputs <- xts(data[, list(pal_indirect, pal_civtargeting)], order.by = data$date)
test <- bcp_function(inputs)

# Technology and location
inputs <- xts(data[, list(pal_indirect, log(mean_palevent_pop+1))], order.by = data$date)
test <- bcp_function(inputs)

# Location
inputs <- xts(data[, list(log(mean_palevent_pop+1))], order.by = data$date)
test <- bcp_function(inputs)

# Technology
inputs <- xts(data[, list(pal_indirect)], order.by = data$date)
test <- bcp_function(inputs)

# Target
inputs <- xts(data[, list(pal_bombing)], order.by = data$date)
test <- bcp_function(inputs)



### Israeli strategy
## General activities
# Intensity
isr_int_breakout <- breakout(log(data$isr_event+1), min.size = 12, method = 'multi', beta = 0.008, plot = T)
# isr_int_mvarcp <- cpt.meanvar(ts(log(data$isr_event+1)), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
isr_int_mvarcp <- cpt.meanvar(ts(log(data$isr_event+1)), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(pal_int_mvarcp)
abline(v = isr_int_breakout$loc, col = 'red', lty = 3)
# Distance to ISRPAL border
isr_mbdist_breakout <- breakout(data$mean_isrevent_bdist, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# isr_bdist_mvarcp <- cpt.meanvar(data$mean_isrevent_bdist, method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
isr_bdist_mvarcp <- cpt.meanvar(data$mean_isrevent_bdist, method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(isr_bdist_mvarcp)
abline(v = isr_mbdist_breakout$loc, col = 'red', lty = 3)
isr_sdbdist_breakout <- breakout(data$sd_isrevent_bdist, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# isr_sddist_mvarcp <- cpt.meanvar(ts(data$sd_isrevent_bdist), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
isr_sddist_mvarcp <- cpt.meanvar(ts(data$sd_isrevent_bdist), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(isr_sddist_mvarcp)
abline(v = isr_sdbdist_breakout$loc, col = 'red', lty = 3)
# Population of targets
isr_mpop_breakout <- breakout(log(data$mean_isrevent_pop+1), min.size = 12, method = 'multi', beta = 0.01, plot = T)
# isr_mpop_mvarcp <- cpt.meanvar(ts(log(data$mean_isrevent_pop+1)), method = 'PELT', penalty = 'Manual', pen.value = '3.5*log(n)', minseglen = 12)
isr_mpop_mvarcp <- cpt.meanvar(ts(log(data$mean_isrevent_pop+1)), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(isr_mpop_mvarcp)
abline(v = isr_mpop_breakout$loc, col = 'red', lty = 3)
isr_sdpop_breakout <- breakout(log(data$sd_isrevent_pop+1), min.size = 12, method = 'multi', beta = 0.01, plot = T)
# isr_sdpop_mvarcp <- cpt.meanvar(ts(data$sd_isrevent_pop), method = 'PELT', penalty = 'Manual', pen.value = '3.5*log(n)', minseglen = 12)
isr_sdpop_mvarcp <- cpt.meanvar(ts(data$sd_isrevent_pop), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(isr_sdpop_mvarcp)
abline(v = isr_sdpop_breakout$loc, col = 'red', lty = 3)
# Targets in Israel vs palestine
isr_loc_breakout <- breakout(data$mean_isrevent_pal, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# isr_loc_mvarcp <- cpt.meanvar(ts(data$mean_isrevent_pal), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
isr_loc_mvarcp <- cpt.meanvar(ts(data$mean_isrevent_pal), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(isr_loc_mvarcp)
abline(v = isr_loc_breakout$loc, col = 'red', lty = 3)
# Civilian vs military targets
isr_civtarg_breakout <- breakout(data$isr_civtargeting, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# isr_civtarget_mvarcp <- cpt.meanvar(ts(data$isr_civtargeting), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
isr_civtarget_mvarcp <- cpt.meanvar(ts(data$isr_civtargeting), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(pal_civtarget_mvarcp)
abline(v = isr_civtarg_breakout$loc, col = 'red', lty = 3)
## Tactics
# Reliance on direct-fire technology
isr_direct_breakout <- breakout(data$isr_direct, min.size = 12, method = 'multi', beta = 0.008, plot = T)
# isr_direct_mvarcp <- cpt.meanvar(ts(data$isr_direct), method = 'PELT', penalty = 'Manual', pen.value = '5*log(n)', minseglen = 12)
isr_direct_mvarcp <- cpt.meanvar(ts(data$isr_direct), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(isr_direct_mvarcp)
abline(v = isr_direct_breakout$loc, col = 'red', lty = 3)
# Reliance on heavy weapons
isr_bigtech_breakout <- breakout(data$isr_bigtech, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# isr_bigtech_mvarcp <- cpt.meanvar(ts(data$isr_bigtech), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
isr_bigtech_mvarcp <- cpt.meanvar(ts(data$isr_bigtech), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(isr_bigtech_mvarcp)
abline(v = isr_bigtech_breakout$loc, col = 'red', lty = 3)
# Reliance on shelling
isr_shelling_breakout <- breakout(data$isr_shelling, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# isr_shelling_mvarcp <- cpt.meanvar(ts(data$isr_shelling), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
isr_shelling_mvarcp <- cpt.meanvar(ts(data$isr_shelling), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(isr_shelling_mvarcp)
abline(v = isr_shelling_breakout$loc, col = 'red', lty = 3)
# Reliance on airstrikes
isr_airstrike_breakout <- breakout(data$isr_airstrike, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# isr_airstrike_mvarcp <- cpt.meanvar(ts(data$isr_airstrike), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
isr_airstrike_mvarcp <- cpt.meanvar(ts(data$isr_airstrike), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(isr_airstrike_mvarcp)
abline(v = isr_airstrike_breakout$loc, col = 'red', lty = 3)
# Reliance on incursions/raids into Palestine
isr_raid_breakout <- breakout(data$isr_raid, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# isr_raid_mvarcp <- cpt.meanvar(ts(data$isr_raid), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
isr_raid_mvarcp <- cpt.meanvar(ts(data$isr_raid), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(isr_raid_mvarcp)
abline(v = isr_raid_breakout$loc, col = 'red', lty = 3)
# Reliance on nonviolent tactics (bulldozing etc)
isr_nonviol_breakout <- breakout(data$isr_nonviol, min.size = 12, method = 'multi', beta = 0.01, plot = T)
# isr_nonviol_mvarcp <- cpt.meanvar(ts(data$isr_nonviol), method = 'PELT', penalty = 'Manual', pen.value = '2.5*log(n)', minseglen = 12)
isr_nonviol_mvarcp <- cpt.meanvar(ts(data$isr_nonviol), method = 'BinSeg', Q = 4, penalty = 'BIC', minseglen = 12)
plot(isr_nonviol_mvarcp)
abline(v = isr_nonviol_breakout$loc, col = 'red', lty = 3)

## Combined
library(bcp)
bcp_function <- function(inputs){
  for(i in 1:ncol(inputs)){
    inputs[, i] <- (inputs[, i] - mean(inputs[, i])) / sd(inputs[, i])
  }
  isr_combined1_ecp <- bcp(inputs)
  plot(isr_combined1_ecp, separated = T)  
}
# All behavioral elements
inputs <- xts(data[, list(isr_event, log(mean_isrevent_pop+1), isr_indirect, isr_civtargeting)], order.by = data$date)
bcp_function(inputs)

# Behavior breakdown (not event count)
inputs <- xts(data[, list(log(mean_isrevent_pop+1), isr_bigtech, isr_civtargeting)], order.by = data$date)
bcp_function(inputs)

# Location and target
inputs <- xts(data[, list(log(mean_isrevent_pop+1), isr_civtargeting)], order.by = data$date)
bcp_function(inputs)

# Technology and target
inputs <- xts(data[, list(isr_bigtech, isr_civtargeting)], order.by = data$date)
bcp_function(inputs)

# Technology and location
inputs <- xts(data[, list(isr_bigtech, log(mean_isrevent_pop+1))], order.by = data$date)
bcp_function(inputs)

#############################################################################
## Early modeling attempts
var_function <- function(inputs, exog){
  model1 <- VAR(inputs, p = 4, type = 'both')
  print(summary(model1))
  irf_m1 <- irf(model1)
  plot(irf_m1)
  return(model1)
}

# Israeli and Palestinian actions
plot(pal_int_mvarcp)
pal_int_mvarcp
# Period 1
inputs = xts(data[14:75, list(isr_event, pal_event)], order.by = data$week[14:75])
exog = xts(data[14:75, list(isrdead_mo, paldead_mo)], order.by = data$week[14:75])
var_function(inputs, exog)
# Period 2
inputs = xts(data[76:229, list(isr_event, pal_event)], order.by = data$week[76:229])
exog = xts(data[76:229, list(isrdead_mo, paldead_mo)], order.by = data$week[76:229])
var_function(inputs, exog)

inputs = xts(data[, list(isr_event, pal_event)], order.by = data$week)
exog = xts(data[, list(isrdead_mo, paldead_mo)], order.by = data$week)

# Israeli casualties and Israeli actions
inputs = xts(data[, list(isrdead_mo, isr_event)], order.by = data$week)
exog = xts(data[, list(isr_event, pal_event)], order.by = data$week)
var_function(inputs, exog)

# Palestinian casualties and Palestinian targeting civilians
inputs = xts(data[, list(paldead_mo, pal_civtargeting)], order.by = data$week)
exog = xts(data[, list(isr_event, pal_event, isrdead_mo)], order.by = data$week)
var_function(inputs, exog)

# Palestinian casualties and Palestinian indirect violence
pal_direct_mvarcp
plot(pal_direct_mvarcp)
inputs1 = xts(data[14:75, list(isr_bigtech, pal_indirect)], order.by = data$week[14:75])
exog1 = xts(data[14:75, list(isr_event, pal_event, isrdead_mo)], order.by = data$week[14:75])
var_function(inputs1, exog1)

inputs2 = xts(data[90:205, list(isr_bigtech, pal_indirect)], order.by = data$week[90:205])
exog2 = xts(data[90:205, list(isr_event, pal_event, isrdead_mo)], order.by = data$week[90:205])
var_function(inputs2, exog2)


# Israeli indirect fire and Palestinian targeting civilians
inputs = xts(data[, list(isr_indirect, pal_civtargeting)], order.by = data$week)
exog = xts(data[, list(pal_event, isr_event, paldead_mo, isrdead_mo)], order.by = data$week)
var_function(inputs, exog)

# Israeli events and Palestinian targeting civilians
inputs = xts(data[, list(isr_event, pal_civtargeting)], order.by = data$week)
exog = xts(data[, list(pal_event, paldead_mo, isrdead_mo)], order.by = data$week)
var_function(inputs, exog)

# Israeli defensive measures and Palestinian indirect fire
inputs = xts(data[, list(isr_nonviol, pal_shelling)], order.by = data$week)
exog = xts(data[, list(pal_event, isr_event, paldead_mo, isrdead_mo)], order.by = data$week)
var_function(inputs, exog)

# Palestinian targeting civilians and Israeli heavy weaponry
inputs = xts(data[, list(pal_civtargeting, isr_bigtech)], order.by = data$week)
exog = xts(data[, list(pal_event, isr_event, paldead_mo, isrdead_mo)], order.by = data$week)
var_function(inputs, exog)

# Palestinian targeting civilians and Israeli force projection
inputs = xts(data[, list(pal_civtargeting, isr_raid)], order.by = data$week)
exog = xts(data[, list(pal_event, isr_event, paldead_mo, isrdead_mo)], order.by = data$week)
var_function(inputs, exog)

# Palestinian targeting civilians and Israeli force projection
inputs = xts(data[, list(pal_civtargeting, isr_raid, isr_airstrike)], order.by = data$week)
exog = xts(data[, list(pal_event, isr_event, paldead_mo, isrdead_mo)], order.by = data$week)
var_function(inputs, exog)


