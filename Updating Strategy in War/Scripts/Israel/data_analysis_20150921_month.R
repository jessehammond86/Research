rm(list = ls())
# setwd('/Users/macbook/Dropbox/Dissertation/Data/Israel')
setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel')
# setwd('/media/jesse/Files/Dropbox/Dissertation/Data/Israel')
library(foreign)
library(data.table)
library(TSA)
library(vars)
library(zoo)
library(xts)
library(lubridate)
library(changepoint)
library(bcp)
library(ecp)
# Read in data
data <- fread('isr_eventdata.csv')
# Data setup
old_data <- data
set.seed(10002)
data <- old_data
setkeyv(data, c('year', 'month', 'day'))
# Create a WEEK count
data$date <- as.Date(paste(data$year, data$month, data$day, sep = '-'), format = '%Y-%m-%d')
data <- data[date > as.Date('2001-01-01') & date < as.Date('2005-03-01')]

# data$week <- floor_date(data$date, 'month')
### ROUND DATES DOWN TO MONTH
data$week <- floor_date(data$date, 'month')
# data$week <- floor_date(data$date, 'week')
# data$week <- data$week - (min(data$week)-1)
# Drop a couple of miscoded locations
data <- data[lat > 30]
data <- data[mindist_border < 100]

# Deal with actions with UNKNOWN outcomes
data[isr_noncom == 555, isr_noncom := median(isr_noncom[isr_noncom != 555], na.rm = T)]
data[pal_milita == 555, pal_milita := median(pal_milita[pal_milita != 555], na.rm = T)]
data[isr_milita == 555, isr_milita := median(isr_milita[isr_milita != 555], na.rm = T)]
data[propertyda == 555, propertyda := median(propertyda[propertyda != 555], na.rm = T)]

# Actor setup
pal_milactors = c('palgun', 'palmil', 'palpol', 'fatah', 'hamas', 'ij', 'pflp', 'prc', 'dflp', 'pflp')
pal_civactors = c('palgov', 'palciv', 'palag', 'palres', 'palind')
isr_milactors = c('isrpol', 'idf')
isr_civactors = c('isrgov', 'isrciv', 'isrres', 'isrpol')
small_arms = c('anti-tank missile', 'anti-tank missiles', 'grenade', 'knife', 'medium arms', 'small arms',
               'stones', 'tear gas', 'rubber bullets', 'concussion grenade', 'concussion grenades'
               
               , 'artillery')

big_arms = c('aircraft', 'belt', 'car bomb', 'drone', 'explosives', 'fighter jets',
             'heavy arms', 'helicopter', 'helicopters', 'land-land missile', 'land-land missiles',
             'mortar', 'rockets', 'shelling')

direct_fire = c('anti_tank missile', 'anti_tank missiles', 'grenade', 'knife', 'medium arms', 'small arms',
                'stones', 'tear gas', 'rubber bullets', 'concussion grenade', 'concussion grenades',
                'heavy arms')

indirect_fire = c('aircraft', 'belt', 'car bomb', 'drone', 'explosives', 'fighter jets',
                  'helicopter', 'helicopters', 'land-land missile', 'land-land missiles',
                  'mortar', 'rockets', 'shelling')

violent_events = c('shelling', 'shooting', 'beating', 'bombing', 'firefight', 'raid', 'air strike', 'shelrock')

nonviol_events = c('bulldozing', 'crowd control', 'detainment', 'fortification', 'movement restriction', 'vandalism'
                   , 'border closure', 'clash')

### ONLY INCLUDING VIOLENT EVENTS
data <- data[interactio %in% violent_events]
data <- data[!(interactio %in% 'raid' & technology %in% 'medium arms' & pal_combin == 0 & is.na(context))]
### Types of location
# Population
data[, mean_palevent_pop := mean(as.numeric(Population[actor1 %in% pal_milactors | interactio %in% 'firefight']), na.rm = T), by = list(week)]
data[, sd_palevent_pop := sd(as.numeric(Population[actor1 %in% pal_milactors | interactio %in% 'firefight']), na.rm = T), by = list(week)]
data[is.na(mean_palevent_pop), mean_palevent_pop := 0]
data[is.na(sd_palevent_pop), sd_palevent_pop := 0]
data[, mean_isrevent_pop := mean(as.numeric(Population[actor1 %in% isr_milactors]), na.rm = T), by = list(week)]
data[, sd_isrevent_pop := sd(as.numeric(Population[actor1 %in% isr_milactors]), na.rm = T), by = list(week)]
data[is.na(mean_isrevent_pop), mean_isrevent_pop := 0]
data[is.na(sd_isrevent_pop), sd_isrevent_pop := 0]

# Israel vs palestine location
data[, mean_palevent_pal := mean(palestine[actor1 %in% pal_milactors | interactio %in% 'firefight'], na.rm = T), by = list(week)]
data[is.na(mean_palevent_pal), mean_palevent_pal := mean(data$mean_palevent_pal, na.rm = T)]
data[, mean_isrevent_pal := mean(palestine[actor1 %in% isr_milactors], na.rm = T), by = list(week)]
data[is.na(mean_isrevent_pal), mean_isrevent_pal := mean(data$mean_isrevent_pal, na.rm = T)]

# Distance from border
data[, mean_palevent_bdist := mean(mindist_border[actor1 %in% pal_milactors | interactio %in% 'firefight'], na.rm = T), by = list(week)]
data[, sd_palevent_bdist := sd(mindist_border[actor1 %in% pal_milactors | interactio %in% 'firefight'], na.rm = T), by = list(week)]
data[is.na(mean_palevent_bdist), mean_palevent_bdist := mean(data$mean_palevent_bdist, na.rm = T)]
data[is.na(sd_palevent_bdist), sd_palevent_bdist := max(data$sd_palevent_bdist, na.rm = T)]
data[, mean_isrevent_bdist := mean(mindist_border[actor1 %in% isr_milactors], na.rm = T), by = list(week)]
data[, sd_isrevent_bdist := sd(mindist_border[actor1 %in% isr_milactors], na.rm = T), by = list(week)]
data[is.na(mean_isrevent_bdist), mean_isrevent_bdist := mean(data$mean_isrevent_bdist, na.rm = T)]
data[is.na(sd_isrevent_bdist), sd_isrevent_bdist := mean(data$sd_isrevent_bdist, na.rm = T)]

### Casualties
data[, paldead_mo := sum(pal_fatali), by = list(week)]
data[, paldead_civ_mo := sum(pal_nonc_1), by = list(week)]
data[, paldead_milt_mo := sum(pal_mili_1), by = list(week)]
data[, paldead_mili_mo := sum(pal_mili_2), by = list(week)]
data[is.na(paldead_mo), paldead_mo := 0]
data[is.na(paldead_civ_mo), paldead_civ_mo := 0]
data[is.na(paldead_mili_mo), paldead_mili_mo := 0]
data[is.na(paldead_milt_mo), paldead_milt_mo := 0]
data[, paldead_mil_mo := paldead_milt_mo + paldead_mili_mo, by = list(week)]
data[, paldead_mili_mo := NULL]
data[, paldead_milt_mo := NULL]

data[, isrdead_mo := sum(isr_fatali), by = list(week)]
data[, isrdead_civ_mo := sum(isr_nonc_1), by = list(week)]
data[, isrdead_mil_mo := sum(isr_combat), by = list(week)]
data[is.na(isrdead_mo), isrdead_mo := 0]
data[is.na(isrdead_civ_mo), isrdead_civ_mo := 0]
data[is.na(isrdead_mil_mo), isrdead_mil_mo := 0]

data[, palwound_mo := sum(pal_combin), by = list(week)]
data[, palwound_civ_mo := sum(pal_noncom), by = list(week)]
data[, palwound_mil_mo := sum(pal_milita), by = list(week)]
data[is.na(palwound_mo), palwound_mo := 0]
data[is.na(palwound_civ_mo), palwound_civ_mo := 0]
data[is.na(palwound_mil_mo), palwound_mil_mo := 0]

data[, isrwound_civ_mo := sum(isr_noncom), by = list(week)]
data[, isrwound_mil_mo := sum(isr_milita), by = list(week)]
data[, isrwound_pol_mo := sum(isr_police), by = list(week)]
data[, isrwound_mil_mo := isrwound_mil_mo + isrwound_pol_mo, by = list(week)]
data[, isrwound_pol_mo := NULL]
data[is.na(isrwound_civ_mo), isrwound_civ_mo := 0]
data[is.na(isrwound_mil_mo), isrwound_mil_mo := 0]
data[, isrwound_mo := isrwound_civ_mo + isrwound_mil_mo]
data[is.na(isrwound_mo), isrwound_mo := 0]

data[, palcas_mo := paldead_mo + palwound_mo]
data[, isrcas_mo := isrdead_mo + isrwound_mo]

data[, pal_casratio := 0.0]
data[, isr_casratio := 0.0]
data[(palcas_mo + isrcas_mo) > 0, pal_casratio := (palcas_mo) / (isrcas_mo + palcas_mo)]
data[(palcas_mo + isrcas_mo) > 0, isr_casratio := (isrcas_mo) / (isrcas_mo + palcas_mo)]
# data[is.na(pal_casratio), pal_casratio := mean(pal_casratio, na.rm = T)]
# data[is.na(isr_casratio), isr_casratio := mean(isr_casratio, na.rm = T)]
data[, pal_casratio_w := pal_casratio * palcas_mo]
data[, isr_casratio_w := isr_casratio * isrcas_mo]

### Interactions
data[, event := 1.0]
data[, pal_event := sum(event == 1 & (actor1 %in% pal_milactors & interactio %in% violent_events) | (interactio == 'firefight')), by = list(week)]
data[, isr_event := sum(event == 1 & (actor1 %in% isr_milactors & interactio %in% violent_events) | (interactio == 'firefight')), by = list(week)]
data[, mut_event := sum(event == 1 & interactio %in% 'firefight'), by = list(week)]

# Shootings
data[, pal_shooting := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shooting')), by = list(week)]
data[pal_event > 0, pal_shooting := pal_shooting / pal_event, by = list(week)]
data[, isr_shooting := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shooting')), by = list(week)]
data[isr_event > 0, isr_shooting := isr_shooting / isr_event, by = list(week)]

# Firefights
data[, pal_firefight := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'firefight')), by = list(week)]
data[pal_event > 0, pal_firefight := pal_firefight / pal_event, by = list(week)]
data[, isr_firefight := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'firefight')), by = list(week)]
data[isr_event > 0, isr_firefight := isr_firefight / isr_event, by = list(week)]

# Shellings
data[, pal_shelling := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shelling')), by = list(week)]
data[pal_event > 0, pal_shelling := pal_shelling / pal_event, by = list(week)]
data[, pal_smallshelling := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shelling' & technolo_1 %in% small_arms)), by = list(week)]
data[pal_event > 0, pal_smallshelling := pal_smallshelling / pal_event, by = list(week)]
data[, pal_bigshelling := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shelling' & technolo_1 %in% big_arms)), by = list(week)]
data[pal_event > 0, pal_bigshelling := pal_bigshelling / pal_event, by = list(week)]

data[, isr_shelling := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shelling')), by = list(week)]
data[isr_event > 0, isr_shelling := isr_shelling / isr_event, by = list(week)]
data[, isr_smallshelling := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shelling' & technolo_1 %in% small_arms)), by = list(week)]
data[isr_event > 0, isr_smallshelling := isr_smallshelling / isr_event, by = list(week)]
data[, isr_bigshelling := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shelling' & technolo_1 %in% big_arms)), by = list(week)]
data[isr_event > 0, isr_bigshelling := isr_bigshelling / isr_event, by = list(week)]

# Bombings
data[, pal_bombing := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'bombing')), by = list(week)]
data[pal_event > 0, pal_bombing := pal_bombing / pal_event, by = list(week)]
data[, pal_suicidebombing := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'bombing' & (context %in% 'suicide' | technolo_1 %in% 'belt'))), by = list(week)]
data[pal_event > 0, pal_suicidebombing := pal_suicidebombing / pal_event, by = list(week)]
data[, isr_bombing := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'bombing')), by = list(week)]
data[isr_event > 0, isr_bombing := isr_bombing / isr_event, by = list(week)]

# Raids
data[, pal_raid := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'raid')), by = list(week)]
data[pal_event > 0, pal_raid := pal_raid / pal_event, by = list(week)]
data[, isr_raid := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'raid')), by = list(week)]
data[isr_event > 0, isr_raid := isr_raid / isr_event, by = list(week)]
data[, isr_smallraid := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'raid' & technolo_1 %in% small_arms)), by = list(week)]
data[isr_event > 0, isr_smallraid := isr_smallraid / isr_event, by = list(week)]
data[, isr_bigraid := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'raid' & technolo_1 %in% big_arms)), by = list(week)]
data[isr_event > 0, isr_bigraid := isr_bigraid / isr_event, by = list(week)]

# Beatings 
data[, pal_beating := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'beating')), by = list(week)]
data[pal_event > 0, pal_beating := pal_beating / pal_event, by = list(week)]

# Clashes
data[, pal_clash := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'crowd control')), by = list(week)]
data[pal_event > 0, pal_clash := pal_clash / pal_event, by = list(week)]

# Air strikes
data[, isr_airstrike := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'air strike')), by = list(week)]
data[isr_event > 0, isr_airstrike := isr_airstrike / isr_event, by = list(week)]

# Nonviolent/defensive
data[, isr_nonviol := as.numeric(sum(actor1 %in% isr_milactors & interactio %in% c('bulldozing', 'detainment', 'movement restriction'))), by = list(week)]
data[isr_event > 0, isr_nonviol := isr_nonviol / isr_event, by = list(week)]

### Type of technology
# Heavy vs light arms
data[, pal_bigtech := as.numeric(sum(actor1 %in% pal_milactors & technology %in% big_arms)), by = list(week)]
data[, pal_smalltech := as.numeric(sum(actor1 %in% pal_milactors & technology %in% small_arms)), by = list(week)]
data[pal_event > 0, pal_bigtech := pal_bigtech / pal_event, by = list(week)]
data[, isr_bigtech := as.numeric(sum(actor1 %in% isr_milactors & technology %in% big_arms)), by = list(week)]
data[, isr_smalltech := as.numeric(sum(actor1 %in% isr_milactors & technology %in% small_arms)), by = list(week)]
data[isr_event > 0, isr_bigtech := isr_bigtech / isr_event, by = list(week)]

# Direct vs indirect fire
data[, pal_direct := as.numeric(sum((actor1 %in% pal_milactors & technology %in% direct_fire) | (interactio %in% 'firefight'))), by = list(week)]
data[pal_event > 0, pal_direct := pal_direct / pal_event, by = list(week)]
data[, pal_indirect := as.numeric(sum(actor1 %in% pal_milactors & technology %in% indirect_fire)), by = list(week)]
data[pal_event > 0, pal_indirect := pal_indirect / pal_event, by = list(week)]
data[, isr_direct := as.numeric(sum((actor1 %in% isr_milactors & technology %in% direct_fire) | (interactio %in% 'firefight'))), by = list(week)]
data[isr_event > 0, isr_direct := isr_direct / isr_event, by = list(week)]
data[, isr_indirect := as.numeric(sum(actor1 %in% isr_milactors & technology %in% indirect_fire)), by = list(week)]
data[isr_event > 0, isr_indirect := isr_indirect / isr_event, by = list(week)]

# On-site vs remote attacks
data[, pal_remote := as.numeric(sum(actor1 %in% pal_milactors & (technology %in% indirect_fire | interactio %in% 'shelling'))), by = list(week)]
data[pal_event > 0, pal_remote := pal_remote / pal_event, by = list(week)]


### Targets
data[, pal_civtargeting := as.numeric(sum(actor1 %in% pal_milactors & actor2 %in% isr_civactors & interactio %in% violent_events)), by = list(week)]
data[pal_event > 0, pal_civtargeting := pal_civtargeting / pal_event, by = list(week)]
data[, pal_miltargeting := as.numeric(sum(actor1 %in% pal_milactors & actor2 %in% isr_milactors & interactio %in% violent_events)), by = list(week)]
data[pal_event > 0, pal_miltargeting := pal_miltargeting / pal_event, by = list(week)]

data[, isr_civtargeting := as.numeric(sum(actor1 %in% isr_milactors & actor2 %in% pal_civactors & interactio %in% violent_events)), by = list(week)]
data[isr_event > 0, isr_civtargeting := isr_civtargeting / isr_event, by = list(week)]
data[, isr_miltargeting := as.numeric(sum(actor1 %in% isr_milactors & actor2 %in% pal_milactors & interactio %in% violent_events)), by = list(week)]
data[isr_event > 0, isr_miltargeting := isr_miltargeting / isr_event, by = list(week)]


### Targeted Killings
data[, isr_assassination := as.numeric(sum(actor1 %in% isr_milactors 
                                          & context %in% 'assassination')), by = list(week)]
data[isr_event > 0, isr_assassination := isr_assassination / isr_event, by = list(week)]


### Events
# data[, pal_eventcount := as.numeric(sum(pal_event)), by = list(week)]
# data[, isr_eventcount := as.numeric(sum(isr_event)), by = list(week)]

## Test
# data[, pal_indirect := pal_indirect * pal_event]
# data[, pal_remote := pal_remote * pal_event]
# data[, pal_bombing := pal_bombing * pal_event]
# data[, pal_suicidebombing := pal_suicidebombing * pal_event]
# data[, pal_civtargeting := pal_civtargeting * pal_event]
# data[, pal_miltargeting := pal_miltargeting * pal_event]
# 
# data[, isr_bigtech := isr_bigtech * isr_event]
# data[, isr_raid := isr_raid * isr_event]
# data[, isr_bigraid := isr_bigraid * isr_event]
# data[, isr_assassination := isr_assassination * isr_event]
# data[, isr_civtargeting := isr_civtargeting * isr_event]
# data[, isr_miltargeting := isr_miltargeting * isr_event]


### Collapse to time series format
old_data2 <- data
#data <- old_data2
# data <- data[!duplicated(data[, week]), ]
data <- data[!duplicated(data[, week]), ]
# data <- data[!duplicated(data[, list(date)]), ]
# data <- data[!duplicated(data[, list(year,month)]), ]


#################################################################
Nsmooth <- 3

### Palestinian strategy
## Activity
inputs0 <- SMA(xts(data[, list(pal_event)], order.by = data$date), n = Nsmooth)
# inputs1 <- xts(data[, list(mean_palevent_bdist)], order.by = data$date)
pal_event <- inputs0[!is.na(inputs0)]

## Location
# inputs1 <- SMA(xts(data[, list(log(mean_palevent_pop+1))], order.by = data$date), n = Nsmooth)
# inputs1 <- xts(data[, list(log(mean_palevent_pop+1))], order.by = data$date)
inputs1 <- SMA(xts(data[, list(mean_palevent_bdist)], order.by = data$date), n = Nsmooth)
# inputs1 <- xts(data[, list(mean_palevent_bdist)], order.by = data$date)
mean_palevent_bdist <- inputs1[!is.na(inputs1)]

## Technology
inputs2 <- SMA(xts(data[, list(pal_remote)], order.by = data$date), n = Nsmooth)
# inputs2 <- xts(data[, list(pal_remote)], order.by = data$date)
pal_indirect <- inputs2[!is.na(inputs2)]
inputs2 <- SMA(xts(data[, list(pal_bombing)], order.by = data$date), n = Nsmooth)
# inputs2 <- xts(data[, list(pal_remote)], order.by = data$date)
pal_bombing <- inputs2[!is.na(inputs2)]
inputs2 <- SMA(xts(data[, list(pal_suicidebombing)], order.by = data$date), n = Nsmooth)
# inputs2 <- xts(data[, list(pal_remote)], order.by = data$date)
pal_suicidebombing <- inputs2[!is.na(inputs2)]

## Target
inputs3 <- SMA(xts(data[, list(pal_civtargeting)], order.by = data$date), n = Nsmooth)
# inputs3 <- xts(data[, list(pal_civtargeting)], order.by = data$date)
pal_civtargeting <- inputs3[!is.na(inputs3)]
inputs3 <- SMA(xts(data[, list(pal_miltargeting)], order.by = data$date), n = Nsmooth)
# inputs3 <- xts(data[, list(pal_civtargeting)], order.by = data$date)
pal_miltargeting <- inputs3[!is.na(inputs3)]


plot(pal_event)
plot(mean_palevent_bdist)
plot(pal_indirect)
plot(pal_bombing)
plot(pal_suicidebombing)
plot(pal_civtargeting)
plot(pal_miltargeting)

### Israeli strategy

## Activity
inputs0 <- SMA(xts(data[, list(isr_event)], order.by = data$date), n = Nsmooth)
# inputs1 <- xts(data[, list(mean_isrevent_bdist)], order.by = data$date)
isr_event <- inputs0[!is.na(inputs0)]

## Location
# inputs1 <- EMA(xts(data[, list(log(mean_isrevent_pop+1))], order.by = data$date), n = Nsmooth)
# inputs1 <- xts(data[, list(log(mean_isrevent_pop+1))], order.by = data$date)
inputs1 <- SMA(xts(data[, list(mean_isrevent_bdist)], order.by = data$date), n = Nsmooth)
# inputs1 <- xts(data[, list(mean_isrevent_bdist)], order.by = data$date)
mean_isrevent_bdist <- inputs1[!is.na(inputs1)]

## Technology
inputs2 <- SMA(xts(data[, list(isr_bigtech)], order.by = data$date), n = Nsmooth)
# inputs2 <- xts(data[, list(isr_bigtech)], order.by = data$date)
isr_bigtech <- inputs2[!is.na(inputs2)]

## Target
inputs3 <- SMA(xts(data[, list(isr_civtargeting)], order.by = data$date), n = Nsmooth)
# inputs3 <- xts(data[, list(isr_civtargeting)], order.by = data$date)
isr_civtargeting <- inputs3[!is.na(inputs3)]
inputs3 <- SMA(xts(data[, list(isr_miltargeting)], order.by = data$date), n = Nsmooth)
# inputs3 <- xts(data[, list(pal_civtargeting)], order.by = data$date)
isr_miltargeting <- inputs3[!is.na(inputs3)]
inputs3 <- SMA(xts(data[, list(isr_assassination)], order.by = data$date), n = Nsmooth)
# inputs3 <- xts(data[, list(pal_civtargeting)], order.by = data$date)
isr_assassination <- inputs3[!is.na(inputs3)]

## Casualty Ratio
inputs4 <- SMA(xts(data[, list(pal_casratio)], order.by = data$date), n = Nsmooth)
# inputs4 <- xts(data[, list(pal_casratio)], order.by = data$date)
pal_casratio <- inputs4[!is.na(inputs4)]

plot(isr_event)
plot(mean_isrevent_bdist)
plot(isr_bigtech)
plot(isr_assassination)
plot(isr_civtargeting)
plot(isr_miltargeting)

#############################################################################
#############################################################################
input_data <- cbind(pal_event, mean_palevent_bdist
                    , pal_indirect, pal_bombing
                    , pal_suicidebombing
                    , pal_civtargeting, pal_civtargeting
                    , isr_event, mean_isrevent_bdist
                    , isr_bigtech, isr_assassination
                    , isr_civtargeting, isr_miltargeting
                    , pal_casratio)
input_data <- scale(input_data)
week <- as.Date(index(input_data))
input_data <- data.table(as.matrix(input_data))
input_data[, week := week]
setnames(input_data, c('pal_event', 'mean_palevent_bdist'
                       , 'pal_indirect', 'pal_bombing'
                       , 'pal_suicidebombing'
                       , 'pal_civtargeting', 'pal_miltargeting'
                       , 'isr_event', 'mean_isrevent_bdist'
                       , 'isr_bigtech', 'isr_assassination'
                       , 'isr_civtargeting', 'isr_miltargeting'
                       , 'pal_casratio'
                       , 'week'))
input_data <- input_data[complete.cases(input_data), ]


#####################################################################################
## MAIN ANALYSIS: These time series are likely highly endogenous, so ARIMAX is probably not the best approach.
## Using VAR models will allow me to assess the endogenized effects of a shock in one system on a shock in the other.
## VAR TS modeling
var_function <- function(inputs, exog = NULL){
  model1 <- VAR(inputs, exogen = exog, ic = 'AIC', type = 'none')
  print(summary(model1))
  return(model1)
}

#####################################################################################
#### ENDOGENOUS SYSTEMS
exog = cbind(lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week)),
             lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 2))
exog[is.na(exog)] <- 0

### 4-WAY INTERACTIONS
## PAL TARG UP - ISR TARG UP - ISR TECH UP - PAL TECH UP
inputs = xts(input_data[,list(pal_civtargeting, isr_civtargeting, isr_bigtech, pal_indirect
)], order.by = input_data$week)
model1 <- var_function(inputs, exog = exog)
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, cumulative = T)
dev.off()
pdf(file = 'itech_ptech_ptarg.pdf')
plot(irf_m1)
dev.off()

# ISR TECH UP - PAL TECH UP
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_bigtech', response = 'pal_indirect')
dev.off()
pdf(file = 'itechPtech.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR heavy weapons use', ylab = 'PAL indirect-fire weapons use', xlab = 'foo')
dev.off()

# ISR TECH UP - PAL TARG UP
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_bigtech', response = 'pal_civtargeting')
dev.off()
pdf(file = 'itechPtarg.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR heavy weapons use', ylab = 'PAL civilian targeting', xlab = 'foo')
dev.off()

# PAL TARG UP - ISR TECH UP
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'pal_civtargeting', response = 'isr_bigtech')
dev.off()
pdf(file = 'ptargItech.pdf', height = 5, width = 8)
plot(irf_m1, main = 'PAL civilian targeting', ylab = 'ISR heavy weapons use', xlab = 'foo')
dev.off()

# PAL TECH UP - ISR TECH UP
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'pal_indirect', response = 'isr_bigtech')
dev.off()
pdf(file = 'ptechItech.pdf', height = 5, width = 8)
plot(irf_m1, main = 'PAL indirect-fire weapons use', ylab = 'ISR heavy weapons use', xlab = 'foo')
dev.off()


## ISR ACT UP - ISR TARG DOWN - PAL ACT DOWN - ISR TECH DOWN
inputs = xts(input_data[,list(isr_event, isr_civtargeting, pal_event, isr_bigtech
)], order.by = input_data$week)
exog[is.na(exog)] <- 0
model1 <- var_function(inputs, exog = exog)
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000)
dev.off()
pdf(file = 'iact_itarg_pact_itech.pdf')
plot(irf_m1)
dev.off()

# ISR ACT UP - PAL ACT DOWN
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_event', response = 'pal_event')
dev.off()
pdf(file = 'iactPact.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR activity', ylab = 'PAL activity ', xlab = 'foo')
dev.off()

# ISR TARG DOWN - PAL ACT DOWN
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_civtargeting', response = 'pal_event')
dev.off()
pdf(file = 'itargPact.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR militant targeting', ylab = 'PAL activity ', xlab = 'foo')
dev.off()

# PAL ACT DOWN - ISR TECH DOWN
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'pal_event', response = 'isr_bigtech')
dev.off()
pdf(file = 'pactItech.pdf', height = 5, width = 8)
plot(irf_m1, main = 'PAL activity', ylab = 'ISR heavy weapons use ', xlab = 'foo')
dev.off()
