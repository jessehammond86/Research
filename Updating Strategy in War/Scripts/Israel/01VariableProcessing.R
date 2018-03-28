rm(list = ls())
setwd('/Users/jesse/Dropbox/Dissertation/Data/Israel')
# setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel')
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
library(TTR)
library(tsDyn)
library(forecast)
library(sp)
library(rgeos)
library(rgdal)
library(spdep)


# Load Palestinian area A/B/C shapes

# I am treating Gaza as functionally equivalent to Area A in the West Bank
gaza <- readOGR(paste0(getwd(), '/IsraelGIS/Data'), 'PSE_adm1')
gaza <- gaza[gaza$NAME_1 == 'Gaza', ]
gaza@data <- data.frame('ID' = 74)

area_a <- readOGR(paste0(getwd(), '/PalestineGIS'), 'areaAB')
area_a@data <- data.frame('ID' = 1:73)
area_a <- SpatialPolygonsDataFrame(gUnion(gaza, area_a), data = data.frame('ID' = 1))

area_c <- readOGR(paste0(getwd(), '/PalestineGIS'), 'areaC')


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

coords <- cbind(data$long, data$lat)
sp <- SpatialPoints(coords)
data <- SpatialPointsDataFrame(coords, data)

proj4string(data) <- CRS(proj4string(area_a))
data$area_ab <- !is.na(over(data, area_a))[, 1]
data$area_c <- !is.na(over(data, area_c))[, 1]
data <- as.data.table(data@data)
# data$week <- floor_date(data$date, 'month')
### ROUND DATES DOWN TO MONTH
# data$week <- floor_date(data$date, 'month')
data$week <- floor_date(data$date, 'week')
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
               'stones', 'tear gas', 'rubber bullets', 'concussion grenade', 'concussion grenades')

big_arms = c('aircraft', 'belt', 'car bomb', 'drone', 'explosives', 'fighter jets',
             'heavy arms', 'helicopter', 'helicopters', 'land-land missile', 'land-land missiles',
             'mortar', 'rockets', 'shelling', 'artillery')

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

# Big city (population over 20,000)
data[, city := ifelse(Population > 20000, 1, 0)]
data[, mean_isrevent_city := mean(city[actor1 %in% isr_milactors | interactio %in% 'firefight'], na.rm = T), by = list(week)]
data[is.na(mean_isrevent_city), mean_isrevent_city := mean(data$mean_isrevent_city, na.rm = T)]

# Israel vs palestine location
data[, mean_palevent_pal := mean(palestine[actor1 %in% pal_milactors | interactio %in% 'firefight'], na.rm = T), by = list(week)]
data[is.na(mean_palevent_pal), mean_palevent_pal := mean(data$mean_palevent_pal, na.rm = T)]
data[, mean_isrevent_pal := mean(palestine[actor1 %in% isr_milactors], na.rm = T), by = list(week)]
data[is.na(mean_isrevent_pal), mean_isrevent_pal := mean(data$mean_isrevent_pal, na.rm = T)]


# Palestinian area A/B location
data[, mean_palevent_area_ab := mean(area_ab[actor1 %in% pal_milactors | interactio %in% 'firefight'], na.rm = T), by = list(week)]
data[is.na(mean_palevent_area_ab), mean_palevent_area_ab := mean(data$mean_palevent_area_ab, na.rm = T)]
data[, mean_isrevent_area_ab := mean(area_ab[actor1 %in% isr_milactors | interactio %in% 'firefight'], na.rm = T), by = list(week)]
data[is.na(mean_isrevent_area_ab), mean_isrevent_area_ab := mean(data$mean_isrevent_area_ab, na.rm = T)]

# Palestinian area C location
data[, mean_palevent_area_c := mean(area_c[actor1 %in% pal_milactors | interactio %in% 'firefight'], na.rm = T), by = list(week)]
data[is.na(mean_palevent_area_c), mean_palevent_area_c := mean(data$mean_palevent_area_c, na.rm = T)]
data[, mean_isrevent_area_c := 1 - mean_isrevent_area_ab, by = list(week)]


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
data[is.na(pal_casratio), pal_casratio := mean(pal_casratio, na.rm = T)]
data[is.na(isr_casratio), isr_casratio := mean(isr_casratio, na.rm = T)]
data[, pal_casratio_w := pal_casratio * palcas_mo]
data[, isr_casratio_w := isr_casratio * isrcas_mo]

data[, pal_cas := pal_fatali + pal_combin]
data[, isr_cas := isr_fatali + isr_noncom + isr_milita + isr_police]
data[, all_cas := pal_cas + isr_cas]
data[, actor1_side := 'PAL']
data[(actor1 %in% isr_milactors | actor1 %in% isr_civactors), actor1_side := 'ISR']
data[, actor2_side := 'CIV']
data[(actor2 %in% isr_milactors | actor2 %in% pal_milactors), actor2_side := 'MIL']
data[, event_count := .N, by = list(actor1_side, lat, long)]

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
data[, isr_nonviol := as.numeric(sum(actor1 %in% isr_milactors & interactio %in% c('bulldozing', 'detainment', 'movement restriction')))
     , by = list(week)]
data[isr_event > 0, isr_nonviol := isr_nonviol / isr_event, by = list(week)]

### Type of technology
# Heavy vs light arms
data[, pal_bigtech := as.numeric(sum(actor1 %in% pal_milactors & technology %in% big_arms)), by = list(week)]
data[, pal_smalltech := as.numeric(sum(actor1 %in% pal_milactors & technology %in% small_arms)), by = list(week)]
data[pal_event > 0, pal_bigtech := pal_bigtech / pal_event, by = list(week)]
data[, isr_bigtech := as.numeric(sum(actor1 %in% isr_milactors & technology %in% big_arms)), by = list(week)]
data[isr_event > 0, isr_bigtech := isr_bigtech / isr_event, by = list(week)]
data[, isr_smalltech := as.numeric(sum(actor1 %in% isr_milactors & technology %in% small_arms)), by = list(week)]
data[isr_event > 0, isr_smalltech := isr_smalltech / isr_event, by = list(week)]

# Direct vs indirect fire
data[, pal_direct := as.numeric(sum((actor1 %in% pal_milactors & technology %in% direct_fire) | (interactio %in% 'firefight'))), by = list(week)]
data[pal_event > 0, pal_direct := pal_direct / pal_event, by = list(week)]
data[, pal_indirect := as.numeric(sum(actor1 %in% pal_milactors & technology %in% indirect_fire)), by = list(week)]
data[pal_event > 0, pal_indirect := pal_indirect / pal_event, by = list(week)]
data[, isr_direct := as.numeric(sum((actor1 %in% isr_milactors & technology %in% direct_fire) | (interactio %in% 'firefight'))), by = list(week)]
data[isr_event > 0, isr_direct := isr_direct / isr_event, by = list(week)]
data[, isr_indirect := as.numeric(sum(actor1 %in% isr_milactors & technology %in% indirect_fire)), by = list(week)]
data[isr_event > 0, isr_indirect := isr_indirect / isr_event, by = list(week)]

### Targets
data[, pal_civtargeting := as.numeric(sum(actor1 %in% pal_milactors & actor2 %in% isr_civactors & interactio %in% violent_events)), by = list(week)]
data[pal_event > 0, pal_civtargeting := pal_civtargeting / pal_event, by = list(week)]
data[, pal_miltargeting := as.numeric(sum(actor1 %in% pal_milactors & actor2 %in% isr_milactors & interactio %in% violent_events)), by = list(week)]
data[pal_event > 0, pal_miltargeting := pal_miltargeting / pal_event, by = list(week)]

data[, isr_civtargeting := as.numeric(sum(actor1 %in% isr_milactors & actor2 %in% pal_civactors & interactio %in% violent_events)), by = list(week)]
data[isr_event > 0, isr_civtargeting := isr_civtargeting / isr_event, by = list(week)]
data[, isr_miltargeting := as.numeric(sum(actor1 %in% isr_milactors & actor2 %in% pal_milactors & interactio %in% violent_events)), by = list(week)]
data[isr_event > 0, isr_miltargeting := isr_miltargeting / isr_event, by = list(week)]


### PAL Combined scores: more 'terrorism' vs more 'conventional'
data[, palscore := 0]
data[actor1 %in% pal_milactors, palscore := ifelse(technology %in% indirect_fire, 1, 0), ]
data[actor1 %in% pal_milactors, palscore := ifelse(actor2 %in% isr_civactors, palscore + 1, palscore)]
data[actor1 %in% pal_milactors, palscore := ifelse(area_c == 1, palscore + 1, palscore)]
data[!(actor1 %in% pal_milactors), palscore := NA]
data[, mean_palscore := mean(palscore, na.rm = T), by = week]
data[is.nan(mean_palscore), mean_palscore := NA]
data[is.na(mean_palscore), mean_palscore := mean(data$mean_palscore, na.rm = T)]

### ISR Combined scores: more 'widespread' vs more 'selective'
data[, isrscore := 0]
data[actor1 %in% isr_milactors, isrscore := ifelse(technology %in% big_arms, 1, 0), ]
data[actor1 %in% isr_milactors, isrscore := ifelse(actor2 %in% pal_civactors, isrscore + 1, isrscore)]
data[actor1 %in% isr_milactors, isrscore := ifelse(area_ab == 1, isrscore + 1, isrscore)]
data[!(actor1 %in% isr_milactors), isrscore := NA]
data[, mean_isrscore := mean(isrscore, na.rm = T), by = week]
data[is.nan(mean_isrscore), mean_isrscore := NA]
data[is.na(mean_isrscore), mean_isrscore := mean(data$mean_isrscore, na.rm = T)]



#### Write out processed data
write.csv(data, file = paste0(getwd(), '/01IsrPalFullData.csv'), row.names = F)
