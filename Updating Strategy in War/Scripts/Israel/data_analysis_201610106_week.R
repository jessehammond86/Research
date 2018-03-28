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
gaza <- readOGR('/Users/jesse/Dropbox/Dissertation/Data/Israel/IsraelGIS/Data', 'PSE_adm1')
gaza <- gaza[gaza$NAME_1 == 'Gaza', ]
gaza@data <- data.frame('ID' = 74)

area_a <- readOGR(paste0(getwd(), '/PalestineGIS'), 'areaAB')
area_a@data <- data.frame('ID' = 1:73)
test <- rbind(gaza, area_a, makeUniqueIDs = T)
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
               'stones', 'tear gas', 'rubber bullets', 'concussion grenade', 'concussion grenades'

               )

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
data[is.na(pal_casratio), pal_casratio := mean(pal_casratio, na.rm = T)]
data[is.na(isr_casratio), isr_casratio := mean(isr_casratio, na.rm = T)]
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

#################################################################
##
## MAKE SOME MAPS
##
######
library(ggmap)
library(rgdal)
library(rgeos)
library(raster)

### Casualties
data[, pal_cas := pal_fatali + pal_combin]
data[, isr_cas := isr_fatali + isr_noncom + isr_milita + isr_police]
data[, all_cas := pal_cas + isr_cas]
data[, actor1_side := 'PAL']
data[(actor1 %in% isr_milactors | actor1 %in% isr_civactors), actor1_side := 'ISR']
data[, actor2_side := 'CIV']
data[(actor2 %in% isr_milactors | actor2 %in% pal_milactors), actor2_side := 'MIL']
data[, event_count := .N, by = list(actor1_side, lat, long)]

### Subset data
# Gaza Strip
gaza_plotdata <- data[(actor1 %in% isr_milactors & interactio %in% c('raid', 'air strike', 'shooting'))
                      | (actor1 %in% pal_milactors & interactio %in% c('bombing', 'shelling', 'shooting'))]

gaza_plotdata_points <- gaza_plotdata[!duplicated(gaza_plotdata[, list(long, lat, actor1_side)])]

# West Bank
westbank_plotdata <- data[((actor1 %in% isr_milactors & interactio %in% c('raid', 'air strike', 'shooting'))
                      | (actor1 %in% pal_milactors & interactio %in% c('bombing', 'shelling', 'shooting')))
                      & palestine == 1 & long > 34.8]

westbank_plotdata_points <- westbank_plotdata[!duplicated(westbank_plotdata[, list(long, lat, actor1_side)])]

# Israel/Palestine total
israel_plotdata <- data[((actor1 %in% isr_milactors & interactio %in% c('raid', 'air strike', 'shooting'))
                           | (actor1 %in% pal_milactors & interactio %in% c('bombing', 'shelling', 'shooting')))]

israel_plotdata_points <- israel_plotdata[!duplicated(israel_plotdata[, list(long, lat, actor1_side)])]

### Grab maps

israel <- ggmap(get_map('rishon letsiyon', zoom = 9, color = 'bw'), maptype = 'roadmap', extent = 'device', legend = 'topleft')
gaza <- ggmap(get_map('Bureij', zoom = 11, color = 'bw'), maptype = 'terrain', extent = 'device', legend = 'topleft')
# westbank <- ggmap(get_map('rishon letsiyon', zoom = 9, color = 'bw'), maptype = 'roadmap', extent = 'device')

### Map some stuff

## GAZA EVENTS BY SIDE AND CASUALTY
dev.off()
pdf(file = 'gazaSideCas.pdf', height = 8, width = 8)
gaza + geom_point(aes(x = long, y = lat, color = actor1_side, shape = actor1_side, size = 2 + log(all_cas)), alpha = 0.75
                  , position = position_jitter(w = 0.01, h = 0.01)
                  , data = gaza_plotdata[actor1_side == 'PAL']) +
  geom_point(aes(x = long, y = lat, color = actor1_side, shape = actor1_side, size = 0.5 + log(all_cas)), alpha = 0.5
                  , position = position_jitter(w = 0.015, h = 0.015)
                  , data = gaza_plotdata[actor1_side == 'ISR']) +
  scale_shape('Actor', labels = c('ISR', 'PAL')) +
  scale_size_continuous('Log(dead+wounded)', range = c(1,5), guide = F) +
  scale_colour_discrete('Actor', labels = c('ISR', 'PAL')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()


## GAZA EVENTS BY COUNT AND TYPE
dev.off()
pdf(file = 'gazaPalCountType.pdf', height = 8, width = 8)
gaza + geom_point(aes(x = long, y = lat, color = interactio, size = log(all_cas)), alpha = 0.6
                  , position = position_jitter(w = 0.01, h = 0.01)
                  , data = gaza_plotdata[actor1_side == 'PAL']) +
  scale_size_continuous('Log(event count)', range = c(1,25), guide = F) +
  scale_colour_discrete('PAL Action', labels = c('bombing', 'shelling', 'shooting')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()

pdf(file = 'gazaIsrCountType.pdf', height = 8, width = 8)
gaza + geom_point(aes(x = long, y = lat, color = interactio, size = log(all_cas)), alpha = 0.5
                  , position = position_jitter(w = 0.01, h = 0.01)
                  , data = gaza_plotdata[actor1_side == 'ISR']) +
  scale_size_continuous('Log(event count)', range = c(1,10), guide = F) +
  scale_colour_discrete('ISR Action', labels = c('air strike', 'raid', 'shooting')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()

## WEST BANK EVENTS BY SIDE AND CASUALTY
dev.off()
pdf(file = 'westBankSideCas.pdf', height = 8, width = 8)
israel + geom_point(aes(x = long, y = lat, color = actor1_side, shape = actor1_side, size = log(all_cas)), alpha = 0.35
                  , position = position_jitter(w = 0.015, h = 0.015)
                  , data = westbank_plotdata) +
  scale_shape('Actor', labels = c('ISR', 'PAL')) +
  scale_size_continuous('Log(dead+wounded)', range = c(1,10), guide = F) +
  scale_colour_discrete('Actor', labels = c('ISR', 'PAL')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()

## WEST BANK EVENTS BY COUNT AND TYPE
dev.off()
pdf(file = 'westBankPalCountType.pdf', height = 8, width = 8)
israel + geom_point(aes(x = long, y = lat, color = interactio, size = log(event_count)), alpha = 0.5
                  , data = westbank_plotdata_points[actor1_side == 'PAL']) +
  scale_size_continuous('Log(event count)', range = c(1,10), guide = F) +
  scale_colour_discrete('PAL Action', labels = c('bombing', 'shelling', 'shooting')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()

pdf(file = 'westBankIsrCountType.pdf', height = 8, width = 8)
israel + geom_point(aes(x = long, y = lat, color = interactio, size = log(event_count)), alpha = 0.5
                  , data = westbank_plotdata_points[actor1_side == 'ISR']) +
  scale_size_continuous('Log(event count)', range = c(1,10), guide = F) +
  scale_colour_discrete('ISR Action', labels = c('air strike', 'raid', 'shooting')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()

## ISRAEL EVENTS BY SIDE AND CASUALTY
dev.off()
pdf(file = 'IsrSideCas.pdf', height = 8, width = 8)
israel + geom_point(aes(x = long, y = lat, color = actor1_side, shape = actor1_side, size = log(all_cas)), alpha = 0.5
                      , position = position_jitter(w = 0.015, h = 0.015)
                      , data = israel_plotdata) +
  scale_shape('Actor', labels = c('ISR', 'PAL')) +
  scale_size_continuous('Log(dead+wounded)', range = c(1,10), guide = F) +
  scale_colour_discrete('Actor', labels = c('ISR', 'PAL')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()

## ISRAEL EVENTS BY COUNT AND TYPE
dev.off()
pdf(file = 'IsrPalCountType.pdf', height = 8, width = 8)
israel + geom_point(aes(x = long, y = lat, color = interactio, size = log(event_count)), alpha = 0.5
                      , data = israel_plotdata_points[actor1_side == 'PAL']) +
  scale_size_continuous('Log(event count)', range = c(1,10), guide = F) +
  scale_colour_discrete('PAL Action', labels = c('bombing', 'shelling', 'shooting')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()

pdf(file = 'IsrIsrCountType.pdf', height = 8, width = 8)
israel + geom_point(aes(x = long, y = lat, color = interactio, size = log(event_count)), alpha = 0.5
                      , data = israel_plotdata_points[actor1_side == 'ISR']) +
  scale_size_continuous('Log(event count)', range = c(1,10), guide = F) +
  scale_colour_discrete('ISR Action', labels = c('air strike', 'raid', 'shooting')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()


### Collapse to time series format
old_data2 <- data
data <- data[!duplicated(data[, week]), ]

###### A couple more plots
Nsmooth <- 8
plot_data <- data[, list(date, isr_bigraid, pal_remote, pal_event, isr_event)]
plot_data[, isr_bigraid := SMA(xts(data[, isr_bigraid], order.by = data$date), n = Nsmooth)]
plot_data[, pal_remote := SMA(xts(data[, pal_remote], order.by = data$date), n = Nsmooth)]


## ISRAEL AND PALESTINE EVENT COUNTS
event_data <- melt(plot_data[, list(date, pal_event, isr_event)], id = 'date')
dev.off()
pdf(file = 'isrPalEvents.pdf', width = 10, height = 6)
ggplot() +
  geom_line(aes(x = date, y = value, color = variable), data = event_data) +
  xlab('Time (weeks)') +
  ylab('Violent events per week') +
  scale_colour_discrete('Initiator', labels = c('Palestinian', 'Israeli')) +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()

## ISRAEL RAIDS AND SHIFTS
date_range <- data.frame(from = as.Date(c('2002-03-24', '2003-06-29'))
                         , to = as.Date(c('2002-05-03', '2003-08-24')))
dev.off()
pdf(file = 'isrBigRaids.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = plot_data, aes(date, isr_bigraid)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.35, fill = c('red', 'blue')) +
  xlab('Time (weeks)') +
  ylab('ISR combined-arms raids (%)') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2002-01-01'), y = 0.08, label = 'Operation\n Defensive\n Shield') +
  annotate('text', x = as.Date('2003-04-01'), y = 0.08, label = 'Temporary\n Armistice')
dev.off()

## PALESTINE SHELLING AND SHIFTS
date_range <- data.frame(from = as.Date(c('2002-03-24', '2004-03-21'))
                         , to = as.Date(c('2002-05-03', '2004-03-28')))
pdf(file = 'palIndirect.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = plot_data, aes(date, pal_remote)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = c(0.35, 0.75), fill = c('red')) +
  xlab('Time (weeks)') +
  ylab('PAL indirect-fire attacks (%)') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2002-01-01'), y = 0.65, label = 'Operation\n Defensive\n Shield') +
  annotate('text', x = as.Date('2003-12-01'), y = 0.65, label = 'Death of\n Ahmed Yassin')
dev.off()




#################################################################
Nsmooth <- 10
data <- data[!duplicated(data$week)]

### Palestinian strategy
## Activity
# inputs0 <- SMA(xts(data[, list(pal_event)], order.by = data$date), n = Nsmooth)
inputs0 <- xts(data[, list(pal_event)], order.by = data$date)
# inputs0 <- diff(xts(data[, list(pal_event)], order.by = data$date))
pal_event <- inputs0[!is.na(inputs0)]

## Location
# inputs1 <- SMA(xts(data[, list(log(mean_palevent_pop+1))], order.by = data$date), n = Nsmooth)
# inputs1 <- xts(data[, list(log(mean_palevent_pop+1))], order.by = data$date)
# inputs1 <- SMA(xts(data[, list(mean_palevent_bdist)], order.by = data$date), n = Nsmooth)
inputs1 <- xts(data[, list(mean_palevent_bdist)], order.by = data$date)
# inputs1 <- diff(xts(data[, list(mean_palevent_bdist)], order.by = data$date))
mean_palevent_bdist <- inputs1[!is.na(inputs1)]

## Technology
# inputs2 <- SMA(xts(data[, list(pal_remote)], order.by = data$date), n = Nsmooth)
inputs2 <- xts(data[, list(pal_remote)], order.by = data$date)
# inputs2 <- diff(xts(data[, list(pal_remote)], order.by = data$date))
pal_indirect <- inputs2[!is.na(inputs2)]
# inputs2 <- SMA(xts(data[, list(pal_bombing)], order.by = data$date), n = Nsmooth)
inputs2 <- xts(data[, list(pal_bombing)], order.by = data$date)
# inputs2 <- diff(xts(data[, list(pal_bombing)], order.by = data$date))
pal_bombing <- inputs2[!is.na(inputs2)]
# inputs2 <- SMA(xts(data[, list(pal_suicidebombing)], order.by = data$date), n = Nsmooth)
inputs2 <- xts(data[, list(pal_suicidebombing)], order.by = data$date)
# inputs2 <- diff(xts(data[, list(pal_suicidebombing)], order.by = data$date))
pal_suicidebombing <- inputs2[!is.na(inputs2)]

## Target
# inputs3 <- SMA(xts(data[, list(pal_civtargeting)], order.by = data$date), n = Nsmooth)
inputs3 <- xts(data[, list(pal_civtargeting)], order.by = data$date)
# inputs3 <- diff(xts(data[, list(pal_civtargeting)], order.by = data$date))
pal_civtargeting <- inputs3[!is.na(inputs3)]
# inputs3 <- SMA(xts(data[, list(pal_miltargeting)], order.by = data$date), n = Nsmooth)
inputs3 <- xts(data[, list(pal_miltargeting)], order.by = data$date)
# inputs3 <- diff(xts(data[, list(pal_miltargeting)], order.by = data$date))
pal_miltargeting <- inputs3[!is.na(inputs3)]

## Location
inputs4 <- xts(data[, list(mean_palevent_area_ab)], order.by = data$date)
mean_palevent_area_ab <- inputs4[!is.na(inputs4)]

inputs4 <- xts(data[, list(mean_palevent_area_c)], order.by = data$date)
mean_palevent_area_c <- inputs4[!is.na(inputs4)]


plot(pal_event)
plot(mean_palevent_bdist)
plot(pal_indirect)
plot(pal_bombing)
plot(pal_suicidebombing)
plot(pal_civtargeting)
plot(pal_miltargeting)
plot(mean_palevent_area_ab)
plot(mean_palevent_area_c)

### Israeli strategy

## Activity
# inputs0 <- SMA(xts(data[, list(isr_event)], order.by = data$date), n = Nsmooth)
inputs0 <- xts(data[, list(isr_event)], order.by = data$date)
# inputs0 <- diff(xts(data[, list(isr_event)], order.by = data$date))
isr_event <- inputs0[!is.na(inputs0)]

## Location
# inputs1 <- EMA(xts(data[, list(log(mean_isrevent_pop+1))], order.by = data$date), n = Nsmooth)
# inputs1 <- xts(data[, list(log(mean_isrevent_pop+1))], order.by = data$date)
# inputs1 <- SMA(xts(data[, list(mean_isrevent_bdist)], order.by = data$date), n = Nsmooth)
inputs1 <- xts(data[, list(mean_isrevent_bdist)], order.by = data$date)
# inputs1 <- diff(xts(data[, list(mean_isrevent_bdist)], order.by = data$date))
mean_isrevent_bdist <- inputs1[!is.na(inputs1)]

## Technology
# inputs2 <- SMA(xts(data[, list(isr_bigtech)], order.by = data$date), n = Nsmooth)
inputs2 <- xts(data[, list(isr_bigtech)], order.by = data$date)
# inputs2 <- diff(xts(data[, list(isr_bigtech)], order.by = data$date))
isr_bigtech <- inputs2[!is.na(inputs2)]
# inputs2 <- SMA(xts(data[, list(isr_smalltech)], order.by = data$date), n = Nsmooth)
inputs2 <- xts(data[, list(isr_smalltech)], order.by = data$date)
# inputs2 <- diff(xts(data[, list(isr_smalltech)], order.by = data$date))
isr_smalltech <- inputs2[!is.na(inputs2)]
# inputs2 <- SMA(xts(data[, list(isr_nonviol)], order.by = data$date), n = Nsmooth)
inputs2 <- xts(data[, list(isr_nonviol)], order.by = data$date)
# inputs2 <- diff(xts(data[, list(isr_nonviol)], order.by = data$date))
isr_nonviol <- inputs2[!is.na(inputs2)]

## Target
# inputs3 <- SMA(xts(data[, list(isr_civtargeting)], order.by = data$date), n = Nsmooth)
inputs3 <- xts(data[, list(isr_civtargeting)], order.by = data$date)
# inputs3 <- diff(xts(data[, list(isr_civtargeting)], order.by = data$date))
isr_civtargeting <- inputs3[!is.na(inputs3)]
# inputs3 <- SMA(xts(data[, list(isr_miltargeting)], order.by = data$date), n = Nsmooth)
inputs3 <- xts(data[, list(isr_miltargeting)], order.by = data$date)
# inputs3 <- diff(xts(data[, list(isr_miltargeting)], order.by = data$date))
isr_miltargeting <- inputs3[!is.na(inputs3)]
# inputs3 <- SMA(xts(data[, list(isr_assassination)], order.by = data$date), n = Nsmooth)
inputs3 <- xts(data[, list(isr_assassination)], order.by = data$date)
# inputs3 <- diff(xts(data[, list(isr_assassination)], order.by = data$date))
isr_assassination <- inputs3[!is.na(inputs3)]

## Location
# City
inputs4 <- xts(data[, list(mean_isrevent_city)], order.by = data$date)
mean_isrevent_city <- inputs4[!is.na(inputs4)]

# Area A/B
inputs5 <- xts(data[, list(mean_isrevent_area_ab)], order.by = data$date)
mean_isrevent_area_ab <- inputs5[!is.na(inputs5)]

## Casualty Ratio
# inputs4 <- SMA(xts(data[, list(pal_casratio)], order.by = data$date), n = Nsmooth)
inputs6 <- xts(data[, list(pal_casratio)], order.by = data$date)
# inputs4 <- diff(xts(data[, list(pal_casratio)], order.by = data$date))
pal_casratio <- inputs6[!is.na(inputs6)]

plot(isr_event)
plot(mean_isrevent_bdist)
plot(isr_bigtech)
plot(isr_smalltech)
plot(isr_nonviol)
plot(isr_assassination)
plot(isr_civtargeting)
plot(isr_miltargeting)

#############################################################################
#############################################################################
input_data <- cbind(pal_event, mean_palevent_bdist
                    , pal_indirect, pal_bombing
                    , pal_suicidebombing
                    , pal_civtargeting, pal_miltargeting
                    , mean_palevent_area_ab, mean_palevent_area_c
                    , isr_event, mean_isrevent_bdist
                    , isr_bigtech, isr_smalltech
                    #, isr_nonviol
                    , isr_assassination
                    , isr_civtargeting, isr_miltargeting
                    , mean_isrevent_city, mean_isrevent_area_ab
                    , pal_casratio)

### Combined measurement: PCA of type/target/location, first component
input_data$pal_terrorism <- pal_indirect + pal_civtargeting + mean_palevent_area_c
input_data$isr_repression <- isr_bigtech + isr_civtargeting + mean_isrevent_city

input_data <- scale(input_data)
colnames <- c('pal_event', 'mean_palevent_bdist'
            , 'pal_indirect', 'pal_bombing'
            , 'pal_suicidebombing'
            , 'pal_civtargeting', 'pal_miltargeting'
            , 'mean_palevent_area_ab', 'mean_palevent_area_c'
            , 'isr_event', 'mean_isrevent_bdist'
            , 'isr_bigtech', 'isr_smalltech'
            #, 'isr_nonviol'
            , 'isr_assassination'
            , 'isr_civtargeting', 'isr_miltargeting'
            , 'mean_isrevent_city', 'mean_isrevent_area_ab'
            , 'pal_terrorism', 'isr_repression'
            , 'pal_casratio'
            , 'week')


for(i in 1:dim(input_data)[2]){
#   input_data[, i] <- diff(input_data[, i])
#   input_data[is.na(input_data)] <- 0
  print(colnames[i])
  print(Box.test(input_data[,i]))
}
week <- as.Date(index(input_data))
input_data <- data.table(as.matrix(input_data))
input_data[, week := week]
setnames(input_data, colnames)
input_data <- input_data[complete.cases(input_data), ]


# Plot number of events over time
dev.off()
pdf(file = 'IsrPalEvents.pdf', height = 5, width = 8)
plot.zoo(isr_event, ylim = c(0,100), col = 'blue', xlab = 'Time', ylab = 'Number of events per week (smoothed)')
par(new = T)
plot.zoo(pal_event, ylim = c(0,100), col = 'green', bty = 'n', xlab = '', ylab = '')
legend('topleft', legend = c('ISR', 'PAL'), fill = c('blue', 'green'))
dev.off()



#############################################################################
## PRELIMINARY STEP: ARIMAX models to assess exogenous relationships
## These are not the most 'theoretically appropriate' models, but they may serve
## to indicate promising avenues for analysis.

#### PAL ACTIONS
# Positive change in CIVILIAN TARGETING from CASUALTY RATIOS
exog = cbind(lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week)),
             lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 2),
             lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 3),
             lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 4))
exog[is.na(exog)] <- 0
armodel1 <- arima(input_data[, list(pal_civtargeting)], order = c(4,0,0), xreg = exog)
armodel1

# Change in AREA A/B targeting from CASUALTY RATIOS
armodel2 <- arima(input_data[, list(mean_palevent_area_ab)], order = c(4,0,0), xreg = exog)
armodel2

# Change on AREA C targeting from CASUALTY RATIOS
armodel3 <- arima(input_data[, list(mean_palevent_area_c)], order = c(4,0,0), xreg = exog)
armodel3

# Change in PAL TERRORISM TACTICS from CASUALTY RATIOS
armodel4 <- arima(input_data[, list(pal_terrorism)], order = c(4,0,0), xreg = exog)
armodel4


#### ISR ACTIONS
# Positive shock on HEAVY WEAPONS USE from CASUALTY RATIOS
armodel1 <- arima(input_data[, list(isr_bigtech)], order = c(4,0,0), xreg = exog)
armodel1

# Positive shock on CITY targeting from CASUALTY RATIOS
armodel2 <- arima(input_data[, list(mean_isrevent_city)], order = c(4,0,0), xreg = exog)
armodel2

# Positive shock on AREA A/B targeting from CASUALTY RATIOS
armodel3 <- arima(input_data[, list(mean_isrevent_area_ab)], order = c(4,0,0), xreg = exog)
armodel3

# Change in ISR INDISCRIMINATE TACTICS from CASUALTY RATIOS
armodel4 <- arima(input_data[, list(isr_repression)], order = c(4,0,0), xreg = exog)
armodel4

#####################################################################################
## MAIN ANALYSIS: These time series are likely highly endogenous, so ARIMAX is probably not the best approach.
## Using VAR models will allow me to assess the endogenized effects of a shock in one system on a shock in the other.
#####################################################################################
#### ENDOGENOUS SYSTEMS
exog = cbind(lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week)),
             lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 2)
             , lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 3),
             lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 4))
exog[is.na(exog)] <- 0

#################### 3-WAY INTERACTIONS
########## PAL TARG UP - ISR TECH UP - PAL TECH UP
inputs = xts(input_data[,list(pal_civtargeting, isr_bigtech, pal_indirect
)], order.by = input_data$week)
VARselect(inputs, lag.max = 8, type = 'both')$selection
model1 <- VAR(inputs, p = 4, type = 'both', exog = exog)
# Portmanteau test - passed!
serial.test(model1, type = 'PT.asymptotic')
# ARCH-LM test - passed!
arch.test(model1, lags.multi = 4)
summary(model1)
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, cumulative = F)

### Plotting IRF
dev.off()
pdf(file = 'ptargItech.pdf')
plot(irf_m1)
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()

######### FANCY PLOTS: ILLUSTRATION VIA GGPLOT
# PAL TARG UP - ISR TECH UP

irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_bigtech', response = 'pal_civtargeting')
irf_data1 <- data.frame(mean = irf_m1[[1]], lower = irf_m1[[2]], upper = irf_m1[[3]], period = c(1:6))
names(irf_data1) <- c('mean', 'lower', 'upper', 'week')

dev.off()
pdf(file = 'isrTechPalTarg.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data1) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Weeks after simulated shock to ISR heavy-arms use'
       , y = 'Change in PAL targeting of civilians') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()

# ISR TECH UP - PAL TECH UP
irf_m2 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_bigtech', response = 'pal_indirect')
irf_data2 <- data.frame(mean = irf_m2[[1]], lower = irf_m2[[2]], upper = irf_m2[[3]], period = c(1:6))
names(irf_data2) <- c('mean', 'lower', 'upper', 'week')

dev.off()
pdf(file = 'isrTechpalTech.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data2) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Weeks after simulated shock to ISR heavy-arms use'
       , y = 'Change in PAL indirect-fire weapons use') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()

######### FANCY PLOTS: ILLUSTRATION VIA GGPLOT
# ISR TECH UP - PAL TARG UP
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_bigtech', response = 'pal_civtargeting')
dev.off()
pdf(file = 'itechPtarg.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR heavy weapons use', ylab = 'PAL civilian targeting', xlab = 'foo')
dev.off()


########## ISR TARG DOWN - PAL ACT DOWN
inputs = xts(input_data[,list(isr_miltargeting, pal_event, isr_smalltech
)], order.by = input_data$week)
model1 <- VAR(inputs, p = 4, type = 'both', exog = exog)
# Consider using VECM model because ISR small tech is not stationary
# model1 <- VECM(inputs, lag = 4, include = 'both', estim = '2OLS', exogen = as.matrix(exog))
summary(model1)

irf_m1 <- irf(model1)
dev.off()
pdf(file = 'iact_itarg_pact_itech.pdf')
plot(irf_m1)
dev.off()

# ISR ACT UP - PAL ACT DOWN
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_miltargeting', response = 'pal_event')
dev.off()
pdf(file = 'iactPact.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR Miltary Targeting', ylab = 'PAL activity', xlab = 'foo')
dev.off()

### ONLY FOR VISUALIZATION - REVERSE SIGN ON PAL EVENTS TO SHOW NEGATIVE SHOCK
## ISR ACT UP - ISR TARG DOWN - PAL ACT DOWN - ISR TECH DOWN
input_data$pal_event2 <- -1 * input_data$pal_event
inputs = xts(input_data[,list(isr_miltargeting, pal_event2, isr_smalltech
)], order.by = input_data$week)
# Using VECM model because ISR small tech is not stationary
# model1 <- VECM(inputs, lag = 4, include = 'both', estim = '2OLS', exogen = as.matrix(exog))
model1 <- VAR(inputs, p = 4, type = 'both', exog = exog)

# PAL ACT DOWN - ISR TECH DOWN
irf_m3 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'pal_event2', response = 'isr_smalltech')
irf_data3 <- data.frame(mean = irf_m3[[1]], lower = irf_m3[[2]], upper = irf_m3[[3]], period = c(1:6))
names(irf_data3) <- c('mean', 'lower', 'upper', 'week')

dev.off()
pdf(file = 'pactItech.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data3) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data3) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Weeks after simulated shock to PAL weekly activity'
       , y = 'Change in ISR reliance on light arms') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()

# pdf(file = 'pactItech.pdf', height = 5, width = 8)
# plot(irf_m1, main = 'PAL activity (negative shock)', ylab = 'ISR light weapons use ', xlab = 'foo')
# dev.off()



########## PAL LOC UP - ISR LOC UP
inputs = xts(input_data[,list(mean_isrevent_area_ab, mean_palevent_area_c
)], order.by = input_data$week)
VARselect(inputs, lag.max = 8, type = 'both')$selection
model1 <- VAR(inputs, p = 4, type = 'both', exog = exog)
# Portmanteau test - passed!
serial.test(model1, type = 'PT.asymptotic')
# ARCH-LM test - passed!
arch.test(model1, lags.multi = 4)
summary(model1)
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, cumulative = F)

#### FANCY PLOTS: ILLUSTRATION VIA GGPLOT
irf_m2 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'mean_palevent_area_c'
              , response = 'mean_isrevent_area_ab')
irf_data1 <- data.frame(mean = irf_m2[[1]], lower = irf_m2[[2]], upper = irf_m2[[3]], period = c(1:6))
names(irf_data1) <- c('mean', 'lower', 'upper', 'week')

dev.off()
pdf(file = 'isrLocPalLoc.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data1) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Weeks after simulated shock to PAL operations in Israel and Area C'
       , y = 'Change in ISR operations in Areas A/B') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()


########## PAL TERROR UP - ISR REPRESSION UP
inputs = xts(input_data[,list(pal_terrorism, isr_repression
)], order.by = input_data$week)
VARselect(inputs, lag.max = 8, type = 'both')$selection
model1 <- VAR(inputs, p = 4, type = 'both', exog = exog)
# Portmanteau test - passed!
serial.test(model1, type = 'PT.asymptotic')
# ARCH-LM test - passed!
arch.test(model1, lags.multi = 4)
summary(model1)
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, cumulative = F)

#### FANCY PLOTS: ILLUSTRATION VIA GGPLOT
irf_m2 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'pal_terrorism'
              , response = 'isr_repression')
irf_data1 <- data.frame(mean = irf_m2[[1]], lower = irf_m2[[2]], upper = irf_m2[[3]], period = c(1:6))
names(irf_data1) <- c('mean', 'lower', 'upper', 'week')

dev.off()
pdf(file = 'palTerIsrRep.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data1) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Weeks after simulated shock to PAL terrorism strategy'
       , y = 'Change in ISR large-scale repression strategy') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()


#####################################################################################
## SUPPLEMENTARY ANALYSIS: Is there evidence that PAL/ISR behavior changed after Defensive Shield?
#####################################################################################
library(changepoint)
library(bcp)
library(ecp)
## Create non-standardized data frame
input_data <- cbind(pal_event, mean_palevent_bdist
                    , pal_indirect, pal_bombing
                    , pal_suicidebombing
                    , pal_civtargeting, pal_civtargeting
                    , mean_palevent_area_ab, mean_palevent_area_c
                    , isr_event, mean_isrevent_bdist
                    , isr_bigtech, isr_smalltech
                    #, isr_nonviol
                    , isr_assassination
                    , isr_civtargeting, isr_miltargeting
                    , mean_isrevent_city, mean_isrevent_area_ab
                    , pal_casratio)

colnames <- c('pal_event', 'mean_palevent_bdist'
              , 'pal_indirect', 'pal_bombing'
              , 'pal_suicidebombing'
              , 'pal_civtargeting', 'pal_miltargeting'
              , 'mean_palevent_area_ab', 'mean_palevent_area_c'
              , 'isr_event', 'mean_isrevent_bdist'
              , 'isr_bigtech', 'isr_smalltech'
              #, 'isr_nonviol'
              , 'isr_assassination'
              , 'isr_civtargeting', 'isr_miltargeting'
              , 'mean_isrevent_city', 'mean_isrevent_area_ab'
              , 'pal_casratio'
              , 'week')
week <- as.Date(index(input_data))
input_data <- data.table(as.matrix(input_data))
input_data[, week := week]
setnames(input_data, colnames)
input_data <- input_data[complete.cases(input_data), ]

inputs <- input_data[,list(week, isr_bigtech, pal_indirect, pal_event
                           , mean_isrevent_area_ab, mean_palevent_area_c)]

##### CP detection
# PAL indirect
pal_cp_1 <- e.divisive(as.matrix(inputs$pal_indirect), R = 999)
pal_change_idx_1 <- pal_cp_1$estimates[c(2,3)]
pal_means_1 <- data.frame('mean' = c(mean(inputs[1:pal_change_idx_1[1], pal_indirect])
                                   , mean(inputs[pal_change_idx_1[1]:pal_change_idx_1[2], pal_indirect])
                                   , mean(inputs[pal_change_idx_1[2]:nrow(inputs), pal_indirect])))

# PAL area C
pal_cp_2 <- e.divisive(as.matrix(inputs$mean_palevent_area_c), R = 999)
pal_change_idx_2 <- pal_cp_2$estimates[c(2,3)]
pal_means_2 <- data.frame('mean' = c(mean(inputs[1:pal_change_idx_2[1], mean_palevent_area_c])
                                   , mean(inputs[pal_change_idx_2[1]:pal_change_idx_2[2], mean_palevent_area_c])
                                   , mean(inputs[pal_change_idx_2[2]:nrow(inputs), mean_palevent_area_c])))

# ISR heavy arms
isr_cp_1 <- e.divisive(as.matrix(inputs$isr_bigtech), R = 999)
isr_change_idx_1 <- isr_cp_1$estimates[c(2,3)]
isr_means_1 <- data.frame('mean' = c(mean(inputs[1:isr_change_idx_1[1], isr_bigtech])
                                   , mean(inputs[isr_change_idx_1[1]:nrow(inputs), isr_bigtech])))

# ISR area A/B
isr_cp_2 <- e.divisive(as.matrix(inputs$mean_isrevent_area_ab), R = 999)
isr_change_idx_2 <- isr_cp_2$estimates[2]
isr_means_2 <- data.frame('mean' = c(mean(inputs[1:isr_change_idx_2[1], mean_isrevent_area_ab])
                                   , mean(inputs[isr_change_idx_2[1]:nrow(inputs), mean_isrevent_area_ab])))

##### Plotting CP
Nsmooth <- 8
inputs[, pal_indirect := SMA(xts(data[, pal_indirect], order.by = data$date), n = Nsmooth)]
inputs[, mean_palevent_area_c := SMA(xts(data[, mean_palevent_area_c], order.by = data$date), n = Nsmooth)]
inputs[, isr_bigtech := SMA(xts(data[, isr_bigtech], order.by = data$date), n = Nsmooth)]
inputs[, mean_isrevent_area_ab := SMA(xts(data[, mean_isrevent_area_ab], order.by = data$date), n = Nsmooth)]

## PAL CP 1
date_range <- data.frame('from' = inputs$week[pal_change_idx_1]
                         , 'to' = inputs$week[pal_change_idx_1 + 2])

dev.off()
pdf(file = 'palCP1.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = inputs, aes(week, pal_indirect)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.5, fill = c('blue')) +
  geom_segment(aes(x=inputs$week[1], xend=inputs$week[pal_change_idx_1[1]]
                   , y=0.286, yend=0.286), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_1[1]], xend=inputs$week[pal_change_idx_1[2]]
                   , y=0.398, yend=0.398), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_1[2]], xend=inputs$week[nrow(inputs)]
                   , y=0.625, yend=0.625), color = 'red') +
  xlab('Time') +
  ylab('PAL indirect-fire attacks (%)') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2001-09-01'), y = 0.6, label = 'First changepoint\n (Defensive Shield)') +
  annotate('text', x = as.Date('2004-02-01'), y = 0.6, label = 'Second changepoint\n (?)')
dev.off()



## PAL CP 2
date_range <- data.frame('from' = inputs$week[pal_change_idx_2]
                         , 'to' = inputs$week[pal_change_idx_2 + 2])

dev.off()
pdf(file = 'palCP2.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = inputs, aes(week, mean_palevent_area_c)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.5, fill = c('blue')) +
  geom_segment(aes(x=inputs$week[1], xend=inputs$week[pal_change_idx_2[1]]
                   , y=0.241, yend=0.241), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_2[1]], xend=inputs$week[pal_change_idx_2[2]]
                   , y=0.118, yend=0.118), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_2[2]], xend=inputs$week[nrow(inputs)]
                   , y=0.068, yend=0.068), color = 'red') +
  xlab('Time') +
  ylab('PAL attacks in pre-1967 borders & area C (%)') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2001-09-01'), y = 0.6, label = 'First changepoint\n (?)') +
  annotate('text', x = as.Date('2004-02-01'), y = 0.6, label = 'Second changepoint\n (?)')
dev.off()


## ISR CP 1
date_range <- data.frame('from' = inputs$week[isr_change_idx]
                         , 'to' = inputs$week[isr_change_idx+2])

dev.off()
pdf(file = 'isrCP.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = inputs, aes(week, isr_bigtech)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.5, fill = c('blue')) +
  geom_segment(aes(x=inputs$week[1], xend=inputs$week[isr_change_idx[1]]
                   , y=0.341, yend=0.341), color = 'red') +
  geom_segment(aes(x=inputs$week[isr_change_idx[1]], xend=inputs$week[nrow(inputs)]
                   , y=0.216, yend=0.216), color = 'red') +
  xlab('Time') +
  ylab('ISR heavy weapons use (%)') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2003-10-01'), y = 0.4, label = 'First changepoint\n (Abbas as PM)')
dev.off()


## ISR CP 2
date_range <- data.frame('from' = inputs$week[isr_change_idx]
                         , 'to' = inputs$week[isr_change_idx+2])

dev.off()
pdf(file = 'isrCP.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = inputs, aes(week, isr_bigtech)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.5, fill = c('blue')) +
  geom_segment(aes(x=inputs$week[1], xend=inputs$week[isr_change_idx[1]]
                   , y=0.341, yend=0.341), color = 'red') +
  geom_segment(aes(x=inputs$week[isr_change_idx[1]], xend=inputs$week[nrow(inputs)]
                   , y=0.216, yend=0.216), color = 'red') +
  xlab('Time') +
  ylab('ISR heavy weapons use (%)') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2003-10-01'), y = 0.4, label = 'First changepoint\n (Abbas as PM)')
dev.off()

#################################################
## SUPPLEMENTARY VAR MODELS
input_data <- cbind(pal_event, mean_palevent_bdist
                    , pal_indirect, pal_bombing
                    , pal_suicidebombing
                    , pal_civtargeting, pal_civtargeting
                    , isr_event, mean_isrevent_bdist
                    , isr_bigtech, isr_smalltech
                    #, isr_nonviol
                    , isr_assassination
                    , isr_civtargeting, isr_miltargeting
                    , pal_casratio)
input_data <- scale(input_data)
colnames <- c('pal_event', 'mean_palevent_bdist'
              , 'pal_indirect', 'pal_bombing'
              , 'pal_suicidebombing'
              , 'pal_civtargeting', 'pal_miltargeting'
              , 'isr_event', 'mean_isrevent_bdist'
              , 'isr_bigtech', 'isr_smalltech'
              #, 'isr_nonviol'
              , 'isr_assassination'
              , 'isr_civtargeting', 'isr_miltargeting'
              , 'pal_casratio'
              , 'week')
for(i in 1:dim(input_data)[2]){
  #   input_data[, i] <- diff(input_data[, i])
  #   input_data[is.na(input_data)] <- 0
  print(colnames[i])
  print(Box.test(input_data[,i]))
}
week <- as.Date(index(input_data))
input_data <- data.table(as.matrix(input_data))
input_data[, week := week]
setnames(input_data, colnames)
input_data <- input_data[complete.cases(input_data), ]

#### ENDOGENOUS SYSTEMS
#################### 3-WAY INTERACTIONS
########## PAL TARG UP - ISR TECH UP
#### PAL REGIME 1
exog = cbind(lag(xts(input_data[1:pal_change_idx[1],list(pal_casratio)], order.by = input_data$week[1:pal_change_idx[1]])),
             lag(xts(input_data[1:pal_change_idx[1],list(pal_casratio)], order.by = input_data$week[1:pal_change_idx[1]]), 2),
             lag(xts(input_data[1:pal_change_idx[1],list(pal_casratio)], order.by = input_data$week[1:pal_change_idx[1]]), 3),
             lag(xts(input_data[1:pal_change_idx[1],list(pal_casratio)], order.by = input_data$week[1:pal_change_idx[1]]), 4))
exog[is.na(exog)] <- 0

inputs = xts(input_data[1:pal_change_idx[1],list(pal_civtargeting, isr_bigtech, pal_indirect
)], order.by = input_data$week[1:pal_change_idx[1]])
VARselect(inputs, lag.max = 8, type = 'both')$selection
model1 <- VAR(inputs, p = 4, type = 'both', exog = exog)
# Portmanteau test - passed!
serial.test(model1, type = 'PT.asymptotic')
# ARCH-LM test - passed!
arch.test(model1, lags.multi = 4)
summary(model1)

#### PAL REGIME 2
exog = cbind(lag(xts(input_data[pal_change_idx[1]:pal_change_idx[2],list(pal_casratio)], order.by = input_data$week[pal_change_idx[1]:pal_change_idx[2]])),
             lag(xts(input_data[pal_change_idx[1]:pal_change_idx[2],list(pal_casratio)], order.by = input_data$week[pal_change_idx[1]:pal_change_idx[2]]), 2),
             lag(xts(input_data[pal_change_idx[1]:pal_change_idx[2],list(pal_casratio)], order.by = input_data$week[pal_change_idx[1]:pal_change_idx[2]]), 3),
             lag(xts(input_data[pal_change_idx[1]:pal_change_idx[2],list(pal_casratio)], order.by = input_data$week[pal_change_idx[1]:pal_change_idx[2]]), 4))
exog[is.na(exog)] <- 0

inputs = xts(input_data[pal_change_idx[1]:pal_change_idx[2],list(pal_civtargeting, isr_bigtech, pal_indirect
)], order.by = input_data$week[pal_change_idx[1]:pal_change_idx[2]])
VARselect(inputs, lag.max = 8, type = 'both')$selection
model1 <- VAR(inputs, p = 1, type = 'both', exog = exog)
# Portmanteau test - passed!
serial.test(model1, type = 'PT.asymptotic')
# ARCH-LM test - passed!
arch.test(model1, lags.multi = 1)
summary(model1)





irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, cumulative = F)

### Plotting IRF
dev.off()
pdf(file = 'ptargItech.pdf')
plot(irf_m1)
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()

# PAL TARG UP - ISR TECH UP
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'pal_civtargeting', response = 'isr_bigtech')
dev.off()
pdf(file = 'itechPtarg.pdf', height = 5, width = 8)
plot(irf_m1, main = 'PAL civilian targeting', ylab = 'ISR heavy weapons use', xlab = 'foo')
dev.off()

# ISR TECH UP - PAL TECH UP
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_bigtech', response = 'pal_indirect')
dev.off()
pdf(file = 'itechPtarg.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR heavy weapons use', ylab = 'PAL indirect-fire weapons use', xlab = 'foo')
dev.off()

# ISR TECH UP - PAL TARG UP
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_bigtech', response = 'pal_civtargeting')
dev.off()
pdf(file = 'itechPtarg.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR heavy weapons use', ylab = 'PAL civilian targeting', xlab = 'foo')
dev.off()


########## ISR TARG DOWN - PAL ACT DOWN
inputs = xts(input_data[,list(isr_miltargeting, pal_event, isr_smalltech
)], order.by = input_data$week)
model1 <- VAR(inputs, p = 4, type = 'both')
# Consider using VECM model because ISR small tech is not stationary
# model1 <- VECM(inputs, lag = 4, include = 'both', estim = '2OLS', exogen = as.matrix(exog))
summary(model1)

irf_m1 <- irf(model1)
dev.off()
pdf(file = 'iact_itarg_pact_itech.pdf')
plot(irf_m1)
dev.off()

# ISR ACT UP - PAL ACT DOWN
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_miltargeting', response = 'pal_event')
dev.off()
pdf(file = 'iactPact.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR Miltary Targeting', ylab = 'PAL activity', xlab = 'foo')
dev.off()

### ONLY FOR VISUALIZATION - REVERSE SIGN ON PAL EVENTS TO SHOW NEGATIVE SHOCK
## ISR ACT UP - ISR TARG DOWN - PAL ACT DOWN - ISR TECH DOWN
input_data$pal_event2 <- -1 * input_data$pal_event
inputs = xts(input_data[,list(isr_miltargeting, pal_event2, isr_smalltech
)], order.by = input_data$week)
# Using VECM model because ISR small tech is not stationary
model1 <- VECM(inputs, lag = 4, include = 'both', estim = '2OLS', exogen = as.matrix(exog))

# PAL ACT DOWN - ISR TECH DOWN
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'pal_event2', response = 'isr_smalltech')
dev.off()
pdf(file = 'pactItech.pdf', height = 5, width = 8)
plot(irf_m1, main = 'PAL activity (negative shock)', ylab = 'ISR light weapons use ', xlab = 'foo')
dev.off()




