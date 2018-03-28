rm(list = ls())
setwd('/Users/macbook/Dropbox/Dissertation/Data/Israel')
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
# Read in data
data <- fread('isr_eventdata.csv')
# Data setup
old_data <- data

data <- old_data
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
violent_events = c('shelling', 'shooting', 'beating', 'bombing', 'firefight', 'raid', 'air strike', 'shelrock')
nonviol_events = c('bulldozing', 'crowd control', 'detainment', 'fortification', 'movement restriction', 'vandalism'
                   , 'border closure', 'clash')

# ################### ################### ################### ##################
# REMOVING ALL NON-VIOLENT EVENTS: ONLY KEEPING VIOLENCE
data <- data[(interactio %in% violent_events) & ((actor1 %in% pal_milactors) | (actor1 %in% isr_milactors))]

# Flagging arrest-raids as non-violent actions
data <- data[!(interactio == 'raid' & actor1 %in% isr_milactors & actor2 == 'palres' & detainment > 0
               & technology == 'medium arms'
              & propertyda == 0 & is.na(technolo_1)
              & pal_fatali == 0 & isr_fatali == 0 & pal_combin == 0), ]

### Types of location
# Population
data[, mean_palevent_pop := mean(as.numeric(Population[actor1 %in% pal_milactors]), na.rm = T), by = list(year, month)]
data[, sd_palevent_pop := sd(as.numeric(Population[actor1 %in% pal_milactors]), na.rm = T), by = list(year, month)]
data[is.na(mean_palevent_pop), mean_palevent_pop := 0]
data[is.na(sd_palevent_pop), sd_palevent_pop := 0]
data[, mean_isrevent_pop := mean(as.numeric(Population[actor1 %in% isr_milactors]), na.rm = T), by = list(year, month)]
data[, sd_isrevent_pop := sd(as.numeric(Population[actor1 %in% isr_milactors]), na.rm = T), by = list(year, month)]
data[is.na(mean_isrevent_pop), mean_isrevent_pop := 0]
data[is.na(sd_isrevent_pop), sd_isrevent_pop := 0]

# Israel vs palestine location
data[, mean_palevent_pal := mean(palestine[actor1 %in% pal_milactors], na.rm = T), by = list(year, month)]
data[is.na(mean_palevent_pal), mean_palevent_pal := mean(data$mean_palevent_pal, na.rm = T)]
data[, mean_isrevent_pal := mean(palestine[actor1 %in% isr_milactors], na.rm = T), by = list(year, month)]
data[is.na(mean_isrevent_pal), mean_isrevent_pal := mean(data$mean_isrevent_pal, na.rm = T)]

# Distance from border
data[, mean_palevent_bdist := mean(mindist_border[actor1 %in% pal_milactors], na.rm = T), by = list(year, month)]
data[, sd_palevent_bdist := sd(mindist_border[actor1 %in% pal_milactors], na.rm = T), by = list(year, month)]
data[is.na(mean_palevent_bdist), mean_palevent_bdist := mean(data$mean_palevent_bdist, na.rm = T)]
data[is.na(sd_palevent_bdist), sd_palevent_bdist := max(data$sd_palevent_bdist, na.rm = T)]
data[, mean_isrevent_bdist := mean(mindist_border[actor1 %in% isr_milactors], na.rm = T), by = list(year, month)]
data[, sd_isrevent_bdist := sd(mindist_border[actor1 %in% isr_milactors], na.rm = T), by = list(year, month)]
data[is.na(mean_isrevent_bdist), mean_isrevent_bdist := mean(data$mean_isrevent_bdist, na.rm = T)]
data[is.na(sd_isrevent_bdist), sd_isrevent_bdist := mean(data$sd_isrevent_bdist, na.rm = T)]

### Casualties
data[, paldead_mo := sum(pal_fatali), by = list(year, month)]
data[, paldead_civ_mo := sum(pal_nonc_1), by = list(year, month)]
data[, paldead_milt_mo := sum(pal_mili_1), by = list(year, month)]
data[, paldead_mili_mo := sum(pal_mili_2), by = list(year, month)]
data[is.na(paldead_mo), paldead_mo := 0]
data[is.na(paldead_civ_mo), paldead_civ_mo := 0]
data[is.na(paldead_mili_mo), paldead_mili_mo := 0]
data[is.na(paldead_milt_mo), paldead_milt_mo := 0]
data[, paldead_mil_mo := paldead_milt_mo + paldead_mili_mo, by = list(year, month)]
data[, paldead_mili_mo := NULL]
data[, paldead_milt_mo := NULL]

data[, isrdead_mo := sum(isr_fatali), by = list(year, month)]
data[, isrdead_civ_mo := sum(isr_nonc_1), by = list(year, month)]
data[, isrdead_mil_mo := sum(isr_combat), by = list(year, month)]
data[is.na(isrdead_mo), isrdead_mo := 0]
data[is.na(isrdead_civ_mo), isrdead_civ_mo := 0]
data[is.na(isrdead_mil_mo), isrdead_mil_mo := 0]

data[, palwound_mo := sum(pal_combin), by = list(year, month)]
data[, palwound_civ_mo := sum(pal_noncom), by = list(year, month)]
data[, palwound_mil_mo := sum(pal_milita), by = list(year, month)]
data[is.na(palwound_mo), palwound_mo := 0]
data[is.na(palwound_civ_mo), palwound_civ_mo := 0]
data[is.na(palwound_mil_mo), palwound_mil_mo := 0]

data[, isrwound_civ_mo := sum(isr_noncom), by = list(year, month)]
data[, isrwound_mil_mo := sum(isr_milita), by = list(year, month)]
data[, isrwound_pol_mo := sum(isr_police), by = list(year, month)]
data[, isrwound_mil_mo := isrwound_mil_mo + isrwound_pol_mo, by = list(year, month)]
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
data[, pal_event := sum(event == 1 & (actor1 %in% pal_milactors & interactio %in% violent_events) | (interactio == 'firefight')), by = list(year, month)]
data[, isr_event := sum(event == 1 & (actor1 %in% isr_milactors & interactio %in% violent_events) | (interactio == 'firefight')), by = list(year, month)]
data[, mut_event := sum(event == 1 & interactio %in% 'firefight'), by = list(year, month)]

# Shootings
data[, pal_shooting := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shooting')), by = list(year, month)]
data[pal_event > 0, pal_shooting := pal_shooting / pal_event, by = list(year, month)]
data[, isr_shooting := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shooting')), by = list(year, month)]
data[isr_event > 0, isr_shooting := isr_shooting / isr_event, by = list(year, month)]

# Firefights
data[, pal_firefight := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'firefight')), by = list(year, month)]
data[pal_event > 0, pal_firefight := pal_firefight / pal_event, by = list(year, month)]
data[, isr_firefight := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'firefight')), by = list(year, month)]
data[isr_event > 0, isr_firefight := isr_firefight / isr_event, by = list(year, month)]

# Shellings
data[, pal_shelling := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shelling')), by = list(year, month)]
data[pal_event > 0, pal_shelling := pal_shelling / pal_event, by = list(year, month)]
data[, pal_smallshelling := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shelling' & technolo_1 %in% small_arms)), by = list(year, month)]
data[pal_event > 0, pal_smallshelling := pal_smallshelling / pal_event, by = list(year, month)]
data[, pal_bigshelling := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'shelling' & technolo_1 %in% big_arms)), by = list(year, month)]
data[pal_event > 0, pal_bigshelling := pal_bigshelling / pal_event, by = list(year, month)]

data[, isr_shelling := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shelling')), by = list(year, month)]
data[isr_event > 0, isr_shelling := isr_shelling / isr_event, by = list(year, month)]
data[, isr_smallshelling := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shelling' & technolo_1 %in% small_arms)), by = list(year, month)]
data[isr_event > 0, isr_smallshelling := isr_smallshelling / isr_event, by = list(year, month)]
data[, isr_bigshelling := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'shelling' & technolo_1 %in% big_arms)), by = list(year, month)]
data[isr_event > 0, isr_bigshelling := isr_bigshelling / isr_event, by = list(year, month)]

# Bombings
data[, pal_bombing := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'bombing')), by = list(year, month)]
data[pal_event > 0, pal_bombing := pal_bombing / pal_event, by = list(year, month)]
data[, pal_suicidebombing := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'bombing' & (context %in% 'suicide' | technolo_1 %in% 'belt'))), by = list(year, month)]
data[pal_event > 0, pal_suicidebombing := pal_suicidebombing / pal_event, by = list(year, month)]
data[, isr_bombing := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'bombing')), by = list(year, month)]
data[isr_event > 0, isr_bombing := isr_bombing / isr_event, by = list(year, month)]

# Raids
data[, pal_raid := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'raid')), by = list(year, month)]
data[pal_event > 0, pal_raid := pal_raid / pal_event, by = list(year, month)]
data[, isr_raid := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'raid')), by = list(year, month)]
data[isr_event > 0, isr_raid := isr_raid / isr_event, by = list(year, month)]
data[, isr_smallraid := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'raid' & technolo_1 %in% small_arms)), by = list(year, month)]
data[isr_event > 0, isr_smallraid := isr_smallraid / isr_event, by = list(year, month)]
data[, isr_bigraid := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'raid' & technolo_1 %in% big_arms)), by = list(year, month)]
data[isr_event > 0, isr_bigraid := isr_bigraid / isr_event, by = list(year, month)]

# Beatings 
data[, pal_beating := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'beating')), by = list(year, month)]
data[pal_event > 0, pal_beating := pal_beating / pal_event, by = list(year, month)]

# Clashes
data[, pal_clash := as.numeric(sum(actor1 %in% pal_milactors & interactio == 'crowd control')), by = list(year, month)]
data[pal_event > 0, pal_clash := pal_clash / pal_event, by = list(year, month)]

# Air strikes
data[, isr_airstrike := as.numeric(sum(actor1 %in% isr_milactors & interactio == 'air strike')), by = list(year, month)]
data[isr_event > 0, isr_airstrike := isr_airstrike / isr_event, by = list(year, month)]

# Nonviolent/defensive
data[, isr_nonviol := as.numeric(sum(actor1 %in% isr_milactors & interactio %in% c('bulldozing', 'detainment', 'movement restriction'))), by = list(year, month)]
data[isr_event > 0, isr_nonviol := isr_nonviol / isr_event, by = list(year, month)]

### Type of technology
# Heavy vs light arms
data[, pal_bigtech := as.numeric(sum(actor1 %in% pal_milactors & technology %in% big_arms)), by = list(year, month)]
data[, pal_smalltech := as.numeric(sum(actor1 %in% pal_milactors & technology %in% small_arms)), by = list(year, month)]
data[pal_event > 0, pal_bigtech := pal_bigtech / pal_event, by = list(year, month)]
data[, isr_bigtech := as.numeric(sum(actor1 %in% isr_milactors & technology %in% big_arms)), by = list(year, month)]
data[, isr_smalltech := as.numeric(sum(actor1 %in% isr_milactors & technology %in% small_arms)), by = list(year, month)]
data[isr_event > 0, isr_bigtech := isr_bigtech / isr_event, by = list(year, month)]

# Direct vs indirect fire
data[, pal_direct := as.numeric(sum((actor1 %in% pal_milactors & technology %in% direct_fire) | (interactio %in% 'firefight'))), by = list(year, month)]
data[pal_event > 0, pal_direct := pal_direct / pal_event, by = list(year, month)]
data[, pal_indirect := as.numeric(sum(actor1 %in% pal_milactors & technology %in% indirect_fire)), by = list(year, month)]
data[pal_event > 0, pal_indirect := pal_indirect / pal_event, by = list(year, month)]
data[, isr_direct := as.numeric(sum((actor1 %in% isr_milactors & technology %in% direct_fire) | (interactio %in% 'firefight'))), by = list(year, month)]
data[isr_event > 0, isr_direct := isr_direct / isr_event, by = list(year, month)]
data[, isr_indirect := as.numeric(sum(actor1 %in% isr_milactors & technology %in% indirect_fire)), by = list(year, month)]
data[isr_event > 0, isr_indirect := isr_indirect / isr_event, by = list(year, month)]


# On-site vs remote attacks
data[, pal_remote := as.numeric(sum(actor1 %in% pal_milactors 
                                    & (technology %in% indirect_fire | interactio %in% 'shelling')
                                    & israel == 1)), by = list(year, month)]
data[pal_event > 0, pal_remote := pal_remote / pal_event, by = list(year, month)]
plot(data$pal_remote)


### Targets
data[, pal_civtargeting := as.numeric(sum(actor1 %in% pal_milactors & actor2 %in% isr_civactors & interactio %in% violent_events)), by = list(year, month)]
data[pal_event > 0, pal_civtargeting := pal_civtargeting / pal_event, by = list(year, month)]
data[, pal_miltargeting := as.numeric(sum(actor1 %in% pal_milactors & actor2 %in% isr_milactors & interactio %in% violent_events)), by = list(year, month)]
data[pal_event > 0, pal_miltargeting := pal_miltargeting / pal_event, by = list(year, month)]

data[, isr_civtargeting := as.numeric(sum(actor1 %in% isr_milactors & actor2 %in% pal_civactors 
                                          & interactio %in% violent_events & interactio != 'raid')), by = list(year, month)]
data[isr_event > 0, isr_civtargeting := isr_civtargeting / isr_event, by = list(year, month)]
data[, isr_miltargeting := as.numeric(sum(actor1 %in% isr_milactors & actor2 %in% pal_milactors & interactio %in% violent_events)), by = list(year, month)]
data[isr_event > 0, isr_miltargeting := isr_miltargeting / isr_event, by = list(year, month)]


## Test
# data[, isr_bigtech := isr_bigtech * isr_event]
# data[, pal_indirect := pal_indirect * pal_event]
# data[, isr_civtargeting := isr_civtargeting * isr_event]
# data[, pal_civtargeting := pal_civtargeting * isr_event]

### Collapse to year-month level
old_data2 <- data
#data <- old_data2
# data <- data[!duplicated(data[, week]), ]
data <- data[!duplicated(data[, list(year, month)]), ]
# data <- data[!duplicated(data[, list(year,month)]), ]


#################################################################
######## Preliminary models
######## Changepoint detection (CPT)
library(bcp)
library(forecast)
library(TTR)
bcp_function <- function(inputs){
  # inputs <- scale(inputs)
  bcp_est <- bcp(inputs, w0 = 0.3, p0 = 0.1, burnin = 1000, mcmc = 10000)
  plot(bcp_est, separated = F)
  return(bcp_est)
}
### Palestinian strategy
# Location
Nsmooth <- 3
spar <- 0.2

y <- c(data[, list(log(mean_palevent_pop+1))])$V1
x <- c(1:nrow(data))

# Location
# inputs1 <- EMA(xts(data[, list(log(mean_palevent_pop+1))], order.by = data$date), n = Nsmooth)
# inputs1 <- xts(data[, list(log(mean_palevent_pop+1))], order.by = data$date)
inputs1 <- EMA(xts(data[, list(mean_palevent_bdist)], order.by = data$date), n = Nsmooth)
# inputs1 <- xts(data[, list(mean_palevent_bdist)], order.by = data$date)
inputs1 <- inputs1[!is.na(inputs1)]
pal_loc <- bcp_function(inputs1)$posterior.prob
pal_loc_dt <- data.table(matrix(pal_loc))
setnames(pal_loc_dt, 'mean_pal_pop')

# Technology
inputs2 <- EMA(xts(data[, list(pal_indirect)], order.by = data$date), n = Nsmooth)
# inputs2 <- xts(data[, list(pal_indirect)], order.by = data$date)
inputs2 <- inputs2[!is.na(inputs2)]
pal_tech <- bcp_function(inputs2)$posterior.prob
pal_tech_dt <- data.table(matrix(pal_tech))
setnames(pal_tech_dt, 'mean_pal_tech')

# Target
inputs3 <- EMA(xts(data[, list(pal_civtargeting)], order.by = data$date), n = Nsmooth)
# inputs3 <- xts(data[, list(pal_civtargeting)], order.by = data$date)
inputs3 <- inputs3[!is.na(inputs3)]
pal_targ <- bcp_function(inputs3)$posterior.prob
pal_targ_dt <- data.table(matrix(pal_targ))
setnames(pal_targ_dt, 'pal_miltargeting')

### Israeli strategy
# Location
# inputs1 <- EMA(xts(data[, list(log(mean_isrevent_pop+1))], order.by = data$date), n = Nsmooth)
# inputs1 <- xts(data[, list(log(mean_isrevent_pop+1))], order.by = data$date)
inputs1 <- EMA(xts(data[, list(mean_isrevent_bdist)], order.by = data$date), n = Nsmooth)
# inputs1 <- xts(data[, list(mean_isrevent_bdist)], order.by = data$date)
inputs1 <- inputs1[!is.na(inputs1)]
isr_loc <- bcp_function(inputs1)$posterior.prob
isr_loc_dt <- data.table(matrix(isr_loc))
setnames(isr_loc_dt, 'mean_isr_pop')

# Technology
inputs2 <- EMA(xts(data[, list(isr_bigtech)], order.by = data$date), n = Nsmooth)
# inputs2 <- xts(data[, list(isr_bigtech)], order.by = data$date)
inputs2 <- inputs2[!is.na(inputs2)]
isr_tech <- bcp_function(inputs2)$posterior.prob
isr_tech_dt <- data.table(matrix(isr_tech))
setnames(isr_tech_dt, 'isr_bigtech')

# Target
inputs3 <- EMA(xts(data[, list(isr_civtargeting)], order.by = data$date), n = Nsmooth)
# inputs3 <- xts(data[, list(isr_civtargeting)], order.by = data$date)
inputs3 <- inputs3[!is.na(inputs3)]
isr_targ <- bcp_function(inputs3)$posterior.prob
isr_targ_dt <- data.table(matrix(isr_targ))
setnames(isr_targ_dt, 'isr_miltargeting')

# Casualty Ratio
inputs4 <- EMA(xts(data[, list(pal_casratio)], order.by = data$date), n = Nsmooth)
# inputs4 <- xts(data[, list(pal_casratio)], order.by = data$date)
inputs4 <- inputs4[!is.na(inputs4)]
pal_casr <- bcp_function(inputs4)$posterior.prob
pal_casr_dt <- data.table(matrix(pal_casr))
setnames(pal_casr_dt, 'pal_casratio')

############ IMPULSES AND RESPONSES
### Israeli action, Palestinian response
# Casualty ratio
ts_data <- EMA(xts(data[, list(pal_casratio)], order.by = data$date), n = Nsmooth)
# ts_data <- smooth.spline(x, c(data[, list(pal_casratio)])$pal_casratio, spar = spar)$y
ts_data <- ts_data[!is.na(ts_data)]
plot(ts_data)


#############################################################################
input_data <- data.table(isr_targ_dt$isr_miltargeting, isr_loc_dt$mean_isr_pop, isr_tech_dt$isr_bigtech
                         , pal_targ_dt$pal_miltargeting, pal_loc_dt$mean_pal_pop, pal_tech_dt$mean_pal_tech
                         , pal_casr_dt$pal_casratio
                         , as.Date(index(ts_data)))
setnames(input_data, c('isr_targ', 'isr_loc', 'isr_tech', 'pal_targ', 'pal_loc', 'pal_tech', 'pal_casratio', 'date'))
input_data <- input_data[complete.cases(input_data), ]

## VAR TS modeling
var_function <- function(inputs, exog = NULL){
  model1 <- VAR(inputs, exogen = exog, p = 3, ic = 'AIC')
  print(summary(model1))
  return(model1)
}

set.seed(10002)
inputs = xts(input_data[,list(pal_loc, pal_targ, pal_tech, isr_loc, isr_targ, isr_tech)], order.by = input_data$date)
#inputs = xts(input_data[,list(isr_loc, isr_targ, isr_tech, pal_loc, pal_targ, pal_tech)], order.by = input_data$date)
exog = xts(input_data[,list(pal_casratio)], order.by = input_data$date)
model1 <- var_function(inputs, exog = exog)
impulse <- c('isr_loc', 'isr_targ', 'isr_tech')
response <- 'pal_targ'
irf_m1 <- irf(model1, n.ahead = 6, runs = 1000, response = response, impulse = impulse)
plot(irf_m1)
response <- 'pal_tech'
irf_m1 <- irf(model1, n.ahead = 6, runs = 1000, response = response, impulse = impulse)
plot(irf_m1)
response <- 'pal_loc'
irf_m1 <- irf(model1, n.ahead = 6, runs = 1000, response = response, impulse = impulse)
plot(irf_m1)

impulse <- c('pal_loc', 'pal_targ', 'pal_tech')
response <- 'isr_targ'
irf_m1 <- irf(model1, n.ahead = 6, runs = 1000, response = response, impulse = impulse)
plot(irf_m1)
response <- 'isr_tech'
irf_m1 <- irf(model1, n.ahead = 6, runs = 1000, response = response, impulse = impulse)
plot(irf_m1)
response <- 'isr_loc'
irf_m1 <- irf(model1, n.ahead = 6, runs = 1000, response = response, impulse = impulse)
plot(irf_m1)


#### PAL ACTIONS
inputs = xts(input_data[,list(pal_targ, isr_loc, isr_targ, isr_tech)], order.by = input_data$date)
exog = xts(input_data[,list(pal_casratio)], order.by = input_data$date)
model2 <- var_function(inputs, exog = exog)
impulse <- c('isr_loc', 'isr_targ', 'isr_tech')
response <- 'pal_targ'
irf_m2 <- irf(model2, n.ahead = 6, runs = 1000, response = response, impulse = impulse)
plot(irf_m2)

inputs = xts(input_data[,list(pal_tech, isr_loc, isr_targ, isr_tech)], order.by = input_data$date)
#exog = xts(data[start_idx:stop_idx,list(isrdead_mo, paldead_mo)], order.by = data$date[start_idx:stop_idx])
model3 <- var_function(inputs, exog = exog)
impulse <- c('isr_loc', 'isr_targ', 'isr_tech')
response <- 'pal_tech'
irf_m3 <- irf(model3, n.ahead = 6, runs = 1000, response = response, impulse = impulse)
plot(irf_m3)

inputs = xts(input_data[,list(pal_loc, isr_loc, isr_targ, isr_tech)], order.by = input_data$date)
#exog = xts(data[start_idx:stop_idx,list(isrdead_mo, paldead_mo)], order.by = data$date[start_idx:stop_idx])
model4 <- var_function(inputs, exog = exog)
impulse <- c('isr_loc', 'isr_targ', 'isr_tech')
response <- 'pal_loc'
irf_m4 <- irf(model4, n.ahead = 6, runs = 1000, response = response, impulse = impulse)
plot(irf_m4)


#### ISR ACTIONS
inputs = xts(input_data[,list(isr_targ, pal_loc, pal_targ, pal_tech)], order.by = input_data$date)
exog = xts(input_data[,list(pal_casratio)], order.by = input_data$date)
model2 <- var_function(inputs, exog = exog)
impulse <- c('pal_loc', 'pal_targ', 'pal_tech')
response <- 'isr_targ'
irf_m2 <- irf(model2, n.ahead = 6, runs = 1000, response = response, impulse = impulse)
plot(irf_m2)

inputs = xts(input_data[,list(isr_tech, pal_loc, pal_targ, pal_tech)], order.by = input_data$date)
#exog = xts(data[start_idx:stop_idx,list(paldead_mo, paldead_mo)], order.by = data$date[start_idx:stop_idx])
model3 <- var_function(inputs, exog = exog)
impulse <- c('pal_loc', 'pal_targ', 'pal_tech')
response <- 'isr_tech'
irf_m3 <- irf(model3, n.ahead = 6, runs = 1000, response = response, impulse = impulse)
plot(irf_m3)

inputs = xts(input_data[,list(isr_loc, pal_loc, pal_targ, pal_tech)], order.by = input_data$date)
#exog = xts(data[start_idx:stop_idx,list(paldead_mo, paldead_mo)], order.by = data$date[start_idx:stop_idx])
model4 <- var_function(inputs, exog = exog)
impulse <- c('pal_loc', 'pal_targ', 'pal_tech')
response <- 'isr_loc'
irf_m4 <- irf(model4, n.ahead = 6, runs = 1000, response = response, impulse = impulse)
plot(irf_m4)
