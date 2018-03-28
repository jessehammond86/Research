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
data <- data[interactio %in% violent_events]

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

data[, palcas_mo := paldead_mil_mo]
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
data[, pal_event := sum(event == 1 & actor1 %in% pal_milactors & interactio %in% violent_events), by = week]
data[, isr_event := sum(event == 1 & actor1 %in% isr_milactors & interactio %in% violent_events), by = week]
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
data[, pal_civtargeting := as.numeric(sum(actor1 %in% pal_milactors & actor2 %in% isr_civactors & interactio %in% violent_events)), by = week]
data[pal_event > 0, pal_civtargeting := pal_civtargeting / pal_event, by = week]
data[, pal_miltargeting := as.numeric(sum(actor1 %in% pal_milactors & actor2 %in% isr_milactors & interactio %in% violent_events)), by = week]
data[pal_event > 0, pal_miltargeting := pal_miltargeting / pal_event, by = week]

data[, isr_civtargeting := as.numeric(sum(actor1 %in% isr_milactors & actor2 %in% pal_civactors & interactio %in% violent_events)), by = week]
data[isr_event > 0, isr_civtargeting := isr_civtargeting / isr_event, by = week]
data[, isr_miltargeting := as.numeric(sum(actor1 %in% isr_milactors & actor2 %in% pal_milactors & interactio %in% violent_events)), by = week]
data[isr_event > 0, isr_miltargeting := isr_miltargeting / isr_event, by = week]

### Collapse to year-month level
old_data2 <- data
#data <- old_data2
data <- data[!duplicated(data[, week]), ]
#data <- data[!duplicated(data[, list(year,month)]), ]


#################################################################
######## Preliminary models
######## Changepoint detection (CPT)
library(bcp)
library(forecast)
library(TTR)
bcp_function <- function(inputs){
  for(i in 1:ncol(inputs)){
    inputs[, i] <- (inputs[, i] - mean(inputs[, i])) / sd(inputs[, i])
  }
#   inputs <- ts(matrix(c(0,HoltWinters(inputs, beta = F, gamma =F)$fitted[, 2]), ncol = 4))
  pal_combined1_ecp <- bcp(inputs, w0 = .01, p0 = .01, burnin = 1000, mcmc = 10000)
  plot(pal_combined1_ecp, separated = T)
  return(pal_combined1_ecp)
}
### Palestinian strategy
# Location
# inputs1 <- EMA(xts(data[, list(log(mean_palevent_pop+1))], order.by = data$week), n = 4)
inputs1 <- EMA(xts(data[, list(mean_palevent_bdist)], order.by = data$week), n = 4)
inputs1 <- inputs1[!is.na(inputs1)]
pal_loc <- e.divisive(inputs1, R = 999, sig.lvl = 0.025, min.size = 16)
pal_loc_dt <- data.table(matrix(inputs1))
setnames(pal_loc_dt, 'mean_pal_pop')
pal_loc_dt[, regime := pal_loc$cluster]
pal_loc_dt[, regime_mean := mean(mean_pal_pop), by = regime]
plot(inputs1)
abline(v = .index(inputs1)[pal_loc$estimates[-c(1, length(pal_loc$estimates))]], col = 'red', lty = 1, lwd = 2)
lines(x = .index(inputs1), y =c(pal_loc_dt$regime_mean), lty = 2, col = 'red')

# Technology
inputs2 <- EMA(xts(data[, list(pal_indirect)], order.by = data$week), n = 4)
inputs2 <- inputs2[!is.na(inputs2)]
pal_tech <- e.divisive(inputs2, R = 999, sig.lvl = 0.025, min.size = 16)
pal_tech_dt <- data.table(matrix(inputs2))
setnames(pal_tech_dt, 'pal_indirect')
pal_tech_dt[, regime := pal_tech$cluster]
pal_tech_dt[, regime_mean := mean(pal_indirect), by = regime]
plot(inputs2)
abline(v = .index(inputs2)[pal_tech$estimates[-c(1, length(pal_tech$estimates))]], col = 'red', lty = 1, lwd = 1)
lines(x = .index(inputs2), y = c(pal_tech_dt$regime_mean), lty = 2, col = 'red')

# Target
inputs3 <- EMA(xts(data[, list(pal_civtargeting)], order.by = data$week), n = 4)
inputs3 <- inputs3[!is.na(inputs3)]
pal_targ <- e.divisive(inputs3, R = 999, sig.lvl = 0.025, min.size = 16)
pal_targ_dt <- data.table(matrix(inputs3))
setnames(pal_targ_dt, 'pal_miltargeting')
pal_targ_dt[, regime := pal_targ$cluster]
pal_targ_dt[, regime_mean := mean(pal_miltargeting), by = regime]
plot(inputs3)
abline(v = .index(inputs3)[pal_targ$estimates[-c(1, length(pal_targ$estimates))]], col = 'red', lty = 1, lwd = 2)
lines(x = .index(inputs3), y = c(pal_targ_dt$regime_mean), lty = 2, col = 'red')

pal_changepoints <- unique(sort(c(pal_loc$estimates[-c(1, length(pal_loc$estimates))], 
                                  pal_tech$estimates[-c(1, length(pal_tech$estimates))], 
                                  pal_targ$estimates[-c(1, length(pal_targ$estimates))])))

### Israeli strategy
# Location
# inputs1 <- EMA(xts(data[, list(log(mean_isrevent_pop+1))], order.by = data$week), n = 4)
inputs1 <- EMA(xts(data[, list(mean_isrevent_bdist)], order.by = data$week), n = 4)
inputs1 <- inputs1[!is.na(inputs1)]
isr_loc <- e.divisive(inputs1, R = 999, sig.lvl = 0.025, min.size = 16)
isr_loc_dt <- data.table(matrix(inputs1))
setnames(isr_loc_dt, 'mean_isr_pop')
isr_loc_dt[, regime := isr_loc$cluster]
isr_loc_dt[, regime_mean := mean(mean_isr_pop), by = regime]
plot(inputs1)
abline(v = .index(inputs1)[isr_loc$estimates[-c(1, length(isr_loc$estimates))]], col = 'red', lty = 1, lwd = 2)
lines(x = .index(inputs1), y =c(isr_loc_dt$regime_mean), lty = 2, col = 'red')


# Technology
inputs2 <- EMA(xts(data[, list(isr_bigtech)], order.by = data$week), n = 4)
inputs2 <- inputs2[!is.na(inputs2)]
isr_tech <- e.divisive(inputs2, R = 999, sig.lvl = 0.025, min.size = 16)
isr_tech_dt <- data.table(matrix(inputs2))
setnames(isr_tech_dt, 'isr_bigtech')
isr_tech_dt[, regime := isr_tech$cluster]
isr_tech_dt[, regime_mean := mean(isr_bigtech), by = regime]
plot(inputs2)
abline(v = .index(inputs2)[isr_tech$estimates[-c(1, length(isr_tech$estimates))]], col = 'red', lty = 1, lwd = 1)
lines(x = .index(inputs2), y = c(isr_tech_dt$regime_mean), lty = 2, col = 'red')

# Target
inputs3 <- EMA(xts(data[, list(isr_civtargeting)], order.by = data$week), n = 4)
inputs3 <- inputs3[!is.na(inputs3)]
isr_targ <- e.divisive(inputs3, R = 999, sig.lvl = 0.025, min.size = 16)
isr_targ_dt <- data.table(matrix(inputs3))
setnames(isr_targ_dt, 'isr_miltargeting')
isr_targ_dt[, regime := isr_targ$cluster]
isr_targ_dt[, regime_mean := mean(isr_miltargeting), by = regime]
plot(inputs3)
abline(v = .index(inputs3)[isr_targ$estimates[-c(1, length(isr_targ$estimates))]], col = 'red', lty = 1, lwd = 2)
lines(x = .index(inputs3), y = c(isr_targ_dt$regime_mean), lty = 2, col = 'red')

isr_changepoints <- unique(sort(c(isr_loc$estimates[-c(1, length(isr_loc$estimates))], 
                           isr_tech$estimates[-c(1, length(isr_tech$estimates))], 
                           isr_targ$estimates[-c(1, length(isr_targ$estimates))])))

# Casualty Ratio
inputs3 <- EMA(xts(data[, list(pal_casratio)], order.by = data$week), n = 4)
inputs3 <- inputs3[!is.na(inputs3)]
pal_casr <- e.divisive(inputs3, R = 999, sig.lvl = 0.025, min.size = 8)
pal_casr_dt <- data.table(matrix(inputs3))
setnames(pal_casr_dt, 'pal_casratio_w')
pal_casr_dt[, regime := pal_casr$cluster]
pal_casr_dt[, regime_mean := mean(pal_casratio_w), by = regime]
plot(inputs3)
abline(v = .index(inputs3)[pal_casr$estimates[-c(1, length(pal_casr$estimates))]], col = 'red', lty = 1, lwd = 2)
lines(x = .index(inputs3), y = c(pal_casr_dt$regime_mean), lty = 2, col = 'red')


############ IMPULSES AND RESPONSES
### Israeli action, Palestinian response
# Casualty ratio
ts_data <- xts(data[, list(pal_casratio)], order.by = data$week)
ts_data <- EMA(ts_data, n = 4)
ts_data <- ts_data[!is.na(ts_data)]
plot(ts_data)
lines(x = .index(ts_data), y = c(pal_casr_dt$regime_mean), lty = 2, col = 'red', lwd = 2)
par(new = T)
plot(x = .index(ts_data), y = isr_loc_dt$regime_mean, type = "l", axes = FALSE, bty = "n"
     , xlab = "", ylab = "", lty = 2, col = 'blue', lwd = 2, ylim = c(-10, 5))
lines(x = .index(ts_data), y = c(pal_loc_dt$regime_mean), lty = 2, col = 'green', lwd = 2)
par(new = T)
plot(x = .index(ts_data), y = isr_tech_dt$regime_mean, type = "l", axes = FALSE, bty = "n"
     , xlab = "", ylab = "", lty = 2, col = 'blue', lwd = 2, ylim = c(0, 1))
lines(x = .index(ts_data), y = c(isr_targ_dt$regime_mean), lty = 2, col = 'blue', lwd = 2)
lines(x = .index(ts_data), y = c(pal_tech_dt$regime_mean), lty = 2, col = 'green', lwd = 2)
lines(x = .index(ts_data), y = c(pal_targ_dt$regime_mean), lty = 2, col = 'green', lwd = 2)


abline(v = .index(ts_data)[pal_loc$estimates[-c(1, length(pal_loc$estimates))]], col = 'green', lty = 2, lwd = 1)
abline(v = .index(ts_data)[pal_tech$estimates[-c(1, length(pal_tech$estimates))]], col = 'green', lty = 2, lwd = 1)
abline(v = .index(ts_data)[pal_targ$estimates[-c(1, length(pal_targ$estimates))]], col = 'green', lty = 2, lwd = 1)
abline(v = .index(ts_data)[isr_loc$estimates[-c(1, length(isr_loc$estimates))]], col = 'blue', lty = 2, lwd = 1)
abline(v = .index(ts_data)[isr_tech$estimates[-c(1, length(isr_tech$estimates))]], col = 'blue', lty = 2, lwd = 1)
abline(v = .index(ts_data)[isr_targ$estimates[-c(1, length(isr_targ$estimates))]], col = 'blue', lty = 2, lwd = 1)
abline(v = .index(ts_data)[pal_casr$estimates[-c(1, length(pal_casr$estimates))]], col = 'red', lty = 2, lwd = 1)

# 
# # Palestinian casualties
# ts_data <- xts(data[, palcas_mo], order.by = data$week)
# ts_data <- EMA(ts_data, n = 4)
# ts_data <- ts_data[!is.na(ts_data)]
# plot(ts_data, type = 'l')
# abline(v = .index(inputs1)[pal_loc$estimates[-c(1, length(pal_loc$estimates))]], col = 'red', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[pal_tech$estimates[-c(1, length(pal_tech$estimates))]], col = 'blue', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[pal_targ$estimates[-c(1, length(pal_targ$estimates))]], col = 'green', lty = 2, lwd = 1)
# 
# # Israeli actions
# ts_data <- EMA(xts(data[, list(log(mean_isrevent_pop+1))], order.by = data$week), n = 4)
# plot(ts_data, type = 'l')
# abline(v = .index(inputs1)[pal_loc$estimates[-c(1, length(pal_loc$estimates))]], col = 'red', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[pal_tech$estimates[-c(1, length(pal_tech$estimates))]], col = 'blue', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[pal_targ$estimates[-c(1, length(pal_targ$estimates))]], col = 'green', lty = 2, lwd = 1)
# 
# ts_data <- EMA(xts(data[, list(isr_bigtech)], order.by = data$week), n = 4)
# plot(ts_data, type = 'l')
# abline(v = .index(inputs1)[pal_loc$estimates[-c(1, length(pal_loc$estimates))]], col = 'red', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[pal_tech$estimates[-c(1, length(pal_tech$estimates))]], col = 'blue', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[pal_targ$estimates[-c(1, length(pal_targ$estimates))]], col = 'green', lty = 2, lwd = 1)
# 
# ts_data <- EMA(xts(data[, list(isr_miltargeting)], order.by = data$week), n = 4)
# plot(ts_data, type = 'l')
# abline(v = .index(inputs1)[pal_loc$estimates[-c(1, length(pal_loc$estimates))]], col = 'red', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[pal_tech$estimates[-c(1, length(pal_tech$estimates))]], col = 'blue', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[pal_targ$estimates[-c(1, length(pal_targ$estimates))]], col = 'green', lty = 2, lwd = 1)
# 
# 
# 
# # Casualty ratio
# ts_data <- xts(data[, list(pal_casratio * (paldead_mo + palwound_mo))], order.by = data$week)
# # ts_data <- xts(data[, list((paldead_mo + palwound_mo))], order.by = data$week)
# ts_data <- EMA(ts_data, n = 4)
# plot(ts_data, type = 'l')
# plot(x, z, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
# axis(side=4, at = pretty(range(z)))
# abline(v = .index(inputs1)[isr_loc$estimates[-c(1, length(isr_loc$estimates))]], col = 'red', lty = 1, lwd = 1)
# abline(v = .index(inputs1)[isr_tech$estimates[-c(1, length(isr_tech$estimates))]], col = 'red', lty = 1, lwd = 1)
# abline(v = .index(inputs1)[isr_targ$estimates[-c(1, length(isr_targ$estimates))]], col = 'red', lty = 1, lwd = 1)
# abline(v = .index(inputs1)[pal_loc$estimates[-c(1, length(pal_loc$estimates))]], col = 'blue', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[pal_tech$estimates[-c(1, length(pal_tech$estimates))]], col = 'blue', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[pal_targ$estimates[-c(1, length(pal_targ$estimates))]], col = 'blue', lty = 2, lwd = 1)
# 
# 
# # Israeli casualties
# ts_data <- EMA(xts(data[, log(isrcas_mo+1)], order.by = data$week), n = 3)
# plot(ts_data, type = 'l')
# abline(v = .index(inputs1)[isr_loc$estimates[-c(1, length(isr_loc$estimates))]-3], col = 'red', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[isr_tech$estimates[-c(1, length(isr_tech$estimates))]-3], col = 'blue', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[isr_targ$estimates[-c(1, length(isr_targ$estimates))]-3], col = 'green', lty = 2, lwd = 1)
# 
# # Palestinian actions
# ts_data <- EMA(xts(data[, list(log(mean_palevent_pop+1))], order.by = data$week), n = 3)
# plot(ts_data, type = 'l')
# abline(v = .index(inputs1)[isr_loc$estimates[-c(1, length(isr_loc$estimates))]-3], col = 'red', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[isr_tech$estimates[-c(1, length(isr_tech$estimates))]-3], col = 'blue', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[isr_targ$estimates[-c(1, length(isr_targ$estimates))]-3], col = 'green', lty = 2, lwd = 1)
# 
# ts_data <- EMA(xts(data[, list(pal_indirect)], order.by = data$week), n = 3)
# plot(ts_data, type = 'l')
# abline(v = .index(inputs1)[isr_loc$estimates[-c(1, length(isr_loc$estimates))]-3], col = 'red', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[isr_tech$estimates[-c(1, length(isr_tech$estimates))]-3], col = 'blue', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[isr_targ$estimates[-c(1, length(isr_targ$estimates))]-3], col = 'green', lty = 2, lwd = 1)
# 
# ts_data <- EMA(xts(data[, list(pal_miltargeting)], order.by = data$week), n = 3)
# plot(ts_data, type = 'l')
# abline(v = .index(inputs1)[isr_loc$estimates[-c(1, length(isr_loc$estimates))]-3], col = 'red', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[isr_tech$estimates[-c(1, length(isr_tech$estimates))]-3], col = 'blue', lty = 2, lwd = 1)
# abline(v = .index(inputs1)[isr_targ$estimates[-c(1, length(isr_targ$estimates))]-3], col = 'green', lty = 2, lwd = 1)


#############################################################################
input_data <- data.table(isr_targ_dt$regime_mean, isr_loc_dt$regime_mean, isr_tech_dt$regime_mean
                         , pal_targ_dt$regime_mean, pal_loc_dt$regime_mean, pal_tech_dt$regime_mean
                         , pal_casr_dt$regime_mean
                         , as.Date(index(ts_data)))
setnames(input_data, c('isr_targ', 'isr_loc', 'isr_tech', 'pal_targ', 'pal_loc', 'pal_tech', 'pal_casratio', 'week'))

input_data[, change_isr_targ := c(0,diff(input_data$isr_targ))]
input_data[, change_isr_targ := ifelse(change_isr_targ != 0, 1, change_isr_targ)]

input_data[, change_pal_targ := c(0,diff(input_data$pal_targ))]
input_data[, change_pal_targ := ifelse(change_pal_targ != 0, 1, change_pal_targ)]

input_data[, change_isr_tech := c(0,diff(input_data$isr_tech))]
input_data[, change_isr_tech := ifelse(change_isr_tech != 0, 1, change_isr_tech)]

input_data[, change_pal_tech := c(0,diff(input_data$pal_tech))]
input_data[, change_pal_tech := ifelse(change_pal_tech != 0, 1, change_pal_tech)]

input_data[, change_isr_loc := c(0,diff(input_data$isr_loc))]
input_data[, change_isr_loc := ifelse(change_isr_loc != 0, 1, change_isr_loc)]

input_data[, change_pal_loc := c(0,diff(input_data$pal_loc))]
input_data[, change_pal_loc := ifelse(change_pal_loc != 0, 1, change_pal_loc)]

input_data[, change_pal_any := ifelse(change_pal_targ > 0 | change_pal_tech > 0 | change_pal_loc > 0, 1, 0)]

input_data[, change_pal_casratio := c(0,diff(input_data$pal_casratio))]
input_data[, change_pal_casratio := ifelse(change_pal_casratio != 0, 1, change_pal_casratio)]

input_data[, weeks_isr_targ := seq(0, .N-1), by = isr_targ]
input_data$weeks_isr_targ[which(input_data$weeks_isr_targ == 0)[-1]] <- input_data$weeks_isr_targ[(which(input_data$weeks_isr_targ == 0)-1)[-1]]+1
input_data[, weeks_isr_tech := seq(0, .N-1), by = isr_tech]
input_data$weeks_isr_tech[which(input_data$weeks_isr_tech == 0)[-1]] <- input_data$weeks_isr_tech[(which(input_data$weeks_isr_tech == 0)-1)[-1]]+1
input_data[, weeks_isr_loc := seq(0, .N-1), by = isr_loc]
input_data$weeks_isr_loc[which(input_data$weeks_isr_loc == 0)[-1]] <- input_data$weeks_isr_loc[(which(input_data$weeks_isr_loc == 0)-1)[-1]]+1
input_data[, weeks_pal_targ := seq(0, .N-1), by = pal_targ]
input_data$weeks_pal_targ[which(input_data$weeks_pal_targ == 0)[-1]] <- input_data$weeks_pal_targ[(which(input_data$weeks_pal_targ == 0)-1)[-1]]+1
input_data[, weeks_pal_tech := seq(0, .N-1), by = pal_tech]
input_data$weeks_pal_tech[which(input_data$weeks_pal_tech == 0)[-1]] <- input_data$weeks_pal_tech[(which(input_data$weeks_pal_tech == 0)-1)[-1]]+1
input_data[, weeks_pal_loc := seq(0, .N-1), by = pal_loc]
input_data$weeks_pal_loc[which(input_data$weeks_pal_loc == 0)[-1]] <- input_data$weeks_pal_loc[(which(input_data$weeks_pal_loc == 0)-1)[-1]]+1
input_data[, weeks_pal_casratio := seq(0, .N-1), by = pal_casratio]
input_data$weeks_pal_casratio[which(input_data$weeks_pal_casratio == 0)[-1]] <- input_data$weeks_pal_casratio[(which(input_data$weeks_pal_casratio == 0)-1)[-1]]+1

input_data[, time := 1:nrow(input_data)]
input_data[, time2 := time^2]
input_data[, time3 := time^3]


###### SIMPLE MODELS
summary(glm(change_pal_targ ~ weeks_isr_targ + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_pal_targ ~ weeks_isr_tech + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_pal_targ ~ weeks_isr_loc + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))

summary(glm(change_pal_tech ~ weeks_isr_targ + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_pal_tech ~ weeks_isr_tech + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_pal_tech ~ weeks_isr_loc + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))

summary(glm(change_pal_loc ~ weeks_isr_targ + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_pal_loc ~ weeks_isr_tech + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_pal_loc ~ weeks_isr_loc + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))


summary(glm(change_isr_targ ~ weeks_pal_targ + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_isr_targ ~ weeks_pal_tech + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_isr_targ ~ weeks_pal_loc + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))

summary(glm(change_isr_tech ~ weeks_pal_targ + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_isr_tech ~ weeks_pal_tech + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_isr_tech ~ weeks_pal_loc + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))

summary(glm(change_isr_loc ~ weeks_pal_targ + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_isr_loc ~ weeks_pal_tech + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_isr_loc ~ weeks_pal_loc + time + time2 + time3, data = input_data, family = binomial(link = 'logit')))

##### MODELS WITH TIME
summary(glm(change_pal_targ ~ weeks_isr_targ, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_pal_targ ~ weeks_isr_tech, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_pal_targ ~ weeks_isr_loc, data = input_data, family = binomial(link = 'logit')))

summary(glm(change_pal_tech ~ weeks_isr_targ, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_pal_tech ~ weeks_isr_tech, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_pal_tech ~ weeks_isr_loc, data = input_data, family = binomial(link = 'logit')))

summary(glm(change_pal_loc ~ weeks_isr_targ, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_pal_loc ~ weeks_isr_tech, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_pal_loc ~ weeks_isr_loc, data = input_data, family = binomial(link = 'logit')))


summary(glm(change_isr_targ ~ weeks_pal_targ, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_isr_targ ~ weeks_pal_tech, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_isr_targ ~ weeks_pal_loc, data = input_data, family = binomial(link = 'logit')))

summary(glm(change_isr_tech ~ weeks_pal_targ, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_isr_tech ~ weeks_pal_tech, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_isr_tech ~ weeks_pal_loc, data = input_data, family = binomial(link = 'logit')))

summary(glm(change_isr_loc ~ weeks_pal_targ, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_isr_loc ~ weeks_pal_tech, data = input_data, family = binomial(link = 'logit')))
summary(glm(change_isr_loc ~ weeks_pal_loc, data = input_data, family = binomial(link = 'logit')))






model <- glm(change_pal_tech ~ isr_targ + weeks_isr_targ + isr_tech + weeks_isr_tech + isr_loc + weeks_isr_loc
             # + weeks_pal_targ + weeks_pal_loc
             + pal_casratio*weeks_pal_casratio
             + time + time2 + time3
             , data = input_data
             , family = binomial(link = 'logit'))
foo <- stepAIC(model, direction='both',  steps = 10000, scope = list(lower = ~time+time2+time3))
summary(foo)

model <- glm(change_pal_targ ~ isr_targ + weeks_isr_targ + isr_tech + weeks_isr_tech + isr_loc + weeks_isr_loc
             # + weeks_pal_tech + weeks_pal_loc
             + pal_casratio*weeks_pal_casratio
             + time + time2 + time3
             , data = input_data
             , family = binomial(link = 'logit'))
foo <- stepAIC(model, direction='both',  steps = 10000, scope = list(lower = ~time+time2+time3))
summary(foo)

model <- glm(change_pal_loc ~ isr_targ + weeks_isr_targ + isr_tech + weeks_isr_tech + isr_loc + weeks_isr_loc
             + weeks_pal_tech + weeks_pal_targ
             + pal_casratio + weeks_pal_casratio
             + time + time2 + time3
             , data = input_data
             , family = binomial(link = 'logit'))
foo <- stepAIC(model, direction='both',  steps = 10000, scope = list(lower = ~time+time2+time3))
summary(foo)


input_data[, isr_targ := c(0,diff(input_data$isr_targ))]
input_data[, isr_targ := ifelse(isr_targ > 0, 1, isr_targ)]
input_data[, isr_targ := ifelse(isr_targ < 0, -1, isr_targ)]

input_data[, pal_targ := c(0,diff(input_data$pal_targ))]
input_data[, pal_targ := ifelse(pal_targ > 0, 1, pal_targ)]
input_data[, pal_targ := ifelse(pal_targ < 0, -1, pal_targ)]

input_data[, isr_tech := c(0,diff(input_data$isr_tech))]
input_data[, isr_tech := ifelse(isr_tech > 0, 1, isr_tech)]
input_data[, isr_tech := ifelse(isr_tech < 0, -1, isr_tech)]

input_data[, pal_tech := c(0,diff(input_data$pal_tech))]
input_data[, pal_tech := ifelse(pal_tech > 0, 1, pal_tech)]
input_data[, pal_tech := ifelse(pal_tech < 0, -1, pal_tech)]

input_data[, isr_loc := c(0,diff(input_data$isr_loc))]
input_data[, isr_loc := ifelse(isr_loc > 0, 1, isr_loc)]
input_data[, isr_loc := ifelse(isr_loc < 0, -1, isr_loc)]

input_data[, pal_loc := c(0,diff(input_data$pal_loc))]
input_data[, pal_loc := ifelse(pal_loc > 0, 1, pal_loc)]
input_data[, pal_loc := ifelse(pal_loc < 0, -1, pal_loc)]

input_data[, pal_casratio := c(0,diff(input_data$pal_casratio))]
input_data[, pal_casratio := ifelse(pal_casratio > 0, 1, pal_casratio)]
input_data[, pal_casratio := ifelse(pal_casratio < 0, -1, pal_casratio)]






ts_data <- xts(input_data$pal_casratio, order.by = input_data$week)
plot(ts_data, main = "", ylim = c(-1,1))
points(x = .index(ts_data)[input_data$pal_targ != 0], y = input_data$pal_targ[input_data$pal_targ != 0], main = "", col = 'green')
points(x = .index(ts_data)[input_data$pal_loc != 0], y = input_data$pal_loc[input_data$pal_loc != 0], main = "", col = 'green')
points(x = .index(ts_data)[input_data$pal_tech != 0], y = input_data$pal_tech[input_data$pal_tech != 0], main = "", col = 'green')

# Israeli and Palestinian actions
inputs = xts(data[,list(isr_event, pal_event)], order.by = data$week)
exog = xts(data[,list(isrdead_mo, paldead_mo)], order.by = data$week)
inputs <- scale(inputs); exog <- scale(exog)
var_function(inputs, exog)

# Israeli and Palestinian civilian targeting
inputs = xts(input_data[,list(isr_targ, pal_targ)], order.by = input_data$week)
exog = xts(input_data[,list(pal_casratio)], order.by = input_data$week)
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


