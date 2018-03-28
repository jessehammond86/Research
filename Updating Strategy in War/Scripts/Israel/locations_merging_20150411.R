rm(list=ls())
library(data.table)
library(lubridate)
#setwd('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\Israel')
setwd('/media/jesse/Files/Dropbox/Prospectus/Data/Israel')
cities_locs <- data.table(read.csv('cities_merged_20150410.csv', stringsAsFactor = F))
cities_locs <- cities_locs[!duplicated(cities_locs$location1), ]
events_jrh <- data.table(read.csv('Nonfatal Events Formatted 20150411.csv', stringsAsFactor = F))
#events_isrpal <- data.table(read.csv('PCHR Events 20141202.csv', stringsAsFactor = F))
#events_palisr <- data.table(read.csv('PalISr Events 20150410.csv', stringsAsFactor = F))
events_miod <- data.table(read.csv('Miodownik Events 20150411.csv', stringsAsFactor = F))

## Pre-format Hammond data
events_jrh[is.na(pal_combined_wounded), pal_combined_wounded := 0]
events_jrh[is.na(pal_noncombat_wounded), pal_noncombat_wounded := 0]
events_jrh[is.na(isr_noncombat_wounded), isr_noncombat_wounded := 0]
events_jrh[is.na(isr_military_wounded), isr_military_wounded := 0]
events_jrh[is.na(pal_military_wounded), pal_military_wounded := 0]
events_jrh[is.na(isr_police_wounded), isr_police_wounded := 0]
events_jrh[is.na(detainments), detainments := 0]
events_jrh[is.na(propertydamage), propertydamage := 0]

## Merge with location data to get lat/long
events_jrh <- merge(events_jrh, cities_locs[, list(location1, lat, long, givenname)], by = 'location1', all.x = T)
events_jrh$givenname <- NULL
events_jrh[, dyadic := NA]
events_jrh[, event_id := NA]
events_jrh[, dyad_id := NA]

setcolorder(events_jrh, c('year', 'month', 'day', 'location1', 'lat', 'long', 'actor1', 'actor2', 'interaction', 'technologysideA_1'
                          , 'technologysideA_2', 'technologysideB_1', 'technologysideB_2', 'context', 'pal_combined_wounded'
                          , 'pal_noncombat_wounded', 'isr_noncombat_wounded', 'pal_military_wounded', 'isr_military_wounded'
                          , 'isr_police_wounded', 'propertydamage', 'detainments', 'dyadic', 'event_id', 'dyad_id'))
events_jrh[, pal_fatalities := 0L]
events_jrh[, isr_fatalities := 0L]
events_jrh[, for_fatalities := 0L]
events_jrh[, pal_militants_fatalities := 0L]
events_jrh[, pal_military_fatalities := 0L]
events_jrh[, isr_combat_fatalities := 0L]
events_jrh[, isr_military_fatalities := 0L]
events_jrh[, isr_police_fatalities := 0L]
events_jrh[, pal_noncombat_fatalities := 0L]
events_jrh[, isr_noncombat_fatalities := 0L]
events_jrh[, for_noncombat_fatalities := 0L]
events_jrh[, fatalevent := NA]

write.csv(events_jrh, file = 'Nonfatal Events Formatted 20150411_1.csv', row.names = F)


## Format Miodovnik data
events_miod$eventdate <- as.Date(events_miod$eventdate, format = "%Y-%m-%d")
events_miod$year <- 1900+as.POSIXlt(events_miod$eventdate)$year
events_miod$month <- as.POSIXlt(events_miod$eventdate)$mon+1
events_miod$day <- as.POSIXlt(events_miod$eventdate)$mday

events_miod[interaction == 'beating' & is.na(technology1), technology1 := 'blunt']
events_miod[incident_type == 'booby-trapped car', interaction := 'bombing']
events_miod[incident_type == 'booby-trapped car', context := 'car bomb']
events_miod[incident_type == 'explosive belt', interaction := 'bombing']
events_miod[incident_type == 'explosive belt', technology1 := 'belt']
events_miod[incident_type == 'bomb', interaction := 'bombing']
events_miod[incident_type == 'bomb', technology1 := 'belt']
events_miod[, dyadic := NA]
events_miod[duplicated(event_id), dyadic := 1]
events_miod$hostile <- NULL
events_miod$eventdate <- NULL
events_miod$incident_type <- NULL

events_miod[, pal_combined_wounded := 0L]
events_miod[, pal_noncombat_wounded := 0L]
events_miod[, isr_noncombat_wounded := 0L]
events_miod[, isr_military_wounded := 0L]
events_miod[, pal_military_wounded := 0L]
events_miod[, isr_police_wounded := 0L]
events_miod[, propertydamage := 0L]
events_miod[, detainments := 0L]

setnames(events_miod, 'technology1', 'technologysideA_1')
events_miod[, technologysideA_2 := NA]
events_miod[, technologysideB_1 := NA]
events_miod[, technologysideB_2 := NA]
events_miod[, fatalevent := 1]
setcolorder(events_miod, names(events_jrh))

events_miod[, pal_fatalities := sum(pal_fatalities), by = event_id]
events_miod[, isr_fatalities := sum(isr_fatalities), by = event_id]
events_miod[, for_fatalities := sum(for_fatalities), by = event_id]
events_miod[, pal_militants_fatalities := sum(pal_militants_fatalities), by = event_id]
events_miod[, pal_military_fatalities := sum(pal_military_fatalities), by = event_id]
events_miod[, isr_combat_fatalities := sum(isr_combat_fatalities), by = event_id]
events_miod[, isr_military_fatalities := sum(isr_military_fatalities), by = event_id]
events_miod[, isr_police_fatalities := sum(isr_police_fatalities), by = event_id]
events_miod[, pal_noncombat_fatalities := sum(pal_noncombat_fatalities), by = event_id]
events_miod[, isr_noncombat_fatalities := sum(isr_noncombat_fatalities), by = event_id]
events_miod[, for_noncombat_fatalities := sum(for_noncombat_fatalities), by = event_id]
events_miod <- events_miod[!duplicated(event_id), ]



events_jrh$dateloc <- paste(events_jrh$year, events_jrh$month, events_jrh$day, events_jrh$lat, events_jrh$long)
events_miod$dateloc <- paste(events_miod$year, events_miod$month, events_miod$day, events_miod$lat, events_miod$long)
events_jrh$grouped_event <- NA
events_miod[, grouped_event := NA]
events_miod[(events_miod$dateloc %in% events_jrh$dateloc), grouped_event := 1]

events_m_yes <- events_miod[events_miod$dateloc %in% events_jrh$dateloc]
events_m_no <- events_miod[!(events_miod$dateloc %in% events_jrh$dateloc)]

events_full <- rbind(events_jrh, events_miod)
setkeyv(events_full, c('lat', 'long', 'year', 'month', 'day'))


write.csv(events_full, file = 'Full Events 20150411.csv', na = '', row.names = F)

## Merge with location data to get lat/long
events_jrh <- merge(events_jrh, cities_locs[, list(location1, lat, long, givenname)], by = 'location1', all.x = T)
events_miod <- merge(events_miod, cities_locs[, list(district, location1, lat, long, givenname)]
                     , by = c('location1'), all.x = T)

## Select variables to merge from Miodownik
merge_miod <- events_miod[, list(year, month, day, location1, givenname, lat, long
                                 , doneby, belongs_to, interaction, technology1, technology2
                                 ,context, injuries, propertydamage, detainments
                                 , pal_fatalities, isr_fatalities, for_fatalities
                                 , pal_allcombat, pal_militants, pal_military
                                 , demfront, fatah, hamas, islamicjihad, popfront
                                 , popresistance, pal_othermil
                                 , isr_combat, isr_military, isr_police
                                 , pal_noncombat, isr_noncombat, for_noncombat)]
setnames(merge_miod, c('location1', 'doneby', 'belongs_to'), c('location', 'actor1', 'actor2'))

merge_jrh <- events_jrh[, list(year, month, day, location1, givenname, lat, long
                               , actor1, actor2, interaction, technology1, technology2
                               , context, injuries, propertydamage, detainments
                               , pal_fatalities, isr_fatalities, for_fatalities
                               , pal_allcombat, pal_militants, pal_military
                               , demfront, fatah, hamas, islamicjihad, popfront
                               , popresistance, pal_othermil
                               , isr_combat, isr_military, isr_police
                               , pal_noncombat, isr_noncombat, for_noncombat)]
setnames(merge_jrh, 'location1', 'location')



## Stack em and rack em
events_merged <- rbind(merge_jrh, merge_miod)
events_merged[, date := as.POSIXct(paste(year,month,day), format = '%Y%m%d')]
events_merged[, week := floor_date(date, 'week')]
setkeyv(events_merged, c('year', 'month', 'day'))

## Format contexts
events_merged[context %in% c('demonstration', 'clash'), context := 'clash']
events_merged[context %in% c('spontaneous', 'trespassing', ''), context := NA_character_]

## Format actor names
events_merged[actor1 %in% c('di', 'idf', 'israeli military'), actor1 := 'israeli military']
events_merged[actor1 %in% c('palestinian security forces'), actor1 := 'palestinian military']
events_merged[actor1 %in% c('israeli civilian'), actor1 := 'israeli civilians']
events_merged[actor2 %in% c('civilians', 'civilian', 'palestinian civilians', 'palestinian civilian')
              , actor2 := 'palestinian civilians']
events_merged[actor2 %in% c('israeli civilian'), actor2 := 'israeli civilians']
events_merged[actor2 %in% c('foreign civilian', 'foreign citizen'), actor2 := 'foreign civilians']
events_merged[actor2 %in% c('pflp'), actor2 := 'popular front']
events_merged[actor2 %in% c('prc'), actor2 := 'popular resistance committee']
events_merged[actor2 %in% c('suspected of collaboration'), actor2 := 'palestinian civilian']
events_merged[actor2 %in% c('palestinian gunmen', 'gunmen'), actor2 := 'palestinian insurgents']
events_merged[actor2 %in% c('palestinian security forces'), actor2 := 'palestinian military']
events_merged[actor2 %in% c('popular resistance committee'), actor2 := 'popular resistance']



events_merged[, actor1_side := ""]
events_merged[actor1 %in% c('palestinian military'), actor1_side := 'palestinian state forces']
events_merged[actor1 %in% c('democratic front', 'fatah', 'hamas', 'hezbollah'
                                        , 'islamic jihad', 'popular front', 'popular resistance'
                                        ), actor1_side := 'palestinian militants']
events_merged[actor1 %in% c('israeli military', 'israeli police', 'undercover idf'
                                        ), actor1_side := 'israeli state forces']
events_merged[actor1 %in% c('settlers', 'israeli militants'), actor1_side := 'israeli militants']
events_merged[actor1 %in% c('israeli government'), actor1_side := 'israeli government']
events_merged[actor1 %in% c('palestinian authorities'), actor1_side := 'palestinian government']
events_merged[actor1 %in% c('palestinian civilians'), actor1_side := 'palestinian civilians']
events_merged[actor1 %in% c('israeli civilians'), actor1_side := 'israeli civilians']
events_merged[actor1 %in% c('foreign civilians'), actor1_side := 'israeli civilians']
events_merged[actor1 %in% c('unknown'), actor1_side := 'unknown']

events_merged[, actor2_side := ""]
events_merged[actor2 %in% c('agriculture', 'industrial', 'palestinian civilians', 'residential'
                                        ), actor2_side := 'palestinian civilians']
events_merged[actor2 %in% c('palestinian government'), actor2_side := 'palestinian government']
events_merged[actor2 %in% c('democratic front', 'fatah', 'hamas', 'islamic jihad', 'palestinian insurgents'
                                        , 'popular front', 'popular resistance'), actor2_side := 'palestinian militants']
events_merged[actor2 %in% c('palestinian military', 'palestinian police'
                                        ), actor2_side := 'palestinian state forces']
events_merged[actor2 %in% c('israeli civilians'), actor2_side := 'israeli civilians']
events_merged[actor2 %in% c('israeli military', 'israeli police'), actor2_side := 'israeli state forces']
events_merged[actor2 %in% c('foreign civilians'), actor2_side := 'foreign civilians']
events_merged[actor2 %in% c(''), actor2_side := NA_character_]


## Alternate coding for actors 1 & 2: separate different palestinian militant groups
## For multiple actors take FIRST LISTED actor as 'main' actor
events_merged[, actor1_side_palgroups := actor1_side]
events_merged[actor1 %in% 'hamas', actor1_side_palgroups := 'hamas']
events_merged[actor1 %in% 'fatah', actor1_side_palgroups := 'fatah']
events_merged[actor1 %in% 'islamic jihad', actor1_side_palgroups := 'islamic jihad']
events_merged[actor1 %in% 'democratic front', actor1_side_palgroups := 'democratic front']
events_merged[actor1 %in% 'popular front', actor1_side_palgroups := 'popular front']
events_merged[actor1 %in% 'popular resistance', actor1_side_palgroups := 'popular resistance']
events_merged[actor1 %in% 'palestinian insurgents', actor1_side_palgroups := 'palestinian insurgents']

events_merged[, actor2_side_palgroups := actor2_side]
events_merged[actor2 %in% 'hamas', actor2_side_palgroups := 'hamas']
events_merged[actor2 %in% 'fatah', actor2_side_palgroups := 'fatah']
events_merged[actor2 %in% 'islamic jihad', actor2_side_palgroups := 'islamic jihad']
events_merged[actor2 %in% 'democratic front', actor2_side_palgroups := 'democratic front']
events_merged[actor2 %in% 'popular front', actor2_side_palgroups := 'popular front']
events_merged[actor2 %in% 'popular resistance', actor2_side_palgroups := 'popular resistance']
events_merged[actor2 %in% 'palestinian insurgents', actor2_side_palgroups := 'palestinian insurgents']

###### Create EVENT and DIRECTED-DYAD-EVENT ID's
events_merged[, eventid := as.integer(as.factor(paste(year, month, day, givenname, context)))]
events_merged[, dyadid := as.integer(as.factor(paste(year, month, day, givenname, context, actor1)))]


###### Event merging:
## Merge by DATE, PLACE, and ACTOR1:
##  all outcomes (injuries, fatalities, etc) resulting from one actor's actions
events_merged[, eventid := as.integer(as.factor(paste(year, month, day, location)))]
events_merged[, dyadid := as.integer(as.factor(paste(year, month, day, location, actor1)))]
events_merged[, injuries := sum(injuries), by = dyadid]
events_merged[, propertydamage := sum(propertydamage), by = dyadid]
events_merged[, pal_fatalities := sum(pal_fatalities), by = dyadid]
events_merged[, isr_fatalities := sum(isr_fatalities), by = dyadid]
events_merged[, for_fatalities := sum(for_fatalities), by = dyadid]
events_merged[, pal_allcombat := sum(pal_allcombat), by = dyadid]
events_merged[, pal_militants := sum(pal_militants), by = dyadid]
events_merged[, pal_military := sum(pal_military), by = dyadid]
events_merged[, demfront := sum(demfront), by = dyadid]
events_merged[, fatah := sum(fatah), by = dyadid]
events_merged[, hamas := sum(hamas), by = dyadid]
events_merged[, islamicjihad := sum(islamicjihad), by = dyadid]
events_merged[, popfront := sum(popfront), by = dyadid]
events_merged[, popresistance := sum(popresistance), by = dyadid]
events_merged[, pal_othermil := sum(pal_othermil), by = dyadid]
events_merged[, isr_combat := sum(isr_combat), by = dyadid]
events_merged[, isr_military := sum(isr_military), by = dyadid]
events_merged[, isr_police := sum(isr_police), by = dyadid]
events_merged[, pal_noncombat := sum(pal_noncombat), by = dyadid]
events_merged[, isr_noncombat := sum(isr_noncombat), by = dyadid]
events_merged[, for_noncombat := sum(for_noncombat), by = dyadid]


#events_merged[, eventid := as.integer(as.factor(paste(week, givenname, context)))]
#events_merged[, dyadid := as.integer(as.factor(paste(week, givenname, context, actor1)))]

events_out <- events_merged[!duplicated(dyadid)]

events_out[injuries > 0 & pal_fatalities > 0]

write.csv(events_out, file = 'test_fullevents.csv', row.names = F)



plot(events_out[givenname == 'khan yunis and khan yunis refugee camp', injuries])


plot(events_out$lat ~ events_out$long)

