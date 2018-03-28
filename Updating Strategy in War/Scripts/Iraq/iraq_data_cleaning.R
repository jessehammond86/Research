rm(list = ls())
setwd('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\Iraq\\Events')
library(data.table)
library(foreign)

data <- fread('iraq_formatted_20140420.csv')
table(data$category)

data[, category := tolower(category)]
data[, type := tolower(type)]

data <- data[!(category %in% c('<null value>', 'ambush', 'deliberate attack', 'accident', 'arrest', 'arson', 'ambush threat', 'arty', 'assassination threat', 'attack threat', 'blue-blue'
                               , 'border ops', 'breaching', 'cache found/cleared'
                               , 'carjacking', 'carjacking threat', 'confiscation', 'convoy', 'detain', 'detention', 'direct fire threat'
                               , 'elicitation', 'equipment failure', 'extortion', 'explosive remnants of war (erw) found/cleared'
                               , 'explosive remnants of war (erw)/turn in'
                               , 'green-blue', 'green-green', 'hijacking', 'ied false', 'ied hoax', 'ied suspected'
                               , 'ied threat', 'indirect fire threat', 'intimidation threat', 'kidnapping threat', 'lasing', 'looting', 'looting threat'
                               , 'medevac', 'meeting', 'movement to contact', 'mugging', 'murder', 'murder threat', 'natural disaster', 'none selected'
                               , 'other', 'other offensive', 'other defensive', 'propaganda', 'police actions', 'raid threat', 'recon', 'recon threat'
                               , 'repetitive activities', 'sabotage threat', 'safire threat'
                               , 'sermon', 'small arms threat', 'smuggling', 'smuggling threat', 'sniper ops threat', 'staff estimate'
                               , 'supporting aif', 'supporting cf', 'surveillance', 'tests of security'
                               , 'tcp', 'theft', 'theft threat', 'tribal feud', 'unknown explosion', 'vehicle interdiction', 'white-white'))
                       ,]
data <- data[!attackon == 'FALSE']
data <- data[!type %in% c('criminal event', 'other', 'suspicious incident', 'friendly fire', 'threat report')]
table(data$category)
table(data$category, data$type)


data[, 'technology_sidea_1' := NA_character_]

data$technology_sidea_1[data$type %in% 'enemy action'][grep('mortar', tolower(data$summary[data$type %in% 'enemy action']))] <- 'mortar'
data$technology_sidea_1[data$category %in% 'indirect fire'][grep('mortar', tolower(data$summary[data$category %in% 'indirect fire']))] <- 'mortar'
data$technology_sidea_1[data$category %in% 'indirect fire'][grep('impact', tolower(data$summary[data$category %in% 'indirect fire']))] <- 'mortar'
data$technology_sidea_1[data$category %in% 'indirect fire'][grep('round', tolower(data$summary[data$category %in% 'indirect fire']))] <- 'mortar'
data$technology_sidea_1[data$category %in% 'indirect fire'][grep(' rds ', tolower(data$summary[data$category %in% 'indirect fire']))] <- 'mortar'
data$technology_sidea_1[data$category %in% 'indirect fire'][grep(' indirect fire ', tolower(data$summary[data$category %in% 'indirect fire']))] <- 'mortar'
data$technology_sidea_1[data$category %in% 'indirect fire'][grep(' idf ', tolower(data$summary[data$category %in% 'indirect fire']))] <- 'mortar'
data$technology_sidea_1[data$category %in% 'indirect fire'][grep(' mm ', tolower(data$summary[data$category %in% 'indirect fire']))] <- 'mortar'

data$technology_sidea_1[data$category %in% 'indirect fire'][grep('rocket', tolower(data$summary[data$category %in% 'indirect fire']))] <- 'anti-tank missile'
data$technology_sidea_1[data$category %in% 'indirect fire'][grep('rpg', tolower(data$summary[data$category %in% 'indirect fire']))] <- 'anti-tank missile'
data$technology_sidea_1[data$category %in% 'indirect fire'][grep('missile', tolower(data$summary[data$category %in% 'indirect fire']))] <- 'anti-tank missile'


data$technology_sidea_1[data$category %in% 'direct fire'][grep('small arms', tolower(data$summary[data$category %in% 'direct fire']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'direct fire'][grep('safire', tolower(data$summary[data$category %in% 'direct fire']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'direct fire'][grep('received saf', tolower(data$summary[data$category %in% 'direct fire']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'direct fire'][grep('reported saf', tolower(data$summary[data$category %in% 'direct fire']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'direct fire'][grep('attacked with saf', tolower(data$summary[data$category %in% 'direct fire']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'direct fire'][grep('drive by', tolower(data$summary[data$category %in% 'direct fire']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'direct fire'][grep('ak-', tolower(data$summary[data$category %in% 'direct fire']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'direct fire'][grep('direct fire', tolower(data$summary[data$category %in% 'direct fire']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'direct fire'][grep('rocket', tolower(data$title[data$category %in% 'direct fire']))] <- 'anti-tank missile'

data$technology_sidea_1[data$category %in% 'attack'][grep('safire', tolower(data$title[data$category %in% 'attack']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'attack'][grep('saf ', tolower(data$title[data$category %in% 'attack']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'attack'][grep('drive by ', tolower(data$title[data$category %in% 'attack']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'attack'][grep('rive by ', tolower(data$title[data$category %in% 'attack']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'attack'][grep('drive-by ', tolower(data$title[data$category %in% 'attack']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'attack'][grep('drive-by', tolower(data$title[data$category %in% 'attack']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'attack'][grep('small arms', tolower(data$title[data$category %in% 'attack']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'attack'][grep('grenade', tolower(data$title[data$category %in% 'attack']))] <- 'grenade'
data$technology_sidea_1[data$category %in% 'attack'][grep('rock', tolower(data$title[data$category %in% 'attack']))] <- 'stones'
data$technology_sidea_1[data$category %in% 'attack'][grep('rpg', tolower(data$title[data$category %in% 'attack']))] <- 'anti-tank missiles'
data$technology_sidea_1[data$category %in% 'attack'][grep('mortar', tolower(data$title[data$category %in% 'attack']))] <- 'mortar'
data$technology_sidea_1[data$category %in% 'attack'][grep('indirect', tolower(data$title[data$category %in% 'attack']))] <- 'mortar'
data$technology_sidea_1[data$category %in% 'attack'][grep('explosion', tolower(data$title[data$category %in% 'attack']))] <- 'explosives'
data$technology_sidea_1[data$category %in% 'attack'][grep('attack', tolower(data$summary[data$category %in% 'attack']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'attack'][grep('rock', tolower(data$summary[data$category %in% 'attack']))] <- 'stones'
data$technology_sidea_1[data$category %in% 'attack'][grep('attack', tolower(data$summary[data$category %in% 'attack']))] <- 'small arms'

data$technology_sidea_1[data$type %in% 'enemy action'][grep('grenade', tolower(data$summary[data$type %in% 'enemy action']))] <- 'grenade'

data$technology_sidea_1[data$category %in% 'assassination']<- 'small arms'

data$technology_sidea_1[data$category %in% 'escalation of force'][grep('62mm', tolower(data$summary[data$category %in% 'escalation of force']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'escalation of force'][grep('56mm', tolower(data$summary[data$category %in% 'escalation of force']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'escalation of force'][grep('9mm', tolower(data$summary[data$category %in% 'escalation of force']))] <- 'small arms'

data$technology_sidea_1[data$category %in% 'safire'][grep('smarm', tolower(data$summary[data$category %in% 'safire']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'safire'][grep('burst', tolower(data$summary[data$category %in% 'safire']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'safire'][grep('round', tolower(data$summary[data$category %in% 'safire']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'safire'][grep('saf', tolower(data$summary[data$category %in% 'safire']))] <- 'small arms'
data$technology_sidea_1[data$category %in% 'safire'][grep('aa', tolower(data$summary[data$category %in% 'safire']))] <- 'medium arms'
data$technology_sidea_1[data$category %in% 'safire'][grep('aaa', tolower(data$summary[data$category %in% 'safire']))] <- 'medium arms'
data$technology_sidea_1[data$category %in% 'safire'][grep('caliber', tolower(data$summary[data$category %in% 'safire']))] <- 'medium arms'
data$technology_sidea_1[data$category %in% 'safire'][grep('launch', tolower(data$summary[data$category %in% 'safire']))] <- 'surface-air missile'
data$technology_sidea_1[data$category %in% 'safire'][grep('missile', tolower(data$summary[data$category %in% 'safire']))] <- 'surface-air missile'
data$technology_sidea_1[data$category %in% 'safire'][grep('launch', tolower(data$summary[data$category %in% 'safire']))] <- 'surface-air missile'
data$technology_sidea_1[data$category %in% 'safire'][grep('projectile', tolower(data$summary[data$category %in% 'safire']))] <- 'surface-air missile'


data[category %in% c('uav'), technology_sidea_1 := 'drone']
data[grep('uav', tolower(data$summary)), technology_sidea_1 := 'drone']

data[category %in% c('close air support'), technology_sidea_1 := 'aircraft']
data[grep(' awt ', tolower(data$summary)), technology_sidea_1 := 'aircraft']
data[grep('fixed wing', tolower(data$summary)), technology_sidea_1 := 'aircraft']

data[type %in% c('explosive hazard'), technology_sidea_1 := 'explosives']


sum(is.na(data[type %in% 'enemy action', technology_sidea_1]))
table(data$category, is.na(data$technology_sidea_1))

write.csv(data$summary[(data$category %in% c('attack')) & data$attackon %in% 'ENEMY' &
                         is.na(data$technology_sidea_1)], file = 'summary_outputs2.csv', row.names = F)



