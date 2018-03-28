setwd('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS')
library(rgdal)
library(foreign)
data <- read.dbf('MergedSOA2011_Centroids_WGS84.dbf')
data <- read.dbf('SOA2011.dbf')
write.dbf(data, 'SOA2011_old.dbf')

data$SOA_LABEL <- gsub('_1', '', data$SOA_LABEL)
data$SOA_LABEL <- gsub('_2', '', data$SOA_LABEL)
data$SOA_LABEL <- gsub('_3', '', data$SOA_LABEL)
data$SOA_LABEL <- gsub('_4', '', data$SOA_LABEL)
data$SOA_LABEL <- gsub('_5', '', data$SOA_LABEL)
data$SOA_LABEL <- gsub('_6', '', data$SOA_LABEL)
data$SOA_LABEL <- gsub('_7', '', data$SOA_LABEL)
data$SOA_LABEL <- gsub('_8', '', data$SOA_LABEL)

write.dbf(data, 'SOA2011.dbf')


setwd('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland')
library(rgdal)
library(foreign)
library(data.table)

sutton <- fread('SuttonDeathsTableMod_20141218.csv')
mckeown <- fread('McKeownDeaths_20141203.csv')
locations <- fread('LocationsMerging_20141217.csv')

merged_data <- merge(sutton, mckeown, by = c('Name', 'Year'), all.x = T, all.y = T)
merged_data <- merge(merged_data, locations, by = c('City', 'Neighborhood', 'Street'), all.x = T, allow.cartesian = T)

merged_data[, Details := tolower(Details)]

merged_data[, interaction := NA_character_]
merged_data[, technology1 := NA_character_]
merged_data[, technology2 := NA_character_]

merged_data[grep('shot', Details), interaction := 'shooting']
merged_data[grep('shot', Details), technology1 := 'small arms']

merged_data[grep('gun', Details), interaction := 'shooting']
merged_data[grep('gun', Details), technology1 := 'small arms']

merged_data[grep('rubber bullet', Details), interaction := 'shooting']
merged_data[grep('rubber bullet', Details), technology1 := 'rubber bullets']

merged_data[grep('stabb', Details), interaction := 'beating']
merged_data[grep('stabb', Details), technology1 := 'handheld']

merged_data[grep('beat', Details), interaction := 'beating']
merged_data[grep('beat', Details), technology1 := 'handheld']

merged_data[grep('strangl', Details), interaction := 'beating']
merged_data[grep('strangl', Details), technology1 := 'handheld']

merged_data[grep('brick', Details), interaction := 'beating']
merged_data[grep('brick', Details), technology1 := 'handheld']

merged_data[grep('knocked down', Details), interaction := 'beating']
merged_data[grep('knocked down', Details), interaction := 'handheld']

merged_data[grep('explo', Details), interaction := 'bombing']
merged_data[grep('explo', Details), technology1 := 'explosives']

merged_data[grep('bomb', Details), interaction := 'bombing']
merged_data[grep('bomb', Details), technology1 := 'explosives']

merged_data[grep('arson', Details), interaction := 'bombing']
merged_data[grep('arson', Details), technology1 := 'incendiaries']

merged_data[grep('mine', Details), interaction := 'bombing']
merged_data[grep('mine', Details), technology1 := 'land mine']

merged_data[grep('grenade', Details), interaction := 'bombing']
merged_data[grep('grenade', Details), technology1 := 'grenade']


merged_data[grep('mortar', Details), interaction := 'shelling']
merged_data[grep('mortar', Details), technology1 := 'mortar']

merged_data[grep('rocket', Details), interaction := 'shelling']
merged_data[grep('rocket', Details), technology1 := 'rocket']

merged_data[grep('inform', Details), interaction := 'execution']
merged_data[grep('inform', Details), technology1 := 'small arms']

merged_data[Context == 'Riot Affray' & is.na(interaction), interaction := 'beating']

setnames(merged_data, 'New Incident', 'NewIncident')
setnames(merged_data, 'Multiple Fatality', 'MultipleFatality')

data_out <- merged_data[, list(Year, Date, interaction, technology1
                               , Religion.x, Religion.y, Status.x, Status.y, KilledBy, Agency, Details
                               , Rationale, Causality, Context, NewIncident, MultipleFatality, latitude, longitude)]
setnames(data_out, c('year', 'date', 'interaction', 'technology1', 'vic_relig_s', 'vic_relig_m'
                     , 'vic_status_s', 'vic_status_m', 'perp_status_s', 'perp_status_m'
                     , 'details_s', 'rationale_m', 'causality_m', 'context_m', 'newincident_m', 'multiplefatality_m'
                     , 'latitude', 'longitude'))
write.csv(data_out
          , file = 'MergedDeaths_20141229.csv', row.names = F)
