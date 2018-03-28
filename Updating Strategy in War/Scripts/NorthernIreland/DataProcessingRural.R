rm(list = ls())
setwd('/Users/jesse/Dropbox/Dissertation/Data/NorthernIreland/Census Data')
# setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland\\Census Data')
#setwd('/media/jesse/Files/Dropbox/Dissertation/Data/NorthernIreland/Census Data')
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
library(lubridate)

#################
##
## Read in data
##
#################

## Census data
soadata <- readOGR('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland\\IrelandGIS'
                   , 'SOA_agg_2001census')
# soadata <- readOGR('/Users/jesse/Dropbox/Dissertation/Data/NorthernIreland/IrelandGIS'
#                    , 'SOA_agg_2001census'
                   # , stringsAsFactors = F)

## Deaths data
deaths <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland\\IrelandGIS'
                  , layer = 'geodeaths'
                  , stringsAsFactors = F)
# deaths <- readOGR('/Users/jesse/Dropbox/Dissertation/Data/NorthernIreland/IrelandGIS'
#                   , 'geodeaths'
#                   , stringsAsFactors = F)

# Pre-process deaths data
deaths@data$date <- as.Date(deaths@data$date, format = '%m/%d/%Y')
deaths <- deaths[deaths@data$date > as.Date("12/31/1969", format = '%m/%d/%Y')
                 & deaths@data$date <= as.Date("12/31/1998", format = '%m/%d/%Y'), ]
deaths@data$tchnlg1[deaths@data$intrctn == 'beating'] <- 'handheld'
# deaths$quarter <- floor_date(deaths$date, unit = 'year')
deaths$quarter <- as.yearqtr(deaths$date)

## Bases data
bases <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland\\IrelandGIS'
                 , layer = 'geobases')
# bases <- readOGR('/Users/jesse/Dropbox/Dissertation/Data/NorthernIreland/IrelandGIS'
#                  , 'geobases'
#                  , stringsAsFactors = F)

## Peacelines data
peacelines <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland\\IrelandGIS'
                      , layer = 'peacelines')
# peacelines <- readOGR('/Users/jesse/Dropbox/Dissertation/Data/NorthernIreland/IrelandGIS'
#                       , 'peacelines'
#                       , stringsAsFactors = F)
peacelines <- spTransform(peacelines, CRS(proj4string(soadata)))
# Pre-process peacelines data
soadata@data$peacelines <- sapply(over(soadata, peacelines, returnList = T), nrow)
soadata@data$peacelines <- ifelse(soadata@data$peacelines > 0, 1, 0)

## Pre-process combined data
# Bases data by type and per-capita
soadata@data$n_ba_bases <- sapply(over(soadata, bases[bases$Type2 == 'British Army',], returnList = T), nrow)
soadata@data$n_ba_bases <- soadata@data$n_ba_bases / soadata@data$TOTPOP * 1000
soadata@data$n_ir_bases <- sapply(over(soadata, bases[bases$Type2 == 'Irish Security',], returnList = T), nrow)
soadata@data$n_ir_bases <- soadata@data$n_ir_bases / soadata@data$TOTPOP * 1000
# Urban or large-population areas
soadata@data$bigtown <- ifelse(soadata@data$PCTURBN > 0, 1, 0)
# Convert unemployment and religion to percentage
soadata@data$PCTCATH <- soadata@data$PCTCATH/100
soadata@data$PCTPROT <- soadata@data$PCTPROT/100
soadata@data$LTUNEMP <- soadata@data$LTUNEMP/100
soadata@data$LTUNEMP <- soadata@data$LTUNEMP/100


#################
##
## Further preprocessing: generating some new variables
##
#################

## Merge deaths with census data
test <- cbind(deaths@data, over(deaths, soadata)
              , coordinates(spTransform(deaths,  CRS('+proj=utm +zone=29 +ellps=WGS84 +datum=WGS84 +units=m +no_defs '))))
test <- data.table(test)

## Standardize measures
test <- test[!is.na(TOTPOP), ]
test[, STOTPOP := scale(TOTPOP)]
test[, SPOPDENS := scale(POPDENS)]
test[, SPCTCATH := scale(PCTCATH)]
test[, SPCTPROT := scale(PCTPROT)]
test[, SLTUNEMP := scale(LTUNEMP)]
test[, SPCTURBN := scale(PCTURBN)]
test[, SBABASES := scale(n_ba_bases)]
test[, SIRBASES := scale(n_ir_bases)]
test[, BORDER := ifelse(BORDER == 1, 0, 1)]

##### PCA to summarize location characteristics
# devtools::install_github("kassambara/factoextra")
# install.packages('FactoMineR')
library(FactoMineR)
library(factoextra)

## PCA analysis
ni_pca <- PCA(test[, list(POPDENS, PCTCATH, LTUNEMP
                          , BORDER, PCTURBN, WKHRS_M, PCTFARM)]
              , scale.unit = T)
## Summarize and make some cool plots
summary(ni_pca)
fviz_screeplot(ni_pca)
fviz_pca_var(ni_pca)
fviz_pca_var(ni_pca, col.var="cos2") +
  scale_color_gradient2(low="blue", high="red", midpoint=0.7) + theme_minimal()

## Analyze variable contribution to PC dimensions
fviz_contrib(ni_pca, choice = "var", axes = 1)
fviz_contrib(ni_pca, choice = "var", axes = 2)
fviz_contrib(ni_pca, choice = "var", axes = 3)
fviz_contrib(ni_pca, choice = "var", axes = 4)
fviz_contrib(ni_pca, choice = "var", axes = 5)
fviz_contrib(ni_pca, choice = "var", axes = 1:5)

## Summarize 'most important' variables on each dimension
dimdesc(ni_pca, axes = 1:5, proba = 0.01)

## Assigning new variables based on PCA dimensions
test[, c('urbn_vs_farm', 'cath_border', 'ltunemp' ) := data.table(ni_pca$ind$coord[, c(1,2,4)])]


##### Coding actions and actors

## Code perpetrating actors according to overall faction
perps <- unique(deaths$prp_stts_s)
test[prp_stts_s %in% perps[c(5, 8, 10, 11, 12, 13, 15, 16, 25, 28, 29, 31)]
     , PERPFACTION := 'Union']
test[prp_stts_s %in% perps[c(1, 2, 6, 7, 17, 19, 20, 21, 22, 23, 24, 26, 30)]
     , PERPFACTION := 'Republican']
test[prp_stts_s %in% perps[c(4, 9, 14, 18)]
     , PERPFACTION := 'State']

## Code target actors according to overall faction
victims <- unique(deaths$vc_stts_s)
test[vc_stts_s %in% victims[c(8, 9, 10, 14, 17, 30, 32, 34, 38)]
     , VICFACTION := 'Union']
test[vc_stts_s %in% victims[c(2, 5, 7, 12, 13, 16, 18, 23, 24, 28, 29, 35, 37, 39)]
     , VICFACTION := 'Republican']
test[vc_stts_s %in% victims[c(1, 4, 6, 15, 19, 20, 21, 22, 25, 26, 27, 31, 33, 36)]
     , VICFACTION := 'State']
test[vc_stts_s %in% victims[c(3, 11)]
     , VICFACTION := 'Civilian']

test[intrctn %in% c('beating', 'shooting'), VIOLENCE := 'Selective']
test[intrctn %in% c('bombing', 'shelling'), VIOLENCE := 'Indiscriminate']

test[cntxt_m %in% c('Ambush', 'Beating', 'Gun Battle', 'Riot Affray', 'Stabbing'), TACTICS := 'Direct']
test[cntxt_m %in% c('Assasination', 'Explosion', 'Dumped', 'Rocket', 'Sniper', 'Arson'), TACTICS := 'Indirect']

## Code victims according to status and religion
victims <- unique(deaths$vc_stts_s)
test[vc_stts_s %in% victims[c(3, 11)]
     , VICTYPE := 'Civilian']
test[vc_stts_s %in% victims[-c(3, 11)], VICTYPE := 'Military']

test[vc_rlg_s %in% c('Catholic'), VICRELIG := 'Catholic']
test[vc_rlg_s %in% c('Protestant'), VICRELIG := 'Protestant']
test[vc_rlg_s %in% c('nfNI', 'nfNIB', 'nfNIRI'), VICRELIG := 'Irrelevant']

## Keep complete observations of perp/vic/violence
test <- test[complete.cases(test[, list(PERPFACTION, VIOLENCE, VICTYPE)]), ]

#################
##
## Further preprocessing: generating some new variables and reshaping
##  DEATH-centric to LOCATION-centric data
##
#################

test$death <- 1
testmo <- test[, list(
  REPUBKILLS = sum(death[PERPFACTION == 'Republican'])
  , UNIONKILLS = sum(death[PERPFACTION == 'Union'])
  , STATEKILLS = sum(death[PERPFACTION == 'State'])
  , REPUBDEATHS = sum(death[VICFACTION == 'Republican'])
  , UNIONDEATHS = sum(death[VICFACTION == 'Union'])
  , STATEDEATHS = sum(death[VICFACTION == 'State'])
  , CATHDEATHS = sum(death[VICRELIG == 'Catholic'])
  , PROTDEATHS = sum(death[VICRELIG == 'Protestant'])
  , REPUBINDIS = sum(death[PERPFACTION == 'Republican' & VIOLENCE == 'Indiscriminate'])
  , REPUBSELECT = sum(death[PERPFACTION == 'Republican' & VIOLENCE == 'Selective'])
  , UNIONINDIS = sum(death[PERPFACTION == 'Union' & VIOLENCE == 'Indiscriminate'])
  , UNIONSELECT = sum(death[PERPFACTION == 'Union' & VIOLENCE == 'Selective'])
  , STATEINDIS = sum(death[PERPFACTION == 'State' & VIOLENCE == 'Indiscriminate'])
  , STATESELECT = sum(death[PERPFACTION == 'State' & VIOLENCE == 'Selective'])
  , REPUBMILKILLS = sum(death[PERPFACTION == 'Republican' & VICTYPE == 'Military'])
  , REPUBCIVKILLS = sum(death[PERPFACTION == 'Republican' & VICTYPE == 'Civilian'])
  , UNIONMILKILLS = sum(death[PERPFACTION == 'Union' & VICTYPE == 'Military'])
  , UNIONCIVKILLS = sum(death[PERPFACTION == 'Union' & VICTYPE == 'Civilian'])
  , STATEMILKILLS = sum(death[PERPFACTION == 'State' & VICTYPE == 'Military'])
  , STATECIVKILLS = sum(death[PERPFACTION == 'State' & VICTYPE == 'Civilian'])
  , REPUBKILLSBASES = mean(n_ba_bases[PERPFACTION == 'Republican']) + mean(n_ir_bases[PERPFACTION == 'Republican'])
  , UNIONKILLSBASES = mean(n_ba_bases[PERPFACTION == 'Union']) + mean(n_ir_bases[PERPFACTION == 'Union'])
  , STATEKILLSBASES = mean(n_ba_bases[PERPFACTION == 'State']) + mean(n_ir_bases[PERPFACTION == 'State'])
  , REPUBLOCDIM1 = mean(urbn_vs_farm[PERPFACTION == 'Republican'])
  , UNIONLOCDIM1 = mean(urbn_vs_farm[PERPFACTION == 'Union'])
  , STATELOCDIM1 = mean(urbn_vs_farm[PERPFACTION == 'State'])
  , REPUBLOCDIM2 = mean(cath_border[PERPFACTION == 'Republican'])
  , UNIONLOCDIM2 = mean(cath_border[PERPFACTION == 'Union'])
  , STATELOCDIM2 = mean(cath_border[PERPFACTION == 'State'])
  , REPUBLOCDIM3 = mean(ltunemp[PERPFACTION == 'Republican'])
  , UNIONLOCDIM3 = mean(ltunemp[PERPFACTION == 'Union'])
  , STATELOCDIM3 = mean(ltunemp[PERPFACTION == 'State'])
  , REPUBDISTRIB = mean(spDists(cbind(coords.x1[PERPFACTION == 'Republican']
                                      , coords.x1[PERPFACTION == 'Republican']), longlat = F), na.rm = T)
  , UNIONDISTRIB = mean(spDists(cbind(coords.x1[PERPFACTION == 'Union']
                                      , coords.x1[PERPFACTION == 'Union']), longlat = F), na.rm = T)
  , STATEDISTRIB = mean(spDists(cbind(coords.x1[PERPFACTION == 'State']
                                      , coords.x1[PERPFACTION == 'State']), longlat = F), na.rm = T)
), by = list(quarter)]

## Deal with missing data in factor variables
testmo[is.na(REPUBKILLSBASES), REPUBKILLSBASES := 0]
testmo[is.na(UNIONKILLSBASES), UNIONKILLSBASES := 0]
testmo[is.na(STATEKILLSBASES), STATEKILLSBASES := 0]

testmo[is.na(REPUBLOCDIM1), REPUBLOCDIM1 := 0]
testmo[is.na(UNIONLOCDIM1), UNIONLOCDIM1 := 0]
testmo[is.na(STATELOCDIM1), STATELOCDIM1 := 0]
testmo[is.na(REPUBLOCDIM2), REPUBLOCDIM2 := 0]
testmo[is.na(UNIONLOCDIM2), UNIONLOCDIM2 := 0]
testmo[is.na(STATELOCDIM2), STATELOCDIM2 := 0]
testmo[is.na(REPUBLOCDIM3), REPUBLOCDIM3 := 0]
testmo[is.na(UNIONLOCDIM3), UNIONLOCDIM3 := 0]
testmo[is.na(STATELOCDIM3), STATELOCDIM3 := 0]

## Casualty ratio between Catholics and Protestants
testmo[, CATH_CASRATIO := CATHDEATHS / (CATHDEATHS + PROTDEATHS)]
testmo[is.na(CATH_CASRATIO), CATH_CASRATIO := 0]
testmo[, PROT_CASRATIO := PROTDEATHS / (CATHDEATHS + PROTDEATHS)]
testmo[is.na(PROT_CASRATIO), PROT_CASRATIO := 0]

## Casualty ratio between Republicans and Unionists/BA
testmo[, REPUB_CASRATIO := REPUBDEATHS / (REPUBDEATHS + PROTDEATHS + STATEDEATHS)]
testmo[is.na(REPUB_CASRATIO), REPUB_CASRATIO := 0]
testmo[, UNION_CASRATIO := UNIONDEATHS / (REPUBDEATHS + PROTDEATHS + STATEDEATHS)]
testmo[is.na(UNION_CASRATIO), UNION_CASRATIO := 0]
testmo[, STATE_CASRATIO := STATEDEATHS / (REPUBDEATHS + PROTDEATHS + STATEDEATHS)]
testmo[is.na(STATE_CASRATIO), STATE_CASRATIO := 0]

## Scale spatial-distribution measures
testmo[is.nan(REPUBDISTRIB), REPUBDISTRIB := 0]
testmo[, REPUBDISTRIB := scale(REPUBDISTRIB)]
testmo[is.nan(UNIONDISTRIB), UNIONDISTRIB := 0]
testmo[, UNIONDISTRIB := scale(UNIONDISTRIB)]
testmo[is.nan(STATEDISTRIB), STATEDISTRIB := 0]
testmo[, STATEDISTRIB := scale(STATEDISTRIB)]

## Percentage of actions by month
# Selective vs indiscriminate violence
testmo[, repub_indis_pct := REPUBINDIS / ((REPUBINDIS + REPUBSELECT)+1)]
testmo[, repub_select_pct := REPUBINDIS / ((REPUBINDIS + REPUBSELECT)+1)]

testmo[, union_indis_pct := UNIONINDIS / ((UNIONINDIS + UNIONINDIS)+1)]
testmo[, union_select_pct := UNIONSELECT / ((UNIONSELECT + UNIONSELECT)+1)]

testmo[, state_indis_pct := STATEINDIS / ((STATEINDIS + STATEINDIS)+1)]
testmo[, state_select_pct := STATESELECT / ((STATESELECT + STATESELECT)+1)]

# Civilian vs military targeting
testmo[, repub_civ_pct := REPUBCIVKILLS / ((REPUBCIVKILLS + REPUBMILKILLS)+1)]
testmo[, repub_mil_pct := REPUBMILKILLS / ((REPUBCIVKILLS + REPUBMILKILLS)+1)]

testmo[, union_civ_pct := UNIONCIVKILLS / ((UNIONCIVKILLS + UNIONMILKILLS)+1)]
testmo[, union_mil_pct := UNIONMILKILLS / ((UNIONCIVKILLS + UNIONMILKILLS)+1)]

testmo[, state_civ_pct := STATECIVKILLS / ((STATECIVKILLS + STATEMILKILLS)+1)]
testmo[, state_mil_pct := STATEMILKILLS / ((STATECIVKILLS + STATEMILKILLS)+1)]




## Write out to CSV for future use
write.csv(test, file = '/Users/jesse/Dropbox/Dissertation/Data/NorthernIreland/GeoDeaths.csv', row.names = F)
write.csv(testmo, file = '/Users/jesse/Dropbox/Dissertation/Data/NorthernIreland/GeoMonthlyDeaths.csv', row.names = F)
