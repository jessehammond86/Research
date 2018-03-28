rm(list = ls())
# setwd('/Users/jesse/Dropbox/Dissertation/Data/NorthernIreland')
# setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland')
setwd('/media/jesse/Files/Dropbox/Dissertation/Data/NorthernIreland')
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
# soadata <- readOGR('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland\\IrelandGIS'
                   # , 'SOA_agg_2001census')
soadata <- readOGR(paste0(getwd(), '/IrelandGIS')
                   , 'SOA_agg_2001census'
                   , stringsAsFactors = F)

## Deaths data
# deaths <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland\\IrelandGIS'
                  # , layer = 'geodeaths'
                  # , stringsAsFactors = F)
deaths <- readOGR(paste0(getwd(), '/IrelandGIS')
                  , 'geodeaths'
                  , stringsAsFactors = F)

# Pre-process deaths data
deaths@data$date <- as.Date(deaths@data$date, format = '%m/%d/%Y')
deaths <- deaths[deaths@data$date > as.Date("12/31/1969", format = '%m/%d/%Y')
                 & deaths@data$date <= as.Date("12/31/1998", format = '%m/%d/%Y'), ]
deaths@data$tchnlg1[deaths@data$intrctn == 'beating'] <- 'handheld'
# deaths$quarter <- floor_date(deaths$date, unit = 'year')
deaths$quarter <- as.yearqtr(deaths$date)

## Bases data
# bases <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland\\IrelandGIS'
                 # , layer = 'geobases')
bases <- readOGR(paste0(getwd(), '/IrelandGIS')
                 , 'geobases'
                 , stringsAsFactors = F)

## Peacelines data
# peacelines <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland\\IrelandGIS'
                      # , layer = 'peacelines')
peacelines <- readOGR(paste0(getwd(), '/IrelandGIS')
                      , 'peacelines'
                      , stringsAsFactors = F)
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
test[, TOTPOP := log(TOTPOP+1)]
test[, BORDER := ifelse(BORDER == 1, 0, 1)]

## Code perpetrating actors according to overall faction
perps <- unique(deaths$prp_stts_s)
test[prp_stts_s %in% perps[c(5, 8, 10, 11, 12, 13, 15, 16, 25, 28, 29, 31)]
     | prp_stts_s %in% perps[c(4, 9, 14, 18)]
     , PERPFACTION := 'Union']
test[prp_stts_s %in% perps[c(1, 2, 6, 7, 17, 19, 20, 21, 22, 23, 24, 26, 30)]
     , PERPFACTION := 'Republican']
test[prp_stts_s %in% perps[c(4, 9, 14, 18)]
     , PERPFACTION := 'State']

## Code target actors according to overall faction
victims <- unique(deaths$vc_stts_s)
test[vc_stts_s %in% victims[c(8, 9, 10, 14, 17, 30, 32, 34, 38)]
     | vc_stts_s %in% victims[c(1, 4, 6, 15, 19, 20, 21, 22, 25, 26, 27, 31, 33, 36)]
     , VICFACTION := 'Union']
test[vc_stts_s %in% victims[c(2, 5, 7, 12, 13, 16, 18, 23, 24, 28, 29, 35, 37, 39)]
     , VICFACTION := 'Republican']
test[vc_stts_s %in% victims[c(1, 4, 6, 15, 19, 20, 21, 22, 25, 26, 27, 31, 33, 36)]
     , VICFACTION := 'State']
test[vc_stts_s %in% victims[c(3, 11)]
     , VICFACTION := 'Civilian']

## Code violence by type of violence and tactic
test[intrctn %in% c('beating', 'shooting'), VIOLENCE := 'Selective']
test[intrctn %in% c('bombing', 'shelling'), VIOLENCE := 'Indiscriminate']

test[cntxt_m %in% c('Ambush', 'Assasination', 'Beating', 'Dumped', 'Gun Battle'
                    , 'Riot Affray', 'Stabbing')
     , TACTICS := 'Direct']
test[cntxt_m %in% c('Assasination', 'Explosion', 'Rocket', 'Sniper', 'Arson')
     , TACTICS := 'Indirect']

## Code victims according to status and religion
victims <- unique(deaths$vc_stts_s)
test[vc_stts_s %in% victims[c(3, 11)], VICTYPE := 'Civilian']
test[vc_stts_s %in% victims[-c(3, 11)], VICTYPE := 'Military']

test[vc_rlg_s %in% c('Catholic'), VICRELIG := 'Catholic']
test[vc_rlg_s %in% c('Protestant'), VICRELIG := 'Protestant']
test[vc_rlg_s %in% c('nfNI', 'nfNIB', 'nfNIRI'), VICRELIG := 'Irrelevant']

## Keep complete observations of perp/vic/violence
test <- test[complete.cases(test[, list(PERPFACTION, VIOLENCE, VICTYPE)]), ]

## Create composite 'score' of events by type/target/location
test[, repub_score := 0]
test[, union_score := 0]
test[, state_score := 0]

# test[PERPFACTION == 'Republican' & (BELFAST == 1 | DERRY == 1), repub_score := repub_score + 1]
# test[PERPFACTION == 'Union' & (BELFAST == 1 | DERRY == 1), union_score := union_score + 1]
# test[PERPFACTION == 'State' & (BELFAST == 1 | DERRY == 1), state_score := state_score + 1]

test[PERPFACTION == 'Republican' & (BELFAST == 1 | DERRY == 1), repub_score := repub_score + 1]
test[PERPFACTION == 'Union' & (BELFAST == 1 | DERRY == 1), union_score := union_score + 1]
test[PERPFACTION == 'State' & (BELFAST == 1 | DERRY == 1), state_score := state_score + 1]

test[PERPFACTION == 'Republican' & VIOLENCE == 'Indiscriminate', repub_score := repub_score + 1]
test[PERPFACTION == 'Union' & VIOLENCE == 'Indiscriminate', union_score := union_score + 1]
test[PERPFACTION == 'State' & VIOLENCE == 'Indiscriminate', state_score := state_score + 1]

test[PERPFACTION == 'Republican' & VICTYPE == 'Civilian', repub_score := repub_score + 1]
test[PERPFACTION == 'Union' & VICTYPE == 'Civilian', union_score := union_score + 1]
test[PERPFACTION == 'State' & VICTYPE == 'Civilian', state_score := state_score + 1]

#################
##
## Further preprocessing: generating some new variables and reshaping
##  DEATH-centric to LOCATION-centric data
##
#################

process_data <- function(x){
  test_out <- x[, list(
    REPUBKILLS = sum(PERPFACTION == 'Republican')
    , UNIONKILLS = sum(PERPFACTION == 'Union')
    , STATEKILLS = sum(PERPFACTION == 'State')
    , REPUBDEATHS = sum(VICFACTION == 'Republican')
    , UNIONDEATHS = sum(VICFACTION == 'Union')
    , STATEDEATHS = sum(VICFACTION == 'State')
    , CATHDEATHS = sum(VICRELIG == 'Catholic')
    , PROTDEATHS = sum(VICRELIG == 'Protestant')
    , REPUBINDIS = sum(PERPFACTION == 'Republican' & VIOLENCE == 'Indiscriminate')
    , REPUBSELECT = sum(PERPFACTION == 'Republican' & VIOLENCE == 'Selective')
    , UNIONINDIS = sum(PERPFACTION == 'Union' & VIOLENCE == 'Indiscriminate')
    , UNIONSELECT = sum(PERPFACTION == 'Union' & VIOLENCE == 'Selective')
    , STATEINDIS = sum(PERPFACTION == 'State' & VIOLENCE == 'Indiscriminate')
    , STATESELECT = sum(PERPFACTION == 'State' & VIOLENCE == 'Selective')
    , REPUBMILKILLS = sum(PERPFACTION == 'Republican' & VICTYPE == 'Military')
    , REPUBCIVKILLS = sum(PERPFACTION == 'Republican' & VICTYPE == 'Civilian')
    , UNIONMILKILLS = sum(PERPFACTION == 'Union' & VICTYPE == 'Military')
    , UNIONCIVKILLS = sum(PERPFACTION == 'Union' & VICTYPE == 'Civilian')
    , STATEMILKILLS = sum(PERPFACTION == 'State' & VICTYPE == 'Military')
    , STATECIVKILLS = sum(PERPFACTION == 'State' & VICTYPE == 'Civilian')
    , REPUBKILLSBASES = mean(mean(n_ba_bases[PERPFACTION == 'Republican']) + mean(n_ir_bases[PERPFACTION == 'Republican']))
    , UNIONKILLSBASES = mean(mean(n_ba_bases[PERPFACTION == 'Union']) + mean(n_ir_bases[PERPFACTION == 'Union']))
    , STATEKILLSBASES = mean(mean(n_ba_bases[PERPFACTION == 'State']) + mean(n_ir_bases[PERPFACTION == 'State']))
    , REPUBKILLSCATH = mean(PCTCATH[PERPFACTION == 'Republican'])
    , UNIONKILLSCATH = mean(PCTCATH[PERPFACTION == 'Union'])
    , STATEKILLSCATH = mean(PCTCATH[PERPFACTION == 'State'])
    , REPUBURBAN = sum(PERPFACTION == 'Republican' & (BELFAST == 1 | DERRY == 1))
    , UNIONURBAN = sum(PERPFACTION == 'Union' & (BELFAST == 1 | DERRY == 1))
    , STATEURBAN = sum(PERPFACTION == 'State' & (BELFAST == 1 | DERRY == 1))
    , REPUBSCORE = mean(repub_score[PERPFACTION == 'Republican'])
    , UNIONSCORE = mean(union_score[PERPFACTION == 'Union'])
    , STATESCORE = mean(state_score[PERPFACTION == 'State'])
  ), by = list(quarter)]
  ## Casualty ratio between Catholics and Protestants
  test_out[, CATH_CASRATIO := CATHDEATHS / (CATHDEATHS + PROTDEATHS)]
  # test_out[is.na(CATH_CASRATIO), CATH_CASRATIO := 0]
  test_out[, PROT_CASRATIO := PROTDEATHS / (CATHDEATHS + PROTDEATHS)]
  # test_out[is.na(PROT_CASRATIO), PROT_CASRATIO := 0]

  ## Casualty ratio between Republicans and Unionists/BA
  test_out[, REPUB_CASRATIO := REPUBDEATHS / (REPUBDEATHS + PROTDEATHS + STATEDEATHS)]
  # test_out[is.na(REPUB_CASRATIO), REPUB_CASRATIO := 0]
  test_out[, UNION_CASRATIO := UNIONDEATHS / (REPUBDEATHS + PROTDEATHS + STATEDEATHS)]
  # test_out[is.na(UNION_CASRATIO), UNION_CASRATIO := 0]
  test_out[, STATE_CASRATIO := STATEDEATHS / (REPUBDEATHS + PROTDEATHS + STATEDEATHS)]
  # test_out[is.na(STATE_CASRATIO), STATE_CASRATIO := 0]

  ## Percentage of fatal attacks in/out of urban areas
  test_out[, REPUBURBAN := REPUBURBAN / REPUBKILLS]
  test_out[, UNIONURBAN := UNIONURBAN / UNIONKILLS]
  test_out[, STATEURBAN := STATEURBAN / STATEKILLS]
  
  ## Scale spatial-distribution measures
  # test_out[is.nan(REPUBDISTRIB), REPUBDISTRIB := 0]
  # test_out[, REPUBDISTRIB := scale(REPUBDISTRIB)]
  # test_out[is.nan(UNIONDISTRIB), UNIONDISTRIB := 0]
  # test_out[, UNIONDISTRIB := scale(UNIONDISTRIB)]
  # test_out[is.nan(STATEDISTRIB), STATEDISTRIB := 0]
  # test_out[, STATEDISTRIB := scale(STATEDISTRIB)]

  ## Percentage of actions by month
  # Selective vs indiscriminate violence
  test_out[, repub_indis_pct := REPUBINDIS / ((REPUBINDIS + REPUBSELECT)+1)]
  test_out[, repub_select_pct := REPUBINDIS / ((REPUBINDIS + REPUBSELECT)+1)]

  test_out[, union_indis_pct := UNIONINDIS / ((UNIONINDIS + UNIONINDIS)+1)]
  test_out[, union_select_pct := UNIONSELECT / ((UNIONSELECT + UNIONSELECT)+1)]

  test_out[, state_indis_pct := STATEINDIS / ((STATEINDIS + STATEINDIS)+1)]
  test_out[, state_select_pct := STATESELECT / ((STATESELECT + STATESELECT)+1)]

  # Civilian vs military targeting
  test_out[, repub_civ_pct := REPUBCIVKILLS / ((REPUBCIVKILLS + REPUBMILKILLS)+1)]
  test_out[, repub_mil_pct := REPUBMILKILLS / ((REPUBCIVKILLS + REPUBMILKILLS)+1)]

  test_out[, union_civ_pct := UNIONCIVKILLS / ((UNIONCIVKILLS + UNIONMILKILLS)+1)]
  test_out[, union_mil_pct := UNIONMILKILLS / ((UNIONCIVKILLS + UNIONMILKILLS)+1)]

  test_out[, state_civ_pct := STATECIVKILLS / ((STATECIVKILLS + STATEMILKILLS)+1)]
  test_out[, state_mil_pct := STATEMILKILLS / ((STATECIVKILLS + STATEMILKILLS)+1)]

  return(test_out)
}

ni_data <- process_data(test)
belfast_data <- process_data(test[BELFAST == 1])
rural_data <- process_data(test[BELFAST == 0])


#################
##
## Fill in missing data points with previous values
##
#################

## Nice function from Stackoverflow to deal with NA's
repNAmed <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
    x
  }  else {
    x[is.na(x)] <- names(which.max(table(x)))
    x
  }
}

replaceNaWithLatest <- function(dfIn, nameColNa = names(dfIn)[1]
){
  dtTest <- data.table(dfIn)
  setnames(dtTest, nameColNa, "colNa")
  dtTest[is.nan(colNa), colNa := NA]
  dtTest[, segment := cumsum(!is.na(colNa))]
  dtTest[, colNa := colNa[1], by = "segment"]
  dtTest[, segment := NULL]
  setnames(dtTest, "colNa", nameColNa)
  return(dtTest)
}

for(name in names(ni_data)[-1]){
  ni_data <- replaceNaWithLatest(ni_data, name)
  belfast_data <- replaceNaWithLatest(belfast_data, name)
  rural_data <- replaceNaWithLatest(rural_data, name)

  ni_data[which(!is.finite(name) | is.na(name)), name] <- 0
  belfast_data[which(!is.finite(name) | is.na(name)), name] <- 0
  rural_data[which(!is.finite(name) | is.na(name)), name] <- 0
}


for(name in names(ni_data)[-1]){
  ni_data <- replaceNaWithLatest(ni_data, name)
  belfast_data <- replaceNaWithLatest(belfast_data, name)
  rural_data <- replaceNaWithLatest(rural_data, name)
}


## Write out to CSV for future use
# Overall data (for mapping)
write.csv(test, file = paste0(getwd(), '/GeoDeaths.csv'), row.names = F)
# Quarterly data (for temporal modeling)
write.csv(ni_data, file = paste0(getwd(), '/NI_quarter_deaths.csv'), row.names = F)
write.csv(belfast_data, file = paste0(getwd(), '/belfast_quarter_deaths.csv'), row.names = F)
write.csv(rural_data, file = paste0(getwd(), '/rural_quarter_deaths.csv'), row.names = F)
