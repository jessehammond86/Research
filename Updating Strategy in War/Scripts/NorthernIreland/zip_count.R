rm(list = ls())
setwd('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\Census Data')
# setwd('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/Census Data')
library(rgdal)
library(foreign)
library(data.table)
library(raster)
library(rgeos)
library(maptools)
library(spdep)
library(spatcounts)
library(pscl)
library(stargazer)
library(MASS)

ni_crs <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ')


soadata <- readOGR('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
         , 'SOA_agg_2001census')
# soadata <- readOGR('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
#                    , 'SOA_agg_2001census')
soadata@proj4string <- ni_crs
soadata <- spTransform(soadata, ni_crs)
deaths <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
         , layer = 'geodeaths')
# deaths <- readOGR(dsn = '/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
#                   , layer = 'geodeaths')
deaths@proj4string <- ni_crs
deaths <- spTransform(deaths, ni_crs)
deaths@data$date <- as.Date(deaths@data$date, format = '%m/%d/%Y')
deaths$ira_prp <- ifelse(deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(7,8,9,10,14,16,17,18,21,27,24)], 1, 0)
deaths$ulster_prp <- ifelse(deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(11,12,13,22,23,28,29,30,31,32)], 1, 0)
deaths$army_prp <- ifelse(deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(1,2,25,26)], 1, 0)
deaths$civ_vic <- ifelse(deaths@data$vc_stts_s %in% levels(deaths$vc_stts_s)[c(4,5)], 1, 0)
deaths$mil_vic <- ifelse(deaths@data$vc_stts_s %in% levels(deaths$vc_stts_s)[c(1:3, 6:39)], 1, 0)

bases <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
         , layer = 'geobases')
# bases <- readOGR(dsn = '/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
#                  , layer = 'geobases')

bases@proj4string <- ni_crs
bases <- spTransform(bases, ni_crs)

peacelines <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
                 , layer = 'peacelines')
# peacelines <- readOGR(dsn = '/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
#                       , layer = 'peacelines')
peacelines <- spTransform(peacelines, ni_crs)

soadata@data$peacelines <- sapply(over(soadata, peacelines, returnList = T), nrow)
soadata@data$peacelines <- ifelse(soadata@data$peacelines > 0, 1, 0)

soadata@data$n_ba_bases <- sapply(over(soadata, bases[bases$Type2 == 'British Army',], returnList = T), nrow)
soadata@data$n_ba_bases <- soadata@data$n_ba_bases / soadata@data$TOTPOP * 1000
soadata@data$n_ir_bases <- sapply(over(soadata, bases[bases$Type2 == 'Irish Security',], returnList = T), nrow)
soadata@data$n_ir_bases <- soadata@data$n_ir_bases / soadata@data$TOTPOP * 1000
soadata@data$n_roadblocks <- sapply(over(soadata, bases[bases$Type2 %in% c('Checkpoint', 'Roadblock'),], returnList = T), nrow)
soadata@data$n_roadblocks <- soadata@data$n_roadblocks / soadata@data$TOTPOP * 1000

soadata@data$deaths <- sapply(over(soadata, deaths, returnList = T), nrow)
soadata@data$cath_deaths <- sapply(over(soadata, deaths[deaths@data$vc_rlg_s == 'Catholic',], returnList = T), nrow)
soadata@data$prot_deaths <- sapply(over(soadata, deaths[deaths@data$vc_rlg_s == 'Protestant',], returnList = T), nrow)

soadata@data$ira_kills <- sapply(over(soadata, deaths[deaths@data$ira_prp == 1, ]
                                      , returnList = T), nrow)

soadata@data$ulster_kills <- sapply(over(soadata, deaths[deaths@data$ulster_prp == 1, ]
                                      , returnList = T), nrow)

soadata@data$army_kills <- sapply(over(soadata, deaths[deaths@data$army_prp == 1, ]
                                         , returnList = T), nrow)



soadata@data$ira_civ_kills <- sapply(over(soadata, deaths[deaths@data$ira_prp == 1
                                                          & deaths@data$civ_vic == 1,]
                                      , returnList = T), nrow)

soadata@data$ira_mil_kills <- sapply(over(soadata, deaths[deaths@data$ira_prp == 1
                                                          & deaths@data$mil_vic == 1,]
                                          , returnList = T), nrow)

soadata@data$ira_indis <- sapply(over(soadata, deaths[deaths@data$ira_prp == 1
                                                      & deaths@data$intrctn %in% c('bombing', 'shelling'),]
                                      , returnList = T), nrow)

soadata@data$ira_select <- sapply(over(soadata, deaths[deaths@data$ira_prp == 1
                                                      & deaths@data$intrctn %in% c('shooting', 'beating'),]
                                      , returnList = T), nrow)


soadata@data$ira_civ_sel <- sapply(over(soadata, deaths[deaths@data$ira_prp == 1
                                                       & deaths@data$intrctn %in% c('shooting', 'beating')
                                                       & deaths@data$civ_vic == 1,]
                                       , returnList = T), nrow)

soadata@data$ira_civ_ind <- sapply(over(soadata, deaths[deaths@data$ira_prp == 1
                                                        & deaths@data$intrctn %in% c('bombing', 'shelling')
                                                        & deaths@data$civ_vic == 1,]
                                        , returnList = T), nrow)

soadata@data$ira_mil_sel <- sapply(over(soadata, deaths[deaths@data$ira_prp == 1
                                                        & deaths@data$intrctn %in% c('shooting', 'beating')
                                                        & deaths@data$mil_vic == 1,]
                                        , returnList = T), nrow)

soadata@data$ira_mil_ind <- sapply(over(soadata, deaths[deaths@data$ira_prp == 1
                                                        & deaths@data$intrctn %in% c('bombing', 'shelling')
                                                        & deaths@data$mil_vic == 1,]
                                        , returnList = T), nrow)

soadata@data$PCTCATH <- soadata@data$PCTCATH/100
soadata@data$PCTCATH2 <- soadata@data$PCTCATH
soadata@data$PCTCATHSQ <- soadata@data$PCTCATH^2

######## IRA VIOLENCE
ira_full <- zeroinfl(ira_kills ~ 
                         n_ba_bases 
                       + n_ir_bases 
                       + peacelines
                       + n_roadblocks
                       + PCTCATH 
                       + PCTCATH:PCTCATH2
                       + ulster_kills
                       + army_kills
                       | LTUNEMP 
                       + PCTURBN
                       + POPDENS 
                       + DERRY
                       + BELFAST
                       , data = soadata@data)
summary(ira_full)

######## SELECTIVE IRA VIOLENCE
ira_select <- zeroinfl(ira_select ~ 
                       n_ba_bases 
                       + n_ir_bases 
                       + peacelines
                       + n_roadblocks
                       + PCTCATH 
                       + PCTCATH:PCTCATH2
                       + ulster_kills
                       + army_kills
                       + ira_indis
                       | LTUNEMP 
                       + PCTURBN
                       + POPDENS 
                       + DERRY
                       + BELFAST
                       , data = soadata@data)
summary(ira_select)

######## INDISCRIMINATE IRA VIOLENCE
ira_indis <- zeroinfl(ira_indis ~ 
                        n_ba_bases 
                      + n_ir_bases 
                      + peacelines
                      + n_roadblocks
                      + PCTCATH 
                      + PCTCATH:PCTCATH2
                      + ulster_kills
                      + army_kills
                      + ira_select
                      + LTUNEMP 
                      + POPDENS 
                      | LTUNEMP 
                      + PCTURBN
                      + POPDENS 
                      + DERRY
                      + BELFAST
                      , data = soadata@data
                      , dist = 'negbin')
summary(ira_indis)

######################################################################################################
######## SELECTIVE IRA VIOLENCE AGAINST CIVILIANS
ira_select_civ <- zeroinfl(ira_civ_sel ~ 
                             n_ba_bases 
                           + n_ir_bases 
                           + peacelines
                           + n_roadblocks
                           + PCTCATH 
                           + PCTCATH:PCTCATH2
                           + LTUNEMP 
                           + POPDENS
                           + ulster_kills
                           + army_kills
                           + ira_civ_ind
                           + ira_mil_sel
                           + ira_mil_ind 
                           | LTUNEMP 
                           #+ PCTURBN
                           + POPDENS
                           + DERRY
                           + BELFAST
                           , data = soadata@data
                           , dist = 'negbin')
summary(ira_select_civ)

######## INDISCRIMINATE IRA VIOLENCE AGAINST CIVILIANS
ira_indis_civ <- zeroinfl(ira_civ_ind ~ 
                            n_ba_bases 
                          + n_ir_bases 
                          + peacelines
                          + n_roadblocks
                          + PCTCATH 
                          + PCTCATH:PCTCATH2
                          + LTUNEMP 
                          + POPDENS
                          + ulster_kills
                          + army_kills
                          + ira_civ_sel
                          + ira_mil_sel
                          + ira_mil_ind
                          | LTUNEMP 
                          #+ PCTURBN
                          + POPDENS
                          + DERRY
                          + BELFAST
                           , data = soadata@data
                           , dist = 'negbin')
summary(ira_indis_civ)

######################################################################################################
######## SELECTIVE IRA VIOLENCE AGAINST MILITARY
ira_select_mil <- zeroinfl(ira_mil_sel ~ 
                             n_ba_bases 
                           + n_ir_bases 
                           + peacelines
                           + n_roadblocks
                           + PCTCATH 
                           + PCTCATH:PCTCATH2
                           + LTUNEMP 
                           + POPDENS
                           + ulster_kills
                           + army_kills
                           + ira_civ_ind
                           + ira_civ_sel
                           + ira_mil_ind
                           | LTUNEMP 
                           #+ PCTURBN
                           + POPDENS
                           + DERRY
                           + BELFAST
                           , data = soadata@data
                           , dist = 'negbin')
summary(ira_select_mil)

######## INDISCRIMINATE IRA VIOLENCE AGAINST MILITARY
ira_indis_mil <- zeroinfl(ira_mil_ind ~ 
                            n_ba_bases 
                          + n_ir_bases 
                          + peacelines
                          + n_roadblocks
                          + PCTCATH 
                          + PCTCATH:PCTCATH2
                          + LTUNEMP 
                          + POPDENS
                          + ulster_kills
                          + army_kills
                          + ira_civ_ind
                          + ira_civ_sel
                          + ira_mil_sel
                          | LTUNEMP 
                          #+ PCTURBN
                          + POPDENS
                          + DERRY
                          + BELFAST
                          , data = soadata@data
                          , dist = 'negbin')
summary(ira_indis_mil)



stargazer(ira_select_mil, ira_select_civ, ira_indis_mil, ira_indis_civ
          , zero.component = F
          , column.labels = c('Select-Mil', 'Select-Civ', 'Indis-Mil', 'Indis-Civ')
          , covariate.labels = c('BA Bases per 1000', 'RUC Bases per 1000', 'Peacelines', 'Roadblocks per 1000', 'Pct Catholic'
                                 , 'Longterm Unemployment', 'Population Density'
                                 , 'Loyalist Violence', 'BA/RUC Violence', 'Repub Ind-Civ', 'Repub Sel-Civ'
                                 , 'Repub Sel-Mil', 'Repub Ind-Mil', 'Pct Catholic$^2$')
          , digits = 2
          , star.cutoffs = c(0.05, 0.01))
stargazer(ira_select_mil, ira_select_civ, ira_indis_mil, ira_indis_civ
          , zero.component = T
          , column.labels = c('Sel-Mil', 'Sel-Civ', 'Ind-Mil', 'Ind-Civ')
          , covariate.labels = c('Longterm Unemployment', 'Population Density', 'Derry/Belfast')
          , digits = 2
          , star.cutoffs = c(0.05, 0.01))


stargazer(ira_full, ira_select, ira_indis
          , zero.component = F
          , column.labels = c('All', 'Select', 'Indis')
          , covariate.labels = c('BA Bases per 1000', 'RUC Bases per 1000', 'Peacelines', 'Roadblocks per 1000', 'Pct Catholic'
                                 , 'Loyalist Violence', 'BA/RUC Violence', 'Repub Indis', 'Repub Select', 'Pct Catholic$^2$')
          , digits = 2
          , star.cutoffs = c(0.05, 0.01))
stargazer(ira_full, ira_select, ira_indis
          , zero.component = T
          , column.labels = c('All', 'Select', 'Indis')
          , covariate.labels = c('Longterm Unemployment', 'Pct Urbanized', 'Population Density', 'Derry/Belfast')
          , digits = 2
          , star.cutoffs = c(0.05, 0.01))
