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

deaths$tactics <- NA_character_
deaths$tactics[deaths$cntxt_m %in% c('Explosion', 'Arson', 'Rocket', 'Sniper')] <- 'explosives_indirect'
deaths$tactics[deaths$cntxt_m %in% c('Ambush', 'Gun Battle', 'Assasination', 'Riot Affray', 'Stabbing', 'Dumped', 'Beating')] <- 'smallarms_direct'

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

soadata@data$ira_kills <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('Prov', 'INLA', 'RIRA', 'NSR'),]
                                      , returnList = T), nrow)

soadata@data$ulster_kills <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('UVF', 'NSL', 'UDR', 'BA', 'RUC'),]
                                      , returnList = T), nrow)

soadata@data$army_kills <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('BA', 'RUC'),]
                                         , returnList = T), nrow)



soadata@data$ira_civ_kills <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('Prov', 'INLA', 'RIRA', 'NSR')
                                                          & deaths@data$vc_stts_s %in% c('Civilian (Civ)'),]
                                      , returnList = T), nrow)

soadata@data$ulster_civ_kills <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('UVF', 'NSL', 'UDR', 'BA', 'RUC')
                                                             & deaths@data$vc_stts_s %in% c('Civilian (Civ)'),]
                                         , returnList = T), nrow)

soadata@data$army_civ_kills <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('BA', 'RUC')
                                                           & deaths@data$vc_stts_s %in% c('Civilian (Civ)'),]
                                       , returnList = T), nrow)



soadata@data$ira_indis <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('Prov', 'INLA', 'RIRA', 'NSR')
                                                      & deaths@data$intrctn %in% c('bombing', 'shelling'),]
                                      , returnList = T), nrow)

soadata@data$ulster_indis <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('UVF', 'NSL', 'UDR', 'BA', 'RUC')
                                                         & deaths@data$intrctn %in% c('bombing', 'shelling'),]
                                         , returnList = T), nrow)

soadata@data$ira_select <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('Prov', 'INLA', 'RIRA', 'NSR')
                                                      & deaths@data$intrctn %in% c('shooting', 'beating'),]
                                      , returnList = T), nrow)

soadata@data$ulster_select <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('UVF', 'NSL', 'UDR', 'BA', 'RUC')
                                                         & deaths@data$intrctn %in% c('shooting', 'beating'),]
                                         , returnList = T), nrow)
soadata@data$ira_indis_pct <- 0
soadata@data$ira_indis_pct[soadata@data$ira_kills > 0] <- soadata@data$ira_indis[soadata@data$ira_kills > 0] / soadata@data$ira_kills[soadata@data$ira_kills > 0]

soadata@data$ulster_indis_pct <- 0
soadata@data$ulster_indis_pct[soadata@data$ulster_kills > 0] <- soadata@data$ulster_indis[soadata@data$ulster_kills > 0] / soadata@data$ulster_kills[soadata@data$ulster_kills > 0]

soadata@data$ira_select_pct <- 0
soadata@data$ira_select_pct[soadata@data$ira_kills > 0] <- soadata@data$ira_select[soadata@data$ira_kills > 0] / soadata@data$ira_kills[soadata@data$ira_kills > 0]

soadata@data$ulster_select_pct <- 0
soadata@data$ulster_select_pct[soadata@data$ulster_kills > 0] <- soadata@data$ulster_select[soadata@data$ulster_kills > 0] / soadata@data$ulster_kills[soadata@data$ulster_kills > 0]




soadata@data$ira_civ_sel <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('Prov', 'INLA', 'RIRA', 'NSR')
                                                       & deaths@data$intrctn %in% c('shooting', 'beating')
                                                       & deaths@data$vc_stts_s %in% c('Civilian (Civ)'),]
                                       , returnList = T), nrow)
soadata@data$ira_civ_ind <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('Prov', 'INLA', 'RIRA', 'NSR')
                                                        & deaths@data$intrctn %in% c('bombing', 'shelling')
                                                        & deaths@data$vc_stts_s %in% c('Civilian (Civ)'),]
                                        , returnList = T), nrow)

soadata@data$ira_mil_sel <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('Prov', 'INLA', 'RIRA', 'NSR')
                                                        & deaths@data$intrctn %in% c('shooting', 'beating')
                                                        & deaths@data$vc_stts_m %in% c('BA', 'NSL', 'Guard/Army', 'RUC', 'Uda', 'Uvf', 'Pol'),]
                                        , returnList = T), nrow)
soadata@data$ira_mil_ind <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('Prov', 'INLA', 'RIRA', 'NSR')
                                                        & deaths@data$intrctn %in% c('bombing', 'shelling')
                                                        & deaths@data$vc_stts_m %in% c('BA', 'NSL', 'Guard/Army', 'RUC', 'Uda', 'Uvf', 'Pol'),]
                                        , returnList = T), nrow)
soadata@data$ira_civ_sel_pct <- 0
soadata@data$ira_civ_sel_pct[soadata@data$ira_kills > 0] <- soadata@data$ira_civ_sel[soadata@data$ira_kills > 0] / soadata@data$ira_kills[soadata@data$ira_kills > 0]
soadata@data$ira_civ_ind_pct <- 0
soadata@data$ira_civ_ind_pct[soadata@data$ira_kills > 0] <- soadata@data$ira_civ_ind[soadata@data$ira_kills > 0] / soadata@data$ira_kills[soadata@data$ira_kills > 0]
soadata@data$ira_mil_sel_pct <- 0
soadata@data$ira_mil_sel_pct[soadata@data$ira_kills > 0] <- soadata@data$ira_mil_sel[soadata@data$ira_kills > 0] / soadata@data$ira_kills[soadata@data$ira_kills > 0]
soadata@data$ira_mil_ind_pct <- 0
soadata@data$ira_mil_ind_pct[soadata@data$ira_kills > 0] <- soadata@data$ira_mil_ind[soadata@data$ira_kills > 0] / soadata@data$ira_kills[soadata@data$ira_kills > 0]

soadata@data$ulster_civ_sel <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('UVF', 'NSL', 'UDR', 'BA', 'RUC')
                                                        & deaths@data$intrctn %in% c('shooting', 'beating')
                                                        & deaths@data$vc_stts_s %in% c('Civilian (Civ)'),]
                                        , returnList = T), nrow)
soadata@data$ulster_civ_ind <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('UVF', 'NSL', 'UDR', 'BA', 'RUC')
                                                        & deaths@data$intrctn %in% c('bombing', 'shelling')
                                                        & deaths@data$vc_stts_s %in% c('Civilian (Civ)'),]
                                        , returnList = T), nrow)

soadata@data$ulster_mil_sel <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('UVF', 'NSL', 'UDR', 'BA', 'RUC')
                                                        & deaths@data$intrctn %in% c('shooting', 'beating')
                                                        & deaths@data$vc_stts_m %in% c('Inla', 'Prov.', 'RIRA'),]
                                        , returnList = T), nrow)
soadata@data$ulster_mil_ind <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('UVF', 'NSL', 'UDR', 'BA', 'RUC')
                                                        & deaths@data$intrctn %in% c('bombing', 'shelling')
                                                        & deaths@data$vc_stts_m %in% c('Inla', 'Prov.', 'RIRA'),]
                                        , returnList = T), nrow)
soadata@data$ulster_civ_sel_pct <- 0
soadata@data$ulster_civ_sel_pct[soadata@data$ulster_kills > 0] <- soadata@data$ulster_civ_sel[soadata@data$ulster_kills > 0] / soadata@data$ulster_kills[soadata@data$ulster_kills > 0]
soadata@data$ulster_civ_ind_pct <- 0
soadata@data$ulster_civ_ind_pct[soadata@data$ulster_kills > 0] <- soadata@data$ulster_civ_ind[soadata@data$ulster_kills > 0] / soadata@data$ulster_kills[soadata@data$ulster_kills > 0]
soadata@data$ulster_mil_sel_pct <- 0
soadata@data$ulster_mil_sel_pct[soadata@data$ulster_kills > 0] <- soadata@data$ulster_mil_sel[soadata@data$ulster_kills > 0] / soadata@data$ulster_kills[soadata@data$ulster_kills > 0]
soadata@data$ulster_mil_ind_pct <- 0
soadata@data$ulster_mil_ind_pct[soadata@data$ulster_kills > 0] <- soadata@data$ulster_mil_ind[soadata@data$ulster_kills > 0] / soadata@data$ulster_kills[soadata@data$ulster_kills > 0]


soadata@data$logdeaths <- log(soadata@data$deaths + 1)

soadata@data$bigtown <- ifelse(soadata@data$PCTURBN > 0, 1, 0)

soadata@data$PCTCATH <- soadata@data$PCTCATH/100
soadata@data$PCTCATHSQ <- soadata@data$PCTCATH^2
#soadata@data$PCTCATHSQ[soadata@data$PCTCATH <= 31] <- soadata@data$PCTCATHSQ[soadata@data$PCTCATH <= 31]/1000
#soadata@data$PCTCATHSQ[soadata@data$PCTCATH > 31] <- soadata@data$PCTCATHSQ[soadata@data$PCTCATH > 31]/10000


listw <- nb2listw(poly2nb(soadata))
listw_urb <- nb2listw(poly2nb(soadata[soadata$BELFAST == 1 | soadata$DERRY == 1, ]))
listw_rur <- nb2listw(poly2nb(soadata[soadata$BELFAST == 0 & soadata$DERRY == 0, ]))

######## SELECTIVE IRA VIOLENCE
ira_select <- zeroinfl(ira_select ~ 
                       n_ba_bases + n_ir_bases + peacelines + n_roadblocks
                       + PCTCATH 
                       + PCTCATHSQ
                       + ulster_kills
                       + ira_indis
                       | LTUNEMP 
                       + PCTURBN
                       + POPDENS 
                       #+ BORDER
                       + BELFAST
                       + DERRY
                       , data = soadata@data)
summary(ira_select)

ira_select_urb <- zeroinfl(ira_select ~ 
                           n_ba_bases + n_ir_bases + peacelines
                         + PCTCATH 
                         + PCTCATHSQ
                         + ulster_kills
                         + ira_indis
                         | LTUNEMP 
                         + PCTURBN
                         + POPDENS 
                    , data = soadata@data[soadata$BELFAST == 1 | soadata$DERRY == 1, ]
                    , dist = 'negbin')
summary(ira_select_urb)

ira_select_rur <- zeroinfl(ira_select ~ 
                    n_ba_bases + n_ir_bases + n_roadblocks
                    + PCTCATH 
                    + PCTCATHSQ
                    + ulster_kills
                    + ira_indis
                    | LTUNEMP
                    + POPDENS 
                    + PCTURBN
                    #+ BORDER
                    , data = soadata@data[soadata$BELFAST == 0 & soadata$DERRY == 0, ]
                    , dist = 'negbin')
summary(ira_select_rur)
stargazer(ira_select, ira_select_urb, ira_select_rur, zero.component = F)
stargazer(ira_select, ira_select_urb, ira_select_rur, zero.component = T)

######## INDISCRIMINATE IRA VIOLENCE
ira_indis <- zeroinfl(ira_indis ~ 
                      n_ba_bases + n_ir_bases + peacelines + n_roadblocks
                      + PCTCATH 
                      + PCTCATHSQ
                      + ulster_kills
                      + ira_select
                      | LTUNEMP 
                      + PCTURBN
                      + POPDENS
                      #+ BORDER
                      + BELFAST
                      + DERRY
                      , data = soadata@data
                      , dist = 'negbin')
summary(ira_indis)

ira_indis_urb <- zeroinfl(ira_indis ~ 
                          n_ba_bases + n_ir_bases + peacelines
                          + PCTCATH 
                          + PCTCATHSQ
                          + ulster_kills
                          + ira_select
                          | LTUNEMP 
                          + PCTURBN
                          + POPDENS 
                          , data = soadata@data[soadata$BELFAST == 1 | soadata$DERRY == 1, ]
                          , dist = 'negbin')
summary(ira_indis_urb)

ira_indis_rur <- zeroinfl(ira_indis ~ 
                            n_ba_bases + n_ir_bases + n_roadblocks
                          + PCTCATH 
                          + PCTCATHSQ
                          + ulster_kills
                          + ira_select
                          | LTUNEMP
                          #+ BORDER
                          + POPDENS 
                          + PCTURBN
                          , data = soadata@data[soadata$BELFAST == 0 & soadata$DERRY == 0, ]
                          , dist = 'negbin')
summary(ira_indis_rur)
stargazer(ira_indis, ira_indis_urb, ira_indis_rur, zero.component = F)
stargazer(ira_indis, ira_indis_urb, ira_indis_rur, zero.component = T)


######################################################################################################
######## SELECTIVE IRA VIOLENCE AGAINST CIVILIANS
ira_select_civ <- zeroinfl(ira_civ_sel ~ 
                             n_ba_bases + n_ir_bases + peacelines + n_roadblocks
                           + PCTCATH 
                           + PCTCATHSQ
                           + ulster_kills
                           + ira_civ_ind
                           + ira_mil_sel
                           + ira_mil_ind
                           | LTUNEMP 
                           + PCTURBN
                           + POPDENS
                           #+ BORDER
                           + BELFAST
                           + DERRY
                           , data = soadata@data
                           , dist = 'negbin')
summary(ira_select_civ)


ira_select_urb_civ <- zeroinfl(ira_civ_sel ~
                                 n_ba_bases + n_ir_bases + peacelines
                               + PCTCATH 
                               + PCTCATHSQ
                               + ulster_kills
                               + ira_civ_ind
                               + ira_mil_sel
                               + ira_mil_ind
                               | LTUNEMP 
                               + PCTURBN
                               + POPDENS 
                               , data = soadata@data[soadata$BELFAST == 1 | soadata$DERRY == 1, ]
                               , dist = 'negbin')
summary(ira_select_urb_civ)

ira_select_rur_civ <- zeroinfl(ira_civ_sel ~ 
                                 n_ba_bases + n_ir_bases + n_roadblocks
                               + PCTCATH 
                               + PCTCATHSQ
                               + ulster_kills
                               + ira_civ_ind
                               + ira_mil_sel
                               + ira_mil_ind
                               | LTUNEMP
                               #+ BORDER
                               + POPDENS 
                               + PCTURBN
                               , data = soadata@data[soadata$BELFAST == 0 & soadata$DERRY == 0, ]
                               , dist = 'negbin')
summary(ira_select_rur_civ)

stargazer(ira_select_civ, ira_select_urb_civ, ira_select_rur_civ, zero.component = F)
stargazer(ira_select_civ, ira_select_urb_civ, ira_select_rur_civ, zero.component = T)



######## INDISCRIMINATE IRA VIOLENCE AGAINST CIVILIANS
ira_indis_civ <- zeroinfl(ira_civ_ind ~ 
                             n_ba_bases + n_ir_bases + peacelines + n_roadblocks
                           + PCTCATH 
                           + PCTCATHSQ
                           + ulster_kills
                           + ira_civ_sel
                           + ira_mil_sel
                           + ira_mil_ind
                           | LTUNEMP 
                           + PCTURBN
                           + POPDENS
                           #+ BORDER
                           + BELFAST
                           + DERRY
                           , data = soadata@data
                           , dist = 'negbin')
summary(ira_indis_civ)


ira_indis_urb_civ <- zeroinfl(ira_civ_ind ~
                                 n_ba_bases + n_ir_bases + peacelines
                               + PCTCATH 
                               + PCTCATHSQ
                               + ulster_kills
                              + ira_civ_sel
                              + ira_mil_sel
                              + ira_mil_ind
                               | LTUNEMP 
                               + PCTURBN
                               + POPDENS 
                               , data = soadata@data[soadata$BELFAST == 1 | soadata$DERRY == 1, ]
                               , dist = 'negbin')
summary(ira_indis_urb_civ)

ira_indis_rur_civ <- zeroinfl(ira_civ_ind ~ 
                                 n_ba_bases + n_ir_bases + n_roadblocks
                               + PCTCATH 
                               + PCTCATHSQ
                               + ulster_kills
                              + ira_civ_sel
                              + ira_mil_sel
                              + ira_mil_ind
                               | LTUNEMP
                              # + BORDER
                               + POPDENS 
                               + PCTURBN
                               , data = soadata@data[soadata$BELFAST == 0 & soadata$DERRY == 0, ]
                               , dist = 'negbin')
summary(ira_indis_rur_civ)

stargazer(ira_indis_civ, ira_indis_urb_civ, ira_indis_rur_civ, zero.component = F)
stargazer(ira_indis_civ, ira_indis_urb_civ, ira_indis_rur_civ, zero.component = T)

######################################################################################################
######## SELECTIVE IRA VIOLENCE AGAINST MILITARY
ira_select_mil <- zeroinfl(ira_mil_sel ~ 
                             n_ba_bases + n_ir_bases + peacelines + n_roadblocks
                           + PCTCATH 
                           + PCTCATHSQ
                           + ulster_kills
                           + ira_mil_ind
                           + ira_civ_ind
                           + ira_civ_sel
                           | LTUNEMP 
                           + PCTURBN
                           + POPDENS
                          # + BORDER
                           + BELFAST
                           + DERRY
                           , data = soadata@data
                           , dist = 'negbin')
summary(ira_select_mil)


ira_select_urb_mil <- zeroinfl(ira_mil_sel ~
                                 n_ba_bases + n_ir_bases + peacelines
                               + PCTCATH 
                               + PCTCATHSQ
                               + ulster_kills
                               + ira_mil_ind
                               + ira_civ_ind
                               + ira_civ_sel
                               | LTUNEMP 
                               + PCTURBN
                               + POPDENS 
                               , data = soadata@data[soadata$BELFAST == 1 | soadata$DERRY == 1, ]
                               , dist = 'negbin')
summary(ira_select_urb_mil)

ira_select_rur_mil <- zeroinfl(ira_mil_sel ~ 
                                 n_ba_bases + n_ir_bases + n_roadblocks
                               + PCTCATH 
                               + PCTCATHSQ
                               + ulster_kills
                               + ira_mil_ind
                               + ira_civ_ind
                               + ira_civ_sel
                               | LTUNEMP
                               #+ BORDER
                               + POPDENS 
                               + PCTURBN
                               , data = soadata@data[soadata$BELFAST == 0 & soadata$DERRY == 0, ]
                               , dist = 'negbin')
summary(ira_select_rur_mil)

stargazer(ira_select_mil, ira_select_urb_mil, ira_select_rur_mil, zero.component = F)
stargazer(ira_select_mil, ira_select_urb_mil, ira_select_rur_mil, zero.component = T)



######## INDISCRIMINATE IRA VIOLENCE AGAINST MILITARY
ira_indis_mil <- zeroinfl(ira_mil_ind ~ 
                            n_ba_bases + n_ir_bases + peacelines + n_roadblocks
                          + PCTCATH 
                          + PCTCATHSQ
                          + ulster_kills
                          + ira_mil_sel
                          + ira_civ_sel
                          + ira_civ_ind
                          | LTUNEMP 
                          + PCTURBN
                          + POPDENS
                          #+ BORDER
                          + BELFAST
                          + DERRY
                          , data = soadata@data
                          , dist = 'negbin')
summary(ira_indis_mil)


ira_indis_urb_mil <- zeroinfl(ira_mil_ind ~
                                n_ba_bases + n_ir_bases + peacelines
                              + PCTCATH 
                              + PCTCATHSQ
                              + ulster_kills
                              + ira_mil_sel
                              + ira_civ_sel
                              + ira_civ_ind
                              | LTUNEMP 
                              + PCTURBN
                              + POPDENS 
                              , data = soadata@data[soadata$BELFAST == 1 | soadata$DERRY == 1, ]
                              , dist = 'negbin')
summary(ira_indis_urb_mil)

ira_indis_rur_mil <- zeroinfl(ira_mil_ind ~ 
                                n_ba_bases + n_ir_bases + n_roadblocks
                              + PCTCATH 
                              + PCTCATHSQ
                              + ulster_kills
                              + ira_mil_sel
                              + ira_civ_sel
                              + ira_civ_ind
                              | LTUNEMP
                              #+ BORDER
                              + POPDENS 
                              + PCTURBN
                              , data = soadata@data[soadata$BELFAST == 0 & soadata$DERRY == 0, ]
                              , dist = 'negbin')
summary(ira_indis_rur_mil)

stargazer(ira_indis_mil, ira_indis_urb_mil, ira_indis_rur_mil, zero.component = F)
stargazer(ira_indis_mil, ira_indis_urb_mil, ira_indis_rur_mil, zero.component = T)