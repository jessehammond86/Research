rm(list = ls())
#setwd('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\Census Data')
setwd('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/Census Data')
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
library(sampleSelection)
ni_crs <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ')


# soadata <- readOGR('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
#          , 'SOA_agg_2001census')
soadata <- readOGR('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
                   , 'SOA_agg_2001census')
soadata@proj4string <- ni_crs
soadata <- spTransform(soadata, ni_crs)
# deaths <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
#          , layer = 'geodeaths')
deaths <- readOGR(dsn = '/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
                  , layer = 'geodeaths')
deaths@proj4string <- ni_crs
deaths <- spTransform(deaths, ni_crs)
deaths@data$date <- as.Date(deaths@data$date, format = '%m/%d/%Y')

# bases <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
#          , layer = 'geobases')
bases <- readOGR(dsn = '/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
                 , layer = 'geobases')

bases@proj4string <- ni_crs
bases <- spTransform(bases, ni_crs)

# peacelines <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
#                  , layer = 'peacelines')
peacelines <- readOGR(dsn = '/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
                      , layer = 'peacelines')
peacelines <- spTransform(peacelines, ni_crs)

soadata@data$peacelines <- sapply(over(soadata, peacelines, returnList = T), nrow)
soadata@data$peacelines <- ifelse(soadata@data$peacelines > 0, 1, 0)

soadata@data$n_ba_bases <- sapply(over(soadata, bases[bases$Type2 == 'British Army',], returnList = T), nrow)
soadata@data$n_ba_bases <- soadata@data$n_ba_bases / soadata@data$TOTPOP * 1000
soadata@data$n_ir_bases <- sapply(over(soadata, bases[bases$Type2 == 'Irish Security',], returnList = T), nrow)
soadata@data$n_ir_bases <- soadata@data$n_ir_bases / soadata@data$TOTPOP * 1000
soadata@data$n_roadblocks <- sapply(over(soadata, bases[bases$Type2 %in% c('Checkpoint', 'Roadblock'),], returnList = T), nrow)
soadata@data$n_roadblocks <- soadata@data$n_roadblocks / soadata@data$TOTPOP * 1000

soadata@data$bases <- sapply(over(soadata, bases[bases$Type2 %in% c('Checkpoint', 'Roadblock', 'British Army', 'Irish Security'),], returnList = T), nrow)
soadata@data$d_bases <- soadata@data$bases / soadata@data$TOTPOP * 1000


soadata@data$deaths <- sapply(over(soadata, deaths, returnList = T), nrow)
soadata@data$cath_deaths <- sapply(over(soadata, deaths[deaths@data$vc_rlg_s == 'Catholic',], returnList = T), nrow)
soadata@data$prot_deaths <- sapply(over(soadata, deaths[deaths@data$vc_rlg_s == 'Protestant',], returnList = T), nrow)

soadata@data$ira_kills <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(7,8,9,10,14,16,17,18,21,27,24)], ]
                                      , returnList = T), nrow)

soadata@data$ulster_kills <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(11,12,13,22,23,28,29,30,31,32)], ]
                                      , returnList = T), nrow)

soadata@data$army_kills <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(1,2,25,26)], ]
                                         , returnList = T), nrow)



soadata@data$ira_civ_kills <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(7,8,9,10,14,16,17,18,21,27,24)]
                                                          & deaths@data$vc_stts_s %in% levels(deaths$vc_stts_s)[c(4,5)],]
                                      , returnList = T), nrow)

soadata@data$ulster_civ_kills <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(11,12,13,22,23,28,29,30,31,32)]
                                                             & deaths@data$vc_stts_s %in% levels(deaths$vc_stts_s)[c(4,5)],]
                                         , returnList = T), nrow)

soadata@data$army_civ_kills <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(1,2,25,26)]
                                                           & deaths@data$vc_stts_s %in% levels(deaths$vc_stts_s)[c(4,5)],]
                                       , returnList = T), nrow)



soadata@data$ira_indis <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(7,8,9,10,14,16,17,18,21,27,24)]
                                                      & deaths@data$intrctn %in% c('bombing', 'shelling'),]
                                      , returnList = T), nrow)

soadata@data$ulster_indis <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(11,12,13,22,23,28,29,30,31,32)]
                                                         & deaths@data$intrctn %in% c('bombing', 'shelling'),]
                                         , returnList = T), nrow)

soadata@data$ira_select <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(7,8,9,10,14,16,17,18,21,27,24)]
                                                      & deaths@data$intrctn %in% c('shooting', 'beating'),]
                                      , returnList = T), nrow)

soadata@data$ira_select <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(8)]
                                                       & deaths@data$intrctn %in% c('shooting', 'beating'),]
                                       , returnList = T), nrow)

soadata@data$ulster_select <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(11,12,13,22,23,28,29,30,31,32)]
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




soadata@data$ira_civ_sel <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(7,8,9,10,14,16,17,18,21,27,24)]
                                                       & deaths@data$intrctn %in% c('shooting', 'beating')
                                                       & deaths@data$vc_stts_s %in% levels(deaths$vc_stts_s)[c(4,5)],]
                                       , returnList = T), nrow)
soadata@data$ira_civ_ind <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(7,8,9,10,14,16,17,18,21,27,24)]
                                                        & deaths@data$intrctn %in% c('bombing', 'shelling')
                                                        & deaths@data$vc_stts_s %in% levels(deaths$vc_stts_s)[c(4,5)],]
                                        , returnList = T), nrow)

soadata@data$ira_mil_sel <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(7,8,9,10,14,16,17,18,21,27,24)]
                                                        & deaths@data$intrctn %in% c('shooting', 'beating')
                                                        & deaths@data$vc_stts_s %in% levels(deaths$vc_stts_s)[c(1,2,3,6,10,11,12,13,14,15,23,24,29,31,32,33,34,35,37,38,39)], ]
                                        , returnList = T), nrow)
soadata@data$ira_mil_ind <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$prp_stts_s)[c(7,8,9,10,14,16,17,18,21,27,24)]
                                                        & deaths@data$intrctn %in% c('bombing', 'shelling')
                                                        & deaths@data$vc_stts_s %in% levels(deaths$vc_stts_s)[c(1,2,3,6,10,11,12,13,14,15,23,24,29,31,32,33,34,35,37,38,39)], ]
                                        , returnList = T), nrow)
soadata@data$ira_civ_sel_pct <- 0
soadata@data$ira_civ_sel_pct[soadata@data$ira_kills > 0] <- soadata@data$ira_civ_sel[soadata@data$ira_kills > 0] / soadata@data$ira_kills[soadata@data$ira_kills > 0]
soadata@data$ira_civ_ind_pct <- 0
soadata@data$ira_civ_ind_pct[soadata@data$ira_kills > 0] <- soadata@data$ira_civ_ind[soadata@data$ira_kills > 0] / soadata@data$ira_kills[soadata@data$ira_kills > 0]
soadata@data$ira_mil_sel_pct <- 0
soadata@data$ira_mil_sel_pct[soadata@data$ira_kills > 0] <- soadata@data$ira_mil_sel[soadata@data$ira_kills > 0] / soadata@data$ira_kills[soadata@data$ira_kills > 0]
soadata@data$ira_mil_ind_pct <- 0
soadata@data$ira_mil_ind_pct[soadata@data$ira_kills > 0] <- soadata@data$ira_mil_ind[soadata@data$ira_kills > 0] / soadata@data$ira_kills[soadata@data$ira_kills > 0]

soadata@data$ulster_civ_sel <- sapply(over(soadata, deaths[deaths@data$prp_stts_s %in% levels(deaths$vc_stts_s)[c(1)],]
                                                        & deaths@data$intrctn %in% c('shooting', 'beating')
                                                        & deaths@data$vc_stts_s %in% levels(deaths$vc_stts_s)[c(4,5)],]
                                        , returnList = T), nrow)
soadata@data$ulster_civ_ind <- sapply(over(soadata, deaths[deaths@data$prp_stts_m %in% c('UVF', 'NSL', 'UDR', 'BA', 'RUC')
                                                        & deaths@data$intrctn %in% c('bombing', 'shelling')
                                                        & deaths@data$vc_stts_s %in% levels(deaths$vc_stts_s)[c(4,5)],]
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

soadata@data$bigtown <- ifelse(soadata@data$BELFAST > 0 | soadata@data$DERRY > 0, 1, 0)

soadata@data$PCTCATH <- soadata@data$PCTCATH/100
soadata@data$PCTCATH2 <- soadata@data$PCTCATH
soadata@data$PCTCATHSQ <- soadata@data$PCTCATH^2
soadata@data$violence <- ifelse(soadata@data$ira_kills > 0, 1, 0)
#soadata@data$PCTCATHSQ[soadata@data$PCTCATH <= 31] <- soadata@data$PCTCATHSQ[soadata@data$PCTCATH <= 31]/1000
#soadata@data$PCTCATHSQ[soadata@data$PCTCATH > 31] <- soadata@data$PCTCATHSQ[soadata@data$PCTCATH > 31]/10000


# listw <- nb2listw(poly2nb(soadata))
# listw_urb <- nb2listw(poly2nb(soadata[soadata$BELFAST == 1 | soadata$DERRY == 1, ]))
# listw_rur <- nb2listw(poly2nb(soadata[soadata$BELFAST == 0 & soadata$DERRY == 0, ]))

######## SELECTIVE IRA VIOLENCE
## Greene( 2003 ): example 22.8, page 786
data( Mroz87 )
Mroz87$kids <- ( Mroz87$kids5 + Mroz87$kids618 > 0 )
# Two-step estimation
summary( heckit( lfp ~ age + I( age^2 ) + faminc + kids + educ,
                 wage ~ exper + I( exper^2 ) + educ + city, Mroz87 ) )
# ML estimation
summary( selection( lfp ~ age + I( age^2 ) + faminc + kids + educ,
                    wage ~ exper + I( exper^2 ) + educ + city, Mroz87 ) )

select_eq <- violence ~ LTUNEMP + PCTURBN + POPDENS + BELFAST + DERRY + PCTCATH
outcome_eq <- ira_mil_sel_pct ~ LTUNEMP + PCTURBN + POPDENS + BELFAST + DERRY + PCTCATH + I(PCTCATH^2) + 
  d_bases + peacelines + ulster_kills + army_kills + ira_kills

outcome_eq <- ira_mil_sel_pct ~ PCTCATH + I(PCTCATH^2) + 
  d_bases + peacelines + ulster_kills + army_kills + ira_kills


ira_select <- selection(select_eq, outcome_eq, data = soadata@data, print.level = 1, method = 'ML')
#ira_select <- heckit(selection = select_eq, outcome = outcome_eq, data = soadata@data)
summary(ira_select)

#
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
                       + bigtown
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
                      | LTUNEMP 
                      + PCTURBN
                      + POPDENS 
                      + bigtown
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
                           + ulster_kills
                           + army_kills
                           + ira_civ_ind
                           + ira_mil_sel
                           + ira_mil_ind
                           | LTUNEMP 
                           + PCTURBN
                           + POPDENS
                           + bigtown
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
                          + ulster_kills
                          + army_kills
                          + ira_civ_sel
                          + ira_mil_sel
                          + ira_mil_ind
                          | LTUNEMP 
                          + PCTURBN
                          + POPDENS
                          + bigtown
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
                           + ulster_kills
                           + army_kills
                           + ira_civ_ind
                           + ira_civ_sel
                           + ira_mil_ind
                           | LTUNEMP 
                           + PCTURBN
                           + POPDENS
                           + bigtown
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
                          + ulster_kills
                          + army_kills
                          + ira_civ_ind
                          + ira_civ_sel
                          + ira_mil_sel
                          | LTUNEMP 
                          + PCTURBN
                          + POPDENS
                          + bigtown
                          , data = soadata@data
                          , dist = 'negbin')
summary(ira_indis_mil)



stargazer(ira_select_mil, ira_select_civ, ira_indis_mil, ira_indis_civ
          , zero.component = F
          , column.labels = c('All', 'Select', 'Indis', 'Sel-Mil', 'Sel-Civ', 'Ind-Mil', 'Ind-Civ')
          , covariate.labels = c('BA Bases per 1000', 'RUC Bases per 1000', 'Peacelines', 'Roadblocks per 1000', 'Pct Catholic'
                                 , 'Loyalist Violence', 'BA/RUC Violence', 'Repub Ind-Civ', 'Repub Sel-Civ'
                                 , 'Repub Sel-Mil', 'Repub Ind-Mil', 'Pct Catholic$^2$')
          , digits = 2
          , star.cutoffs = c(0.05, 0.01))
stargazer(ira_select_mil, ira_select_civ, ira_indis_mil, ira_indis_civ
          , zero.component = T
          , column.labels = c('All', 'Select', 'Indis', 'Sel-Mil', 'Sel-Civ', 'Ind-Mil', 'Ind-Civ')
          , covariate.labels = c('Longterm Unemployment', 'Pct Urbanized', 'Population Density', 'Derry/Belfast')
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
