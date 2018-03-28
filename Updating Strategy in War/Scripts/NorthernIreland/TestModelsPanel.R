rm(list = ls())
setwd('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\Census Data')
library(rgdal)
library(foreign)
library(data.table)
library(raster)
library(rgeos)
library(maptools)
library(spdep)
library(spatcounts)
library(pscl)
library(splm)


soadata <- readOGR('C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
                   , 'SOA_agg_2001census')

deaths <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
                  , layer = 'geodeaths')
deaths@data$date <- as.Date(deaths@data$date, format = '%m/%d/%Y')

bases <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
                 , layer = 'geobases')

peacelines <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Prospectus\\Data\\NorthernIreland\\IrelandGIS'
                      , layer = 'peacelines')
peacelines <- spTransform(peacelines, CRS(proj4string(soadata)))

soadata@data$peacelines <- sapply(over(soadata, peacelines, returnList = T), nrow)
soadata@data$peacelines <- ifelse(soadata@data$peacelines > 0, 1, 0)

soadata@data$n_ba_bases <- sapply(over(soadata, bases[bases$Type2 == 'British Army',], returnList = T), nrow)
soadata@data$n_ba_bases <- soadata@data$n_ba_bases / soadata@data$TOTPOP * 1000
soadata@data$n_ir_bases <- sapply(over(soadata, bases[bases$Type2 == 'Irish Security',], returnList = T), nrow)
soadata@data$n_ir_bases <- soadata@data$n_ir_bases / soadata@data$TOTPOP * 1000

deaths@data$SOALAB <- over(deaths, soadata[, 'SOALAB'])$SOALAB


soadata@data$logdeaths <- log(soadata@data$deaths + 1)

soadata@data$bigtown <- ifelse(soadata@data$PCTURBN > 0, 1, 0)

soadata@data$PCTCATH <- soadata@data$PCTCATH/100
soadata@data$PCTCATHSQ <- soadata@data$PCTCATH^2
#soadata@data$PCTCATHSQ[soadata@data$PCTCATH <= 31] <- soadata@data$PCTCATHSQ[soadata@data$PCTCATH <= 31]/1000
#soadata@data$PCTCATHSQ[soadata@data$PCTCATH > 31] <- soadata@data$PCTCATHSQ[soadata@data$PCTCATH > 31]/10000


listw <- nb2listw(poly2nb(soadata))
matw <- listw2mat(listw)

################ TEST SPATIAL PANEL: 1969-1980 BY MONTH
start_date <- as.Date('1969-07-01', format = '%Y-%m-%d')
end_date <- as.Date('1975-01-01', format = '%Y-%m-%d')
dates <- seq.Date(start_date, end_date, by = 'month')

soa_panel <- data.frame(
  'ID' = sort(rep(as.character(row.names(soadata)), length(dates)))
  , 'MONTH' = rep(dates, nrow(soadata@data))
  , stringsAsFactors = F)

soadata@data$ID <- row.names(soadata)
soa_panel <- merge(soa_panel, soadata@data, by = 'ID', all.x = T)
soa_panel$SOALAB <- as.character(soa_panel$SOALAB)

soa_panel$deaths <- 0
soa_panel$cath_deaths <- 0
soa_panel$prot_deaths <- 0
soa_panel$security_deaths <- 0
soa_panel$ira_kills <- 0
soa_panel$ulster_kills <- 0
soa_panel$security_kills <- 0
soa_panel$ira_indis <- 0
soa_panel$ulster_indis <- 0
soa_panel$ira_select <- 0
soa_panel$ulster_select <- 0


for(i in 1:length(dates[-length(dates)])){
  
  date1 <- dates[i]
  date2 <- dates[i+1]
  
  thismonth_d <- deaths@data[deaths@data$date > date1
                             & deaths@data$date < date2
                             & !is.na(deaths@data$SOALAB), ]
  thismonth_d$SOALAB <- as.character(thismonth_d$SOALAB)
  
  for(this_soa in unique(thismonth_d$SOALAB)){
    
    ## Death counts
    soa_panel$deaths[soa_panel$MONTH >= date1
                     & soa_panel$MONTH < date2
                     & soa_panel$SOALAB == this_soa] <- nrow(thismonth_d[thismonth_d$SOALAB == this_soa, ])
    
    soa_panel$cath_deaths[soa_panel$MONTH >= date1
                          & soa_panel$MONTH < date2
                          & soa_panel$SOALAB == this_soa] <- nrow(thismonth_d[thismonth_d$SOALAB == this_soa
                                                                              & thismonth_d$vc_rlg_s == 'Catholic', ])
    
    soa_panel$prot_deaths[soa_panel$MONTH >= date1
                          & soa_panel$MONTH < date2
                          & soa_panel$SOALAB == this_soa] <- nrow(thismonth_d[thismonth_d$SOALAB == this_soa
                                                                              & thismonth_d$vc_rlg_s == 'Protestant', ])
    soa_panel$security_deaths[soa_panel$MONTH >= date1
                              & soa_panel$MONTH < date2
                              & soa_panel$SOALAB == this_soa] <- nrow(thismonth_d[thismonth_d$SOALAB == this_soa
                                                                                  & (thismonth_d$vc_stts_m == 'RUC' 
                                                                                     | thismonth_d$vc_stts_m == 'BA'), ])
    
    ## Killings by affiliation
    soa_panel$ira_kills[soa_panel$MONTH >= date1
                        & soa_panel$MONTH < date2
                        & soa_panel$SOALAB == this_soa] <- nrow(thismonth_d[thismonth_d$SOALAB == this_soa
                                                                            & (thismonth_d$prp_stts_m == 'Prov'
                                                                               | thismonth_d$prp_stts_m == 'INLA'
                                                                               | thismonth_d$prp_stts_m == 'RIRA'
                                                                               | thismonth_d$prp_stts_m == 'NSR'), ])
    soa_panel$ulster_kills[soa_panel$MONTH >= date1
                           & soa_panel$MONTH < date2
                           & soa_panel$SOALAB == this_soa] <- nrow(thismonth_d[thismonth_d$SOALAB == this_soa
                                                                               & (thismonth_d$prp_stts_m == 'UVF'
                                                                                  | thismonth_d$prp_stts_m == 'NSL'
                                                                                  | thismonth_d$prp_stts_m == 'UDR'), ])
    
    soa_panel$security_kills[soa_panel$MONTH >= date1
                             & soa_panel$MONTH < date2
                             & soa_panel$SOALAB == this_soa] <- nrow(thismonth_d[thismonth_d$SOALAB == this_soa
                                                                                 & (thismonth_d$prp_stts_m == 'BA'
                                                                                    | thismonth_d$prp_stts_m == 'RUC'), ])
    
    ## Killings by affiliation and type
    soa_panel$ira_indis[soa_panel$MONTH >= date1
                        & soa_panel$MONTH < date2
                        & soa_panel$SOALAB == this_soa] <- nrow(thismonth_d[thismonth_d$SOALAB == this_soa
                                                                            & (thismonth_d$prp_stts_m == 'Prov'
                                                                               | thismonth_d$prp_stts_m == 'INLA'
                                                                               | thismonth_d$prp_stts_m == 'RIRA'
                                                                               | thismonth_d$prp_stts_m == 'NSR')
                                                                            & (thismonth_d$intrctn == 'bombing'
                                                                               | thismonth_d$intrctn == 'shelling'), ])
    
    soa_panel$ulster_indis[soa_panel$MONTH >= date1
                           & soa_panel$MONTH < date2
                           & soa_panel$SOALAB == this_soa] <- nrow(thismonth_d[thismonth_d$SOALAB == this_soa
                                                                               & (thismonth_d$prp_stts_m == 'UVF'
                                                                                  | thismonth_d$prp_stts_m == 'NSL'
                                                                                  | thismonth_d$prp_stts_m == 'UDR')
                                                                               & (thismonth_d$intrctn == 'bombing'
                                                                                  | thismonth_d$intrctn == 'shelling'), ])
    
    soa_panel$ira_select[soa_panel$MONTH >= date1
                         & soa_panel$MONTH < date2
                         & soa_panel$SOALAB == this_soa] <- nrow(thismonth_d[thismonth_d$SOALAB == this_soa
                                                                             & (thismonth_d$prp_stts_m == 'Prov'
                                                                              | thismonth_d$prp_stts_m == 'INLA'
                                                                              | thismonth_d$prp_stts_m == 'RIRA'
                                                                              | thismonth_d$prp_stts_m == 'NSR')
                                                                             & (thismonth_d$intrctn == 'shooting'
                                                                                | thismonth_d$intrctn == 'beating'), ])
    soa_panel$ulster_select[soa_panel$MONTH >= date1
                            & soa_panel$MONTH < date2
                            & soa_panel$SOALAB == this_soa] <- nrow(thismonth_d[thismonth_d$SOALAB == this_soa
                                                                                & (thismonth_d$prp_stts_m == 'UVF'
                                                                                   | thismonth_d$prp_stts_m == 'NSL'
                                                                                   | thismonth_d$prp_stts_m == 'UDR')
                                                                                & (thismonth_d$intrctn == 'shooting'
                                                                                   | thismonth_d$intrctn == 'beating'), ])
    
  }
  
}





soa_panel <- soa_panel[order(soa_panel$ID, soa_panel$MONTH), ]


testmod <- spgm(ira_kills ~ POPDENS + PCTCATH + ulster_kills
                , data = soa_panel
                , listw = matw
                , lag = T)







sapply(soa_panel$SOALAB[soa_panel$MONTH == start_date] %in% thismonth_d@data$SOALAB)

soadata@data$deaths <- sapply(over(soadata, thismonth_d, returnList = T), nrow)
soadata@data$cath_deaths <- sapply(over(soadata, thismonth_d[thismonth_d@data$vc_rlg_s == 'Catholic',], returnList = T), nrow)
soadata@data$prot_deaths <- sapply(over(soadata, thismonth_d[thismonth_d@data$vc_rlg_s == 'Protestant',], returnList = T), nrow)

soadata@data$ira_kills <- sapply(over(soadata, thismonth_d[thismonth_d@data$prp_stts_m %in% c('Prov', 'INLA', 'RIRA', 'NSR'),]
                                      , returnList = T), nrow)

soadata@data$ulster_kills <- sapply(over(soadata, thismonth_d[thismonth_d@data$prp_stts_m %in% c('UVF', 'NSL', 'UDR'),]
                                         , returnList = T), nrow)

soadata@data$army_kills <- sapply(over(soadata, thismonth_d[thismonth_d@data$prp_stts_m %in% c('BA', 'RUC'),]
                                       , returnList = T), nrow)


soadata@data$ira_indis <- sapply(over(soadata, thismonth_d[thismonth_d@data$prp_stts_m %in% c('Prov', 'INLA', 'RIRA', 'NSR')
                                                      & thismonth_d@data$intrctn %in% c('bombing', 'shelling')
                                                      & thismonth_d@data$vc_stts_m != 'BA',]
                                      , returnList = T), nrow)

soadata@data$ulster_indis <- sapply(over(soadata, thismonth_d[thismonth_d@data$prp_stts_m %in% c('UVF', 'NSL', 'UDR')
                                                         & thismonth_d@data$intrctn %in% c('bombing', 'shelling'),]
                                         , returnList = T), nrow)

soadata@data$ira_select <- sapply(over(soadata, thismonth_d[thismonth_d@data$prp_stts_m %in% c('Prov', 'INLA', 'RIRA', 'NSR')
                                                       & thismonth_d@data$intrctn %in% c('shooting', 'beating'),]
                                       , returnList = T), nrow)

soadata@data$ulster_select <- sapply(over(soadata, thismonth_d[thismonth_d@data$prp_stts_m %in% c('UVF', 'NSL', 'UDR')
                                                          & thismonth_d@data$intrctn %in% c('shooting', 'beating'),]
                                          , returnList = T), nrow)


soadata@data$ira_indis_pct <- 0
soadata@data$ira_indis_pct[soadata@data$ira_kills > 0] <- soadata@data$ira_indis[soadata@data$ira_kills > 0] / soadata@data$ira_kills[soadata@data$ira_kills > 0]

soadata@data$ulster_indis_pct <- 0
soadata@data$ulster_indis_pct[soadata@data$ulster_kills > 0] <- soadata@data$ulster_indis[soadata@data$ulster_kills > 0] / soadata@data$ulster_kills[soadata@data$ulster_kills > 0]

soadata@data$ira_select_pct <- 0
soadata@data$ira_select_pct[soadata@data$ira_kills > 0] <- soadata@data$ira_select[soadata@data$ira_kills > 0] / soadata@data$ira_kills[soadata@data$ira_kills > 0]

soadata@data$ulster_select_pct <- 0
soadata@data$ulster_select_pct[soadata@data$ulster_kills > 0] <- soadata@data$ulster_select[soadata@data$ulster_kills > 0] / soadata@data$ulster_kills[soadata@data$ulster_kills > 0]







over(thismonth_d, soadata)


