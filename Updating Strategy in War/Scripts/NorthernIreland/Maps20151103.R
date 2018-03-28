rm(list = ls())
# setwd('/Users/jesse/Dropbox/Dissertation/Data/NorthernIreland')
setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland')
# setwd('/media/jesse/Files/Dropbox/Dissertation/Data/NorthernIreland')
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
library(xts)
library(forecast)
library(ggmap)

## Read in data
deaths <- fread('GeoDeaths.csv')


#################################################################
##
## CHANGE SOME VARIABLES
##
######
deaths[, num_deaths := .N, by = list(longitd, latitud)]

## Aggregate deaths by location
deaths_plot <- deaths[!duplicated(deaths[, list(longitd, latitud)])]

## Cut events not in NIRELAND
deaths <- deaths[(longitd > -8 & longitd < -5.5) 
                 & (latitud > 54 & latitud < 55)]

## Cut events not in BELFAST
belfast_deaths <- deaths[(longitd > -6.1 & longitd < -5.8) 
                 & (latitud > 54.54 & latitud < 54.66)]

#################################################################
##
## MAKE SOME MAPS
##
######

nireland <- ggmap(get_map('dungannon, northern ireland', zoom = 8, color = 'bw')
                  , maptype = 'terrain'
                  , extent = 'device', legend = 'topleft'
                  , maprange = F, darken = c(0,5, 'white')) +
  xlim(-8.1, -5.5) + ylim(54.08, 55.2)

belfast_google <- get_map('belfast, northern ireland', zoom = 12
                          , color = 'bw', maptype = 'roadmap')

## KILLS BY SIDE: Northern Ireland
sides_map <- nireland +
  geom_point(aes(x = longitd, y = latitud, color = PERPFACTION
                 , size = log(num_deaths)), alpha = 0.5
             , data = deaths_plot) +
  scale_size_continuous('# of events', range = c(3,10), guide = F) +
  scale_colour_manual('Faction', labels = c('Republican', 'British Army', 'Unionist')
                      , values = c('#1b9e77', '#386cb0', 'red')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7))) +
  labs(title = "Deaths in the Troubles, 1969-2003")

dev.off()
pdf(file = 'NIDeathsSide.pdf', height = 9, width = 9)
sides_map
dev.off()

## KILLS BY SIDE AND TYPE: Northern Ireland
deaths_type <- deaths[intrctn %in% c('bombing', 'shooting') & PERPFACTION != 'State']
sides_map <- nireland +
  geom_point(aes(x = longitd, y = latitud, color = PERPFACTION
                 , shape = intrctn), alpha = 0.5
             , data = deaths_type
             , size = 4) +
  scale_shape_manual('Violence', values = c(0,4)) +
  scale_size_continuous('# of events', range = c(3,10), guide = F) +
  scale_colour_manual('Faction', labels = c('Republican', 'Unionist')
                      , values = c('#386cb0', 'red')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7))) +
  labs(title = "Deaths in the Troubles, 1969-2003")

dev.off()
pdf(file = 'NIDeathsSideType.pdf', height = 9, width = 11)
sides_map
dev.off()


###### Subset Belfast data
belfast_deaths <- belfast_deaths[PERPFACTION != 'State']
belfast_deathtypes <- belfast_deaths[intrctn %in% c('bombing', 'shooting')]
## KILLS BY SIDE: Belfast
belfast <- ggmap(belfast_google
                 , extent = 'device', legend = 'bottomright'
                 , darken = c(0.35, 'white')
                 , maprange = F)

belfast_kills <- belfast +
  geom_point(aes(x = longitd, y = latitud
                 , color = PERPFACTION), alpha = 0.4
             , data = belfast_deathtypes, size = 3
             , shape = 4) +
  scale_colour_manual('Faction', labels = c('Republican', 'Unionist')
                      , values = c('blue', 'red')) +
  scale_shape_discrete(solid = F) + 
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 10)) +
  guides(colour = guide_legend(override.aes = list(size=4))) +
  labs(title = "Fatal Violence in the Troubles, 1969-2003")

dev.off()
pdf(file = 'BelfastDeathsSide.pdf', height = 9, width = 9)
belfast_kills
dev.off()

## ALL DEATHS: Belfast
belfast <- ggmap(belfast_google
                 , extent = 'device', legend = 'topleft'
                 , darken = c(0.35, 'white')
                 , maprange = F)

belfast_rep <- belfast %+% deaths + aes(x = longitd, y = latitud) + 
  stat_density2d(aes(fill = ..level..
                     , alpha = ..level..)
                 , bins = 20, geom = 'polygon'
                 , data = belfast_deaths[PERPFACTION == 'Republican']) +
  scale_fill_gradient(low = 'green', high = '#31a354') +
  scale_alpha(range = c(0.2, 0.3), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) +
  labs(title = "Republican Killings in Belfast, 1969-2003")


dev.off()
pdf(file = 'BelfastKillsRep.pdf', height = 9, width = 9)
belfast_rep
dev.off()


belfast_un <- belfast %+% deaths + aes(x = longitd, y = latitud) + 
  stat_density2d(aes(fill = ..level..
                     , alpha = ..level..)
                 , bins = 20, geom = 'polygon'
                 , data = belfast_deaths[PERPFACTION == 'Union']) +
  scale_fill_gradient(low = '#feb24c', high = '#f03b20') +
  scale_alpha(range = c(0.2, 0.3), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) +
  labs(title = "Unionist Killings in Belfast, 1969-2003")


dev.off()
pdf(file = 'BelfastKillsUn.pdf', height = 9, width = 9)
belfast_un
dev.off()

