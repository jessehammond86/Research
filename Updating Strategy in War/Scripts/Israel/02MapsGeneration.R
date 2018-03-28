rm(list = ls())
# setwd('/Users/jesse/Dropbox/Dissertation/Data/Israel')
setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel')
# setwd('/media/jesse/Files/Dropbox/Dissertation/Data/Israel')
library(foreign)
library(data.table)
library(TSA)
library(vars)
library(zoo)
library(xts)
library(lubridate)
library(changepoint)
library(bcp)
library(ecp)
library(TTR)
library(tsDyn)
library(forecast)
library(sp)
library(rgeos)
library(rgdal)
library(spdep)
library(ggmap)
library(rgdal)
library(rgeos)
library(raster)

#### Read in data
data <- fread('01IsrPalFullData.csv')

#################################################################
##
## MAKE SOME MAPS
##
######

# Actor setup
pal_milactors = c('palgun', 'palmil', 'palpol', 'fatah', 'hamas', 'ij', 'pflp', 'prc', 'dflp', 'pflp')
pal_civactors = c('palgov', 'palciv', 'palag', 'palres', 'palind')
isr_milactors = c('isrpol', 'idf')
isr_civactors = c('isrgov', 'isrciv', 'isrres', 'isrpol')

### Subset data
# Gaza Strip
gaza_plotdata <- data[(actor1 %in% isr_milactors & interactio %in% c('raid', 'air strike', 'shooting'))
                      | (actor1 %in% pal_milactors & interactio %in% c('bombing', 'shelling', 'shooting'))]

gaza_plotdata_points <- gaza_plotdata[!duplicated(gaza_plotdata[, list(long, lat, actor1_side)])]

# West Bank
westbank_plotdata <- data[((actor1 %in% isr_milactors & interactio %in% c('raid', 'air strike', 'shooting'))
                           | (actor1 %in% pal_milactors & interactio %in% c('bombing', 'shelling', 'shooting')))
                          & palestine == 1 & long > 34.8]

westbank_plotdata_points <- westbank_plotdata[!duplicated(westbank_plotdata[, list(long, lat, actor1_side)])]

# Israel/Palestine total
israel_plotdata <- data[((actor1 %in% isr_milactors & interactio %in% c('raid', 'air strike', 'shooting'))
                         | (actor1 %in% pal_milactors & interactio %in% c('bombing', 'shelling', 'shooting')))]

israel_plotdata_points <- israel_plotdata[!duplicated(israel_plotdata[, list(long, lat, actor1_side)])]


##### Create raw Google maps
israel <- ggmap(get_map('rishon letsiyon', zoom = 9, color = 'bw'), maptype = 'roadmap', extent = 'device', legend = 'topleft')
gaza <- ggmap(get_map('Bureij', zoom = 11, color = 'bw'), maptype = 'terrain', extent = 'device', legend = 'topleft')


########## Map some stuff
###### GAZA
### GAZA EVENTS BY SIDE AND CASUALTY
dev.off()
pdf(file = 'gazaSideCas.pdf', height = 8, width = 8)
gaza + geom_point(aes(x = long, y = lat, color = actor1_side, shape = actor1_side, size = 2 + log(all_cas)), alpha = 0.75
                  , position = position_jitter(w = 0.01, h = 0.01)
                  , data = gaza_plotdata[actor1_side == 'PAL']) +
  geom_point(aes(x = long, y = lat, color = actor1_side, shape = actor1_side, size = 0.5 + log(all_cas)), alpha = 0.5
             , position = position_jitter(w = 0.015, h = 0.015)
             , data = gaza_plotdata[actor1_side == 'ISR']) +
  scale_shape('Actor', labels = c('ISR', 'PAL')) +
  scale_size_continuous('Log(dead+wounded)', range = c(1,5), guide = F) +
  scale_colour_discrete('Actor', labels = c('ISR', 'PAL')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()


### PAL-INITIATED GAZA EVENTS BY COUNT AND TYPE
dev.off()
pdf(file = 'gazaPalCountType.pdf', height = 8, width = 8)
gaza + geom_point(aes(x = long, y = lat, color = interactio, size = log(all_cas)), alpha = 0.6
                  , position = position_jitter(w = 0.01, h = 0.01)
                  , data = gaza_plotdata[actor1_side == 'PAL']) +
  scale_size_continuous('Log(event count)', range = c(1,25), guide = F) +
  scale_colour_discrete('PAL Action', labels = c('bombing', 'shelling', 'shooting')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()

### ISR-INITIATED GAZA EVENTS BY COUNT AND TYPE
pdf(file = 'gazaIsrCountType.pdf', height = 8, width = 8)
gaza + geom_point(aes(x = long, y = lat, color = interactio, size = log(all_cas)), alpha = 0.5
                  , position = position_jitter(w = 0.01, h = 0.01)
                  , data = gaza_plotdata[actor1_side == 'ISR']) +
  scale_size_continuous('Log(event count)', range = c(1,10), guide = F) +
  scale_colour_discrete('ISR Action', labels = c('air strike', 'raid', 'shooting')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()



###### WEST BANK
### WEST BANK EVENTS BY SIDE AND CASUALTY
dev.off()
pdf(file = 'westBankSideCas.pdf', height = 8, width = 8)
israel + geom_point(aes(x = long, y = lat, color = actor1_side, shape = actor1_side, size = log(all_cas)), alpha = 0.35
                    , position = position_jitter(w = 0.015, h = 0.015)
                    , data = westbank_plotdata) +
  scale_shape('Actor', labels = c('ISR', 'PAL')) +
  scale_size_continuous('Log(dead+wounded)', range = c(1,10), guide = F) +
  scale_colour_discrete('Actor', labels = c('ISR', 'PAL')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()


### PAL-INITIATED WEST BANK EVENTS BY COUNT AND TYPE
dev.off()
pdf(file = 'westBankPalCountType.pdf', height = 8, width = 8)
israel + geom_point(aes(x = long, y = lat, color = interactio, size = log(event_count)), alpha = 0.5
                    , data = westbank_plotdata_points[actor1_side == 'PAL']) +
  scale_size_continuous('Log(event count)', range = c(1,10), guide = F) +
  scale_colour_discrete('PAL Action', labels = c('bombing', 'shelling', 'shooting')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()

### ISR-INITIATED WEST BANK EVENTS BY COUNT AND TYPE
pdf(file = 'westBankIsrCountType.pdf', height = 8, width = 8)
israel + geom_point(aes(x = long, y = lat, color = interactio, size = log(event_count)), alpha = 0.5
                    , data = westbank_plotdata_points[actor1_side == 'ISR']) +
  scale_size_continuous('Log(event count)', range = c(1,10), guide = F) +
  scale_colour_discrete('ISR Action', labels = c('air strike', 'raid', 'shooting')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()



###### ISRAEL
## ISRAEL EVENTS BY SIDE AND CASUALTY
dev.off()
pdf(file = 'IsrSideCas.pdf', height = 8, width = 8)
israel + geom_point(aes(x = long, y = lat, color = actor1_side, shape = actor1_side, size = log(all_cas)), alpha = 0.5
                    , position = position_jitter(w = 0.015, h = 0.015)
                    , data = israel_plotdata) +
  scale_shape('Actor', labels = c('ISR', 'PAL')) +
  scale_size_continuous('Log(dead+wounded)', range = c(1,10), guide = F) +
  scale_colour_discrete('Actor', labels = c('ISR', 'PAL')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()

### PAL-INITIATED ISRAEL EVENTS BY COUNT AND TYPE
dev.off()
pdf(file = 'IsrPalCountType.pdf', height = 8, width = 8)
israel + geom_point(aes(x = long, y = lat, color = interactio, size = log(event_count)), alpha = 0.5
                    , data = israel_plotdata_points[actor1_side == 'PAL']) +
  scale_size_continuous('Log(event count)', range = c(1,10), guide = F) +
  scale_colour_discrete('PAL Action', labels = c('bombing', 'shelling', 'shooting')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()

### ISR-INITIATED ISRAEL EVENTS BY COUNT AND TYPE
pdf(file = 'IsrIsrCountType.pdf', height = 8, width = 8)
israel + geom_point(aes(x = long, y = lat, color = interactio, size = log(event_count)), alpha = 0.5
                    , data = israel_plotdata_points[actor1_side == 'ISR']) +
  scale_size_continuous('Log(event count)', range = c(1,10), guide = F) +
  scale_colour_discrete('ISR Action', labels = c('air strike', 'raid', 'shooting')) +
  theme(legend.box.just = "left"
        , legend.text = element_text(size = 15)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
dev.off()
