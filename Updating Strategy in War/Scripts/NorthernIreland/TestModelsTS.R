rm(list = ls())
setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland\\Census Data')
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
# soadata <- readOGR('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
#                    , 'SOA_agg_2001census'
#                    , stringsAsFactors = F)

## Deaths data
deaths <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland\\IrelandGIS'
                  , layer = 'geodeaths'
                  , stringsAsFactors = F)
# deaths <- readOGR('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
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
# bases <- readOGR('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
#                  , 'geobases'
#                  , stringsAsFactors = F)

## Peacelines data
peacelines <- readOGR(dsn = 'C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland\\IrelandGIS'
                      , layer = 'peacelines')
# peacelines <- readOGR('/media/jesse/Files/Dropbox/Prospectus/Data/NorthernIreland/IrelandGIS'
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
## Further preprocessing: generating some new variables and reshaping
##  DEATH-centric to LOCATION-centric data
##
#################

## Convert to LOCATION-centric data level
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

#### PCA
library(devtools)
library(ggbiplot)
library(FactoMineR)

test2 <- test[!duplicated(SOALAB)]

foo1 <- prcomp(~ SPOPDENS + SPCTCATH + SLTUNEMP
                             + BORDER + SBABASES + SIRBASES
              , data = test2)

foo2 <- PCA(test2[, list(SPOPDENS, SPCTCATH, SLTUNEMP
                      , BORDER, SBABASES, SIRBASES)]
            , scale.unit = F
            , ncp = 5)
summary(foo2)

df <- fortify(foo1)

g <- ggplot(df, aes(x = PC1, PC2)) +
  geom_point() +
  geom_axis(data = attr(df, "basis"), aes(label = .name))
print(g)

g <- ggplot(df, aes(x = PC1, PC2)) +
  geom_point() +
  geom_axis(data = attr(df, "basis"), aes(label = .name)) +
  annotate("circle", x = 0, y = 0, radius = 1, alpha = 1/4)
print(g)



ggbiplot(foo2, obs.scale = 1, var.scale = 1,
        ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')


test[, c('component1', 'component2') := data.table(principal(test[, list(STOTPOP, SPOPDENS, SPCTCATH, SLTUNEMP, SPCTURBN
                                          , BORDER, SBABASES, SIRBASES)]
                              , n.obs = 3000
                              , nfactors = 2)$scores)]
test[, cluster := bclust(test[, list(STOTPOP, SPOPDENS, SPCTCATH, SLTUNEMP, SPCTURBN
                                     , BORDER, SBABASES, SIRBASES)]
                         , centers = 5)$cluster]

perps <- unique(deaths$prp_stts_s)
# test[prp_stts_s %in% perps[c(4, 5, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 25, 28, 29, 31)]
test[prp_stts_s %in% perps[c(5, 8, 10, 11, 12, 13, 15, 16, 25, 28, 29, 31)]
     , PERPFACTION := 'Union']
test[prp_stts_s %in% perps[c(1, 2, 6, 7, 17, 19, 20, 21, 22, 23, 24, 26, 30)]
     , PERPFACTION := 'Republican']
test[prp_stts_s %in% perps[c(4, 9, 14, 18)]
     , PERPFACTION := 'State']

test[intrctn %in% c('beating', 'shooting'), VIOLENCE := 'Selective']
test[intrctn %in% c('bombing', 'shelling'), VIOLENCE := 'Indiscriminate']

test[cntxt_m %in% c('Ambush', 'Beating', 'Gun Battle', 'Riot Affray', 'Stabbing'), TACTICS := 'Direct']
test[cntxt_m %in% c('Assasination', 'Explosion', 'Dumped', 'Rocket', 'Sniper', 'Arson'), TACTICS := 'Indirect']

victims <- unique(deaths$vc_stts_s)
test[vc_stts_s %in% victims[c(3, 11)]
     , VICTYPE := 'Civilian']
test[vc_stts_s %in% victims[-c(3, 11)], VICTYPE := 'Military']

test[vc_rlg_s %in% c('Catholic'), VICRELIG := 'Catholic']
test[vc_rlg_s %in% c('Protestant'), VICRELIG := 'Protestant']
test[vc_rlg_s %in% c('nfNI', 'nfNIB', 'nfNIRI'), VICRELIG := 'Irrelevant']

test <- test[complete.cases(test[, list(PERPFACTION, VIOLENCE, VICTYPE)]), ]

#########################################

test$death <- 1
testmo <- test[, list(
  REPUBKILLS = sum(death[PERPFACTION == 'Republican'])
  , UNIONKILLS = sum(death[PERPFACTION == 'Union'])
  , STATEKILLS = sum(death[PERPFACTION == 'State'])
  , REPUBINDIS = sum(death[PERPFACTION == 'Republican' & VIOLENCE == 'Indiscriminate'])
  , REPUBSELECT = sum(death[PERPFACTION == 'Republican' & VIOLENCE == 'Selective'])
  , UNIONINDIS = sum(death[PERPFACTION == 'Union' & VIOLENCE == 'Indiscriminate'])
  , UNIONSELECT = sum(death[PERPFACTION == 'Union' & VIOLENCE == 'Selective'])
  , REPUBMILKILLS = sum(death[PERPFACTION == 'Republican' & VICTYPE == 'Military'])
  , REPUBCIVKILLS = sum(death[PERPFACTION == 'Republican' & VICTYPE == 'Civilian'])
  , UNIONMILKILLS = sum(death[PERPFACTION == 'Union' & VICTYPE == 'Military'])
  , UNIONCIVKILLS = sum(death[PERPFACTION == 'Union' & VICTYPE == 'Civilian'])
  , UNIONSCORE = mean(SCORE[PERPFACTION == 'Union'], na.rm = T)
  , REPUBSCORE = sd(SCORE[PERPFACTION == 'Republican'], na.rm = T)
  , REPUBDIST = mean(spDists(cbind(coords.x1[PERPFACTION == 'Republican'], coords.x1[PERPFACTION == 'Republican']), longlat = F), na.rm = T)
), by = list(quarter)]
testmo[is.nan(REPUBDIST), REPUBDIST := 0]
testmo[is.nan(REPUBSCORE), REPUBSCORE := 0]
testmo[, SUNIONSCORE := UNIONSCORE / (2*sd(UNIONSCORE))]
testmo[, SREPUBSCORE := REPUBSCORE / (2*sd(REPUBSCORE))]
testmo[, SREPUBDIST := REPUBDIST / (2*sd(REPUBDIST))]
testmo[, INDISTINCT := REPUBINDIS / ((REPUBINDIS + REPUBSELECT)+1)]
