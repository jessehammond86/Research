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
data$date <- as.Date(data$date)

#################################################################
data <- data[!duplicated(data$week)]

### Palestinian strategy
## Activity
inputs0 <- xts(data[, list(pal_event)], order.by = data$date)
pal_event <- inputs0[!is.na(inputs0)]

## Technology
inputs1 <- xts(data[, list(pal_remote)], order.by = data$date)
pal_indirect <- inputs1[!is.na(inputs1)]

## Target
inputs2 <- xts(data[, list(pal_civtargeting)], order.by = data$date)
pal_civtargeting <- inputs2[!is.na(inputs2)]
inputs2 <- xts(data[, list(pal_miltargeting)], order.by = data$date)
pal_miltargeting <- inputs2[!is.na(inputs2)]

## Location
inputs3 <- xts(data[, list(mean_palevent_area_ab)], order.by = data$date)
mean_palevent_area_ab <- inputs3[!is.na(inputs3)]
inputs3 <- xts(data[, list(mean_palevent_area_c)], order.by = data$date)
mean_palevent_area_c <- inputs3[!is.na(inputs3)]

## Terrorism
inputs4 <- xts(data[, list(mean_palscore)], order.by = data$date)
mean_palscore <- inputs4[!is.na(inputs4)]

### Israeli strategy

## Activity
inputs0 <- xts(data[, list(isr_event)], order.by = data$date)
isr_event <- inputs0[!is.na(inputs0)]

## Technology
inputs1 <- xts(data[, list(isr_bigtech)], order.by = data$date)
isr_bigtech <- inputs1[!is.na(inputs1)]
inputs1 <- xts(data[, list(isr_smalltech)], order.by = data$date)
isr_smalltech <- inputs2[!is.na(inputs1)]

## Target
inputs2 <- xts(data[, list(isr_civtargeting)], order.by = data$date)
isr_civtargeting <- inputs2[!is.na(inputs2)]
inputs2 <- xts(data[, list(isr_miltargeting)], order.by = data$date)
isr_miltargeting <- inputs2[!is.na(inputs2)]

## Location
# City
inputs3 <- xts(data[, list(mean_isrevent_city)], order.by = data$date)
mean_isrevent_city <- inputs3[!is.na(inputs3)]
# Area A/B
inputs3 <- xts(data[, list(mean_isrevent_area_ab)], order.by = data$date)
mean_isrevent_area_ab <- inputs3[!is.na(inputs3)]

## Repression
inputs4 <- xts(data[, list(mean_isrscore)], order.by = data$date)
mean_isrscore <- inputs4[!is.na(inputs4)]

## Casualty Ratio
inputs5 <- xts(data[, list(pal_casratio)], order.by = data$date)
pal_casratio <- inputs5[!is.na(inputs5)]



#############################################################################
#############################################################################
input_data <- cbind(pal_event
                    , pal_indirect
                    , pal_civtargeting, pal_miltargeting
                    , mean_palscore
                    , mean_palevent_area_ab, mean_palevent_area_c
                    , isr_event
                    , isr_bigtech, isr_smalltech
                    , mean_isrscore
                    , isr_civtargeting, isr_miltargeting
                    , mean_isrevent_city, mean_isrevent_area_ab
                    , pal_casratio)


input_data <- scale(input_data)
colnames <- c('pal_event'
              , 'pal_indirect'
              , 'pal_civtargeting', 'pal_miltargeting'
              , 'mean_palscore'
              , 'mean_palevent_area_ab', 'mean_palevent_area_c'
              , 'isr_event'
              , 'isr_bigtech', 'isr_smalltech'
              , 'mean_isrscore'
              , 'isr_civtargeting', 'isr_miltargeting'
              , 'mean_isrevent_city', 'mean_isrevent_area_ab'
              , 'pal_casratio'
              , 'week')


for(i in 1:dim(input_data)[2]){
  print(colnames[i])
  print(Box.test(input_data[,i]))
}
week <- as.Date(index(input_data))
input_data <- data.table(as.matrix(input_data))
input_data[, week := week]
setnames(input_data, colnames)
input_data <- input_data[complete.cases(input_data), ]




write.csv(input_data, file = '02IsrPalTSData.csv', row.names = F)
