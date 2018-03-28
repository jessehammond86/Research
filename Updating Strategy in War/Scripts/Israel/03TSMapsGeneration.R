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

### Collapse to time series format
old_data2 <- data
data <- data[!duplicated(data[, week]), ]
data$date <- as.Date(data$date)

###### Create some smoothed data to plot
Nsmooth <- 8
plot_data <- data[, list(date, isr_bigraid, pal_indirect, pal_event, isr_event)]
plot_data[, isr_bigraid := SMA(xts(data[, isr_bigraid], order.by = data$date), n = Nsmooth)]
plot_data[, pal_remote := SMA(xts(data[, pal_indirect], order.by = data$date), n = Nsmooth)]
plot_data[, mean_palscore := SMA(xts(data[, mean_palscore], order.by = data$date), n = Nsmooth)]
plot_data[, mean_isrscore := SMA(xts(data[, mean_isrscore], order.by = data$date), n = Nsmooth)]

plot_data[, pal_event := SMA(xts(data[, pal_event], order.by = data$date), n = Nsmooth)]
plot_data[, isr_event := SMA(xts(data[, isr_event], order.by = data$date), n = Nsmooth)]

## ISRAEL AND PALESTINE EVENT COUNTS
event_data <- melt(plot_data[, list(date, pal_event, isr_event)], id = 'date')
dev.off()
pdf(file = 'isrPalEvents.pdf', width = 10, height = 6)
ggplot() +
  geom_line(aes(x = date, y = value, color = variable), data = event_data) +
  xlab('Time (weeks)') +
  ylab('Violent events per week (smoothed)') +
  scale_colour_discrete('Initiator', labels = c('Palestinian', 'Israeli')) +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()

## ISRAEL RAIDS AND SHIFTS
date_range <- data.frame(from = as.Date(c('2002-03-24', '2003-06-29'))
                         , to = as.Date(c('2002-05-03', '2003-08-24')))
dev.off()
pdf(file = 'isrBigRaids.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = plot_data, aes(date, isr_bigraid)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.35, fill = c('red', 'blue')) +
  xlab('Time (weeks)') +
  ylab('ISR combined-arms raids (%)') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2002-01-01'), y = 0.08, label = 'Operation\n Defensive\n Shield') +
  annotate('text', x = as.Date('2003-04-01'), y = 0.08, label = 'Temporary\n Armistice')
dev.off()

## PALESTINE SHELLING AND SHIFTS
date_range <- data.frame(from = as.Date(c('2002-03-24', '2004-03-21'))
                         , to = as.Date(c('2002-05-03', '2004-03-28')))
pdf(file = 'palIndirect.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = plot_data, aes(date, pal_remote)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = c(0.35, 0.75), fill = c('red')) +
  xlab('Time (weeks)') +
  ylab('PAL indirect-fire attacks (%)') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2002-01-01'), y = 0.65, label = 'Operation\n Defensive\n Shield') +
  annotate('text', x = as.Date('2003-12-01'), y = 0.65, label = 'Death of\n Ahmed Yassin')
dev.off()


# Plot mean type of violence over time
event_data <- melt(plot_data[, list(date, mean_palscore, mean_isrscore)], id = 'date')
dev.off()
pdf(file = 'isrPalScore.pdf', width = 10, height = 6)
ggplot() +
  geom_line(aes(x = date, y = value, color = variable), data = event_data) +
  xlab('Time (weeks)') +
  ylab('Mean event type (smoothed)') +
  scale_colour_discrete('Initiator', labels = c('Palestinian', 'Israeli')) +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()