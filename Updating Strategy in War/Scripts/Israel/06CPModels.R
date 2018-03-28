rm(list = ls())
setwd('/Users/jesse/Dropbox/Dissertation/Data/Israel')
# setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel')
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
library(changepoint)
library(bcp)
library(ecp)

#### Read in data
input_data <- fread('02IsrPalTSData.csv')
input_data$week <- as.Date(input_data$week)

#####################################################################################
## SUPPLEMENTARY ANALYSIS: Is there evidence that PAL/ISR behavior changed after Defensive Shield?
#####################################################################################

inputs <- input_data[,list(week, isr_bigtech, pal_indirect
                           , isr_civtargeting, pal_civtargeting
                           , mean_isrevent_area_ab, mean_palevent_area_c
                           , mean_isrscore, mean_palscore)]

##### CP detection
# PAL indirect
pal_cp_1 <- e.divisive(as.matrix(inputs$pal_indirect), R = 999, sig.lvl = 0.05)
pal_change_idx_1 <- pal_cp_1$estimates[c(2,3)]
pal_means_1 <- data.frame('mean' = c(mean(inputs[1:pal_change_idx_1[1], pal_indirect])
                                     , mean(inputs[pal_change_idx_1[1]:pal_change_idx_1[2], pal_indirect])
                                     , mean(inputs[pal_change_idx_1[2]:nrow(inputs), pal_indirect])))

# PAL civilian targeting
pal_cp_2 <- e.divisive(as.matrix(inputs$pal_civtargeting), R = 999, sig.lvl = 0.05)
pal_change_idx_2 <- pal_cp_2$estimates[c(2,3)]
pal_means_2 <- data.frame('mean' = c(mean(inputs[1:pal_change_idx_2[1], pal_civtargeting])
                                     , mean(inputs[pal_change_idx_2[1]:pal_change_idx_2[2], pal_civtargeting])
                                     , mean(inputs[pal_change_idx_2[2]:nrow(inputs), pal_civtargeting])))

# PAL area C
pal_cp_3 <- e.divisive(as.matrix(inputs$mean_palevent_area_c), R = 999, sig.lvl = 0.05)
pal_change_idx_3 <- pal_cp_3$estimates[c(2,3)]
pal_means_3 <- data.frame('mean' = c(mean(inputs[1:pal_change_idx_3[1], mean_palevent_area_c])
                                     , mean(inputs[pal_change_idx_3[1]:pal_change_idx_3[2], mean_palevent_area_c])
                                     , mean(inputs[pal_change_idx_3[2]:nrow(inputs), mean_palevent_area_c])))

# PAL terrorism
pal_cp_4 <- e.divisive(as.matrix(inputs$mean_palscore), R = 999, sig.lvl = 0.05)
pal_change_idx_4 <- pal_cp_4$estimates[c(2,3,4)]
pal_means_4 <- data.frame('mean' = c(mean(inputs[1:pal_change_idx_4[1], mean_palscore])
                                     , mean(inputs[pal_change_idx_4[1]:pal_change_idx_4[2], mean_palscore])
                                     , mean(inputs[pal_change_idx_4[2]:pal_change_idx_4[3], mean_palscore])
                                     , mean(inputs[pal_change_idx_4[3]:nrow(inputs), mean_palscore])))


# ISR heavy arms
isr_cp_1 <- e.divisive(as.matrix(inputs$isr_bigtech), R = 999)
isr_change_idx_1 <- isr_cp_1$estimates[c(2,3)]
isr_means_1 <- data.frame('mean' = c(mean(inputs[1:isr_change_idx_1[1], isr_bigtech])
                                     , mean(inputs[isr_change_idx_1[1]:nrow(inputs), isr_bigtech])))

# ISR civilian targeting
isr_cp_2 <- e.divisive(as.matrix(inputs$isr_civtargeting), R = 999, sig.lvl = 0.05)
isr_change_idx_2 <- isr_cp_2$estimates[c(2)]
isr_means_2 <- data.frame('mean' = c(mean(inputs[1:isr_change_idx_2[1], isr_civtargeting])
                                     , mean(inputs[isr_change_idx_2[1]:nrow(inputs), isr_civtargeting])))

# ISR area A/B
isr_cp_3 <- e.divisive(as.matrix(inputs$mean_isrevent_area_ab), R = 999, sig.lvl = 0.05)
isr_change_idx_3 <- isr_cp_3$estimates[c(2)]
isr_means_3 <- data.frame('mean' = c(mean(inputs[1:isr_change_idx_3[1], mean_isrevent_area_ab])
                                     , mean(inputs[isr_change_idx_3[1]:nrow(inputs), mean_isrevent_area_ab])))

# ISR repression
isr_cp_4 <- e.divisive(as.matrix(inputs$mean_isrscore), R = 999, sig.lvl = 0.01)
isr_change_idx_4 <- isr_cp_4$estimates[c(2,3)]
isr_means_4 <- data.frame('mean' = c(mean(inputs[1:isr_change_idx_4[1], mean_isrevent_area_c])
                                     , mean(inputs[isr_change_idx_4[1]:isr_change_idx_4[2], mean_isrevent_area_c])
                                     , mean(inputs[isr_change_idx_4[2]:nrow(inputs), mean_isrevent_area_c])))

##### Plotting CP
Nsmooth <- 8
inputs[, pal_indirect := SMA(xts(inputs[, pal_indirect], order.by = inputs$week), n = Nsmooth)]
inputs[, pal_civtargeting := SMA(xts(inputs[, pal_civtargeting], order.by = inputs$week), n = Nsmooth)]
inputs[, mean_palevent_area_c := SMA(xts(inputs[, mean_palevent_area_c], order.by = inputs$week), n = Nsmooth)]
inputs[, mean_palscore := SMA(xts(inputs[, mean_palscore], order.by = inputs$week), n = Nsmooth)]

inputs[, isr_bigtech := SMA(xts(inputs[, isr_bigtech], order.by = inputs$week), n = Nsmooth)]
inputs[, isr_civtargeting := SMA(xts(inputs[, isr_civtargeting], order.by = inputs$week), n = Nsmooth)]
inputs[, mean_isrevent_area_ab := SMA(xts(inputs[, mean_isrevent_area_ab], order.by = inputs$week), n = Nsmooth)]
inputs[, mean_isrscore := SMA(xts(inputs[, mean_isrscore], order.by = inputs$week), n = Nsmooth)]

## PAL CP 1: indirect-fire weapons
date_range <- data.frame('from' = inputs$week[pal_change_idx_1]
                         , 'to' = inputs$week[pal_change_idx_1 + 2])

dev.off()
pdf(file = 'palCP1.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = inputs, aes(week, pal_indirect)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.5, fill = c('blue')) +
  geom_segment(aes(x=inputs$week[1], xend=inputs$week[pal_change_idx_1[1]]
                   , y=0.286, yend=0.286), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_1[1]], xend=inputs$week[pal_change_idx_1[2]]
                   , y=0.398, yend=0.398), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_1[2]], xend=inputs$week[nrow(inputs)]
                   , y=0.625, yend=0.625), color = 'red') +
  xlab('Time') +
  ylab('PAL indirect-fire attacks (%)') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2001-09-01'), y = 0.6, label = 'First changepoint\n (Defensive Shield)') +
  annotate('text', x = as.Date('2004-02-01'), y = 0.6, label = 'Second changepoint\n (?)')
dev.off()


## PAL CP 2: civilian targeting
date_range <- data.frame('from' = inputs$week[pal_change_idx_2]
                         , 'to' = inputs$week[pal_change_idx_2 + 2])

dev.off()
pdf(file = 'palCP2.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = inputs, aes(week, pal_civtargeting)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.5, fill = c('blue')) +
  geom_segment(aes(x=inputs$week[1], xend=inputs$week[pal_change_idx_2[1]]
                   , y=0.267, yend=0.267), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_2[1]], xend=inputs$week[pal_change_idx_2[2]]
                   , y=-0.400, yend=-0.400), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_2[2]], xend=inputs$week[nrow(inputs)]
                   , y=0.442, yend=0.442), color = 'red') +
  xlab('Time') +
  ylab('PAL reliance on targeting civilians') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2002-06-01'), y = 1.0, label = 'First changepoint\n (?)') +
  annotate('text', x = as.Date('2004-03-01'), y = 1.0, label = 'Second changepoint\n (?)')
dev.off()


## PAL CP 3: violence in Israel & area C
date_range <- data.frame('from' = inputs$week[pal_change_idx_3]
                         , 'to' = inputs$week[pal_change_idx_3 + 2])

dev.off()
pdf(file = 'palCP3.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = inputs, aes(week, mean_palevent_area_c)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.5, fill = c('blue')) +
  geom_segment(aes(x=inputs$week[1], xend=inputs$week[pal_change_idx_3[1]]
                   , y=0.894, yend=0.894), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_3[1]], xend=inputs$week[pal_change_idx_3[2]]
                   , y=-0.266, yend=-0.266), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_3[2]], xend=inputs$week[nrow(inputs)]
                   , y=-0.742, yend=-0.742), color = 'red') +
  xlab('Time') +
  ylab('PAL violence in Israel and area C') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2001-12-01'), y = 0.0, label = 'First changepoint\n (?)') +
  annotate('text', x = as.Date('2004-01-01'), y = 1.0, label = 'Second changepoint\n (?)')
dev.off()

## PAL CP 4: PAL mean violence type
date_range <- data.frame('from' = inputs$week[pal_change_idx_4]
                         , 'to' = inputs$week[pal_change_idx_4 + 2])

dev.off()
pdf(file = 'palCP4.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = inputs, aes(week, mean_palscore)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.5, fill = c('blue')) +
  geom_segment(aes(x=inputs$week[1], xend=inputs$week[pal_change_idx_4[1]]
                   , y=0.313, yend=0.313), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_4[1]], xend=inputs$week[pal_change_idx_4[2]]
                   , y=-0.173, yend=-0.173), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_4[2]], xend=inputs$week[pal_change_idx_4[3]]
                   , y=-0.878, yend=-0.878), color = 'red') +
  geom_segment(aes(x=inputs$week[pal_change_idx_4[3]], xend=inputs$week[nrow(inputs)]
                   , y=0.681, yend=0.681), color = 'red') +
  xlab('Time') +
  ylab('PAL reliance on terrorism') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2002-03-01'), y = 1.2, label = 'First changepoint\n (?)') +
  annotate('text', x = as.Date('2003-06-01'), y = 1.2, label = 'Second changepoint\n (?)') +
  annotate('text', x = as.Date('2004-04-01'), y = 1.2, label = 'Third changepoint\n (?)')
dev.off()


## ISR CP 1: heavy arms usage
date_range <- data.frame('from' = inputs$week[isr_change_idx_1]
                         , 'to' = inputs$week[isr_change_idx_1 + 2])

dev.off()
pdf(file = 'isrCP1.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = inputs, aes(week, isr_bigtech)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.5, fill = c('blue')) +
  geom_segment(aes(x=inputs$week[1], xend=inputs$week[isr_change_idx_1[1]]
                   , y=0.402, yend=0.402), color = 'red') +
  geom_segment(aes(x=inputs$week[isr_change_idx_1[1]], xend=inputs$week[nrow(inputs)]
                   , y=-0.499, yend=-0.499), color = 'red') +
  xlab('Time') +
  ylab('ISR reliance on heavy arms') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2003-09-01'), y = 1.0, label = 'First changepoint\n (?)')
dev.off()


## ISR CP 2: civilian targeting
date_range <- data.frame('from' = inputs$week[isr_change_idx_2]
                         , 'to' = inputs$week[isr_change_idx_2 + 2])

dev.off()
pdf(file = 'isrCP2.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = inputs, aes(week, isr_civtargeting)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.5, fill = c('blue')) +
  geom_segment(aes(x=inputs$week[1], xend=inputs$week[isr_change_idx_2[1]]
                   , y=-0.052, yend=-0.052), color = 'red') +
  geom_segment(aes(x=inputs$week[isr_change_idx_2[1]], xend=inputs$week[nrow(inputs)]
                   , y=0.099, yend=0.099), color = 'red') +
  xlab('Time') +
  ylab('isr reliance on targeting civilians') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2003-03-01'), y = 0.75, label = 'First changepoint\n (?)')
dev.off()


## ISR CP 3: violence in Israel & area C
date_range <- data.frame('from' = inputs$week[isr_change_idx_3]
                         , 'to' = inputs$week[isr_change_idx_3 + 2])

dev.off()
pdf(file = 'isrCP3.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = inputs, aes(week, mean_isrevent_area_ab)) +
  geom_rect(data = date_range, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf)
            , alpha = 0.5, fill = c('blue')) +
  geom_segment(aes(x=inputs$week[1], xend=inputs$week[isr_change_idx_3[1]]
                   , y=-0.567, yend=-0.567), color = 'red') +
  geom_segment(aes(x=inputs$week[isr_change_idx_3[1]], xend=inputs$week[nrow(inputs)]
                   , y=0.109, yend=0.109), color = 'red') +
  xlab('Time') +
  ylab('ISR violence in Gaza and areas A/B') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2001-05-01'), y =0.5, label = 'First changepoint\n (?)')
dev.off()

## ISR CP 4: ISR mean violence type

dev.off()
pdf(file = 'isrCP4.pdf', width = 10, height = 6)
ggplot() +
  geom_line(data = inputs, aes(week, mean_isrscore)) +
  xlab('Time') +
  ylab('ISR large-scale repressive violence') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14)) +
  annotate('text', x = as.Date('2003-06-01'), y = 0.75, label = 'No changepoints\n found')
dev.off()


#################################################
## SUPPLEMENTARY VAR MODELS
input_data <- cbind(pal_event, mean_palevent_bdist
                    , pal_indirect, pal_bombing
                    , pal_suicidebombing
                    , pal_civtargeting, pal_civtargeting
                    , isr_event, mean_isrevent_bdist
                    , isr_bigtech, isr_smalltech
                    #, isr_nonviol
                    , isr_assassination
                    , isr_civtargeting, isr_miltargeting
                    , pal_casratio)
input_data <- scale(input_data)
colnames <- c('pal_event', 'mean_palevent_bdist'
              , 'pal_indirect', 'pal_bombing'
              , 'pal_suicidebombing'
              , 'pal_civtargeting', 'pal_miltargeting'
              , 'isr_event', 'mean_isrevent_bdist'
              , 'isr_bigtech', 'isr_smalltech'
              #, 'isr_nonviol'
              , 'isr_assassination'
              , 'isr_civtargeting', 'isr_miltargeting'
              , 'pal_casratio'
              , 'week')
for(i in 1:dim(input_data)[2]){
  #   input_data[, i] <- diff(input_data[, i])
  #   input_data[is.na(input_data)] <- 0
  print(colnames[i])
  print(Box.test(input_data[,i]))
}
week <- as.Date(index(input_data))
input_data <- data.table(as.matrix(input_data))
input_data[, week := week]
setnames(input_data, colnames)
input_data <- input_data[complete.cases(input_data), ]

#### ENDOGENOUS SYSTEMS
#################### 3-WAY INTERACTIONS
########## PAL TARG UP - ISR TECH UP
#### PAL REGIME 1
exog = cbind(lag(xts(input_data[1:pal_change_idx[1],list(pal_casratio)], order.by = input_data$week[1:pal_change_idx[1]])),
             lag(xts(input_data[1:pal_change_idx[1],list(pal_casratio)], order.by = input_data$week[1:pal_change_idx[1]]), 2),
             lag(xts(input_data[1:pal_change_idx[1],list(pal_casratio)], order.by = input_data$week[1:pal_change_idx[1]]), 3),
             lag(xts(input_data[1:pal_change_idx[1],list(pal_casratio)], order.by = input_data$week[1:pal_change_idx[1]]), 4))
exog[is.na(exog)] <- 0

inputs = xts(input_data[1:pal_change_idx[1],list(pal_civtargeting, isr_bigtech, pal_indirect
)], order.by = input_data$week[1:pal_change_idx[1]])
VARselect(inputs, lag.max = 8, type = 'both')$selection
model1 <- VAR(inputs, p = 4, type = 'both', exog = exog)
# Portmanteau test - passed!
serial.test(model1, type = 'PT.asymptotic')
# ARCH-LM test - passed!
arch.test(model1, lags.multi = 4)
summary(model1)

#### PAL REGIME 2
exog = cbind(lag(xts(input_data[pal_change_idx[1]:pal_change_idx[2],list(pal_casratio)], order.by = input_data$week[pal_change_idx[1]:pal_change_idx[2]])),
             lag(xts(input_data[pal_change_idx[1]:pal_change_idx[2],list(pal_casratio)], order.by = input_data$week[pal_change_idx[1]:pal_change_idx[2]]), 2),
             lag(xts(input_data[pal_change_idx[1]:pal_change_idx[2],list(pal_casratio)], order.by = input_data$week[pal_change_idx[1]:pal_change_idx[2]]), 3),
             lag(xts(input_data[pal_change_idx[1]:pal_change_idx[2],list(pal_casratio)], order.by = input_data$week[pal_change_idx[1]:pal_change_idx[2]]), 4))
exog[is.na(exog)] <- 0

inputs = xts(input_data[pal_change_idx[1]:pal_change_idx[2],list(pal_civtargeting, isr_bigtech, pal_indirect
)], order.by = input_data$week[pal_change_idx[1]:pal_change_idx[2]])
VARselect(inputs, lag.max = 8, type = 'both')$selection
model1 <- VAR(inputs, p = 1, type = 'both', exog = exog)
# Portmanteau test - passed!
serial.test(model1, type = 'PT.asymptotic')
# ARCH-LM test - passed!
arch.test(model1, lags.multi = 1)
summary(model1)





irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, cumulative = F)

### Plotting IRF
dev.off()
pdf(file = 'ptargItech.pdf')
plot(irf_m1)
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()

# PAL TARG UP - ISR TECH UP
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'pal_civtargeting', response = 'isr_bigtech')
dev.off()
pdf(file = 'itechPtarg.pdf', height = 5, width = 8)
plot(irf_m1, main = 'PAL civilian targeting', ylab = 'ISR heavy weapons use', xlab = 'foo')
dev.off()

# ISR TECH UP - PAL TECH UP
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_bigtech', response = 'pal_indirect')
dev.off()
pdf(file = 'itechPtarg.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR heavy weapons use', ylab = 'PAL indirect-fire weapons use', xlab = 'foo')
dev.off()

# ISR TECH UP - PAL TARG UP
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_bigtech', response = 'pal_civtargeting')
dev.off()
pdf(file = 'itechPtarg.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR heavy weapons use', ylab = 'PAL civilian targeting', xlab = 'foo')
dev.off()


########## ISR TARG DOWN - PAL ACT DOWN
inputs = xts(input_data[,list(isr_miltargeting, pal_event, isr_smalltech
)], order.by = input_data$week)
model1 <- VAR(inputs, p = 4, type = 'both')
# Consider using VECM model because ISR small tech is not stationary
# model1 <- VECM(inputs, lag = 4, include = 'both', estim = '2OLS', exogen = as.matrix(exog))
summary(model1)

irf_m1 <- irf(model1)
dev.off()
pdf(file = 'iact_itarg_pact_itech.pdf')
plot(irf_m1)
dev.off()

# ISR ACT UP - PAL ACT DOWN
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_miltargeting', response = 'pal_event')
dev.off()
pdf(file = 'iactPact.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR Miltary Targeting', ylab = 'PAL activity', xlab = 'foo')
dev.off()

### ONLY FOR VISUALIZATION - REVERSE SIGN ON PAL EVENTS TO SHOW NEGATIVE SHOCK
## ISR ACT UP - ISR TARG DOWN - PAL ACT DOWN - ISR TECH DOWN
input_data$pal_event2 <- -1 * input_data$pal_event
inputs = xts(input_data[,list(isr_miltargeting, pal_event2, isr_smalltech
)], order.by = input_data$week)
# Using VECM model because ISR small tech is not stationary
model1 <- VECM(inputs, lag = 4, include = 'both', estim = '2OLS', exogen = as.matrix(exog))

# PAL ACT DOWN - ISR TECH DOWN
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'pal_event2', response = 'isr_smalltech')
dev.off()
pdf(file = 'pactItech.pdf', height = 5, width = 8)
plot(irf_m1, main = 'PAL activity (negative shock)', ylab = 'ISR light weapons use ', xlab = 'foo')
dev.off()



