rm(list = ls())
# setwd('/Users/jesse/Dropbox/Dissertation/Data/Israel')
# setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\Israel')
setwd('/media/jesse/Files/Dropbox/Dissertation/Data/Israel')
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
input_data <- fread('02IsrPalTSData.csv')
input_data$week <- as.Date(input_data$week)


#############################################################################
## PRELIMINARY STEP: ARIMAX models to assess exogenous relationships
## These are not the most 'theoretically appropriate' models, but they may serve
## to indicate promising avenues for analysis.


####### Hypothesis set 1: predicting actor behavior based on casualty ratios

#### PAL ACTIONS
# Positive change in CIVILIAN TARGETING from CASUALTY RATIOS
exog = cbind(lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week)),
             lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 2),
             lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 3),
             lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 4))
exog[is.na(exog)] <- 0
armodel1 <- arima(input_data[, list(pal_civtargeting)], order = c(4,0,0), xreg = exog)
armodel1

# Change in AREA A/B targeting from CASUALTY RATIOS
armodel2 <- arima(input_data[, list(mean_palevent_area_ab)], order = c(4,0,0), xreg = exog)
armodel2

# Change on AREA C targeting from CASUALTY RATIOS
armodel3 <- arima(input_data[, list(mean_palevent_area_c)], order = c(4,0,0), xreg = exog)
armodel3

# # Change in PAL TERRORISM TACTICS from CASUALTY RATIOS
armodel4 <- arima(input_data[, list(mean_palscore)], order = c(4,0,0), xreg = exog)
armodel4


#### ISR ACTIONS
# Positive shock on HEAVY WEAPONS USE from CASUALTY RATIOS
armodel1 <- arima(input_data[, list(isr_bigtech)], order = c(4,0,0), xreg = exog)
armodel1

# Positive shock on CITY targeting from CASUALTY RATIOS
armodel2 <- arima(input_data[, list(mean_isrevent_city)], order = c(4,0,0), xreg = exog)
armodel2

# Positive shock on AREA A/B targeting from CASUALTY RATIOS
armodel3 <- arima(input_data[, list(mean_isrevent_area_ab)], order = c(4,0,0), xreg = exog)
armodel3

# Change in ISR INDISCRIMINATE TACTICS from CASUALTY RATIOS
armodel4 <- arima(input_data[, list(mean_isrscore)], order = c(4,0,0), xreg = exog)
armodel4



#####################################################################################
## MAIN ANALYSIS: These time series are likely highly endogenous, so ARIMAX is probably not the best approach.
## Using VAR models will allow me to assess the endogenized effects of a shock in one system on a shock in the other.
#####################################################################################
#### ENDOGENOUS SYSTEMS
exog = cbind(lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week)),
             lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 2),
             lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 3),
             lag(xts(input_data[,list(pal_casratio)], order.by = input_data$week), 4))
exog[is.na(exog)] <- 0



#################### VAR MODELS

####### Hypothesis set 2: PAL civilian targeting / ISR heavy weapons / PAL indirect-fire weapons
inputs = xts(input_data[,list(pal_civtargeting, isr_bigtech, pal_indirect
)], order.by = input_data$week)
VARselect(inputs, lag.max = 8, type = 'both')$selection
model1 <- VAR(inputs, p = 4, type = 'both', exog = exog)
# Portmanteau test - passed!
serial.test(model1, type = 'PT.asymptotic')
# ARCH-LM test - passed!
arch.test(model1, lags.multi = 4)
summary(model1)

##### Hypothesis set 2 IRF plots
## ISR heavy weapons -> PAL civilian targeting
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_bigtech', response = 'pal_civtargeting')
irf_data1 <- data.frame(mean = irf_m1[[1]], lower = irf_m1[[2]], upper = irf_m1[[3]], period = c(1:6))
names(irf_data1) <- c('mean', 'lower', 'upper', 'week')

dev.off()
pdf(file = 'isrTechPalTarg.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data1) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Weeks after simulated shock to ISR heavy-arms use'
       , y = 'Change in PAL targeting of civilians') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()

## ISR heavy weapons -> PAL indirect-fire weapons
irf_m2 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_bigtech', response = 'pal_indirect')
irf_data2 <- data.frame(mean = irf_m2[[1]], lower = irf_m2[[2]], upper = irf_m2[[3]], period = c(1:6))
names(irf_data2) <- c('mean', 'lower', 'upper', 'week')

dev.off()
pdf(file = 'isrTechpalTech.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data2) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Weeks after simulated shock to ISR heavy-arms use'
       , y = 'Change in PAL indirect-fire weapons use') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()



####### Hypothesis set 3: PAL violence in Israel & area C / ISR violence in Gaza & areas A/B
inputs = xts(input_data[,list(mean_isrevent_area_ab, mean_palevent_area_c
)], order.by = input_data$week)
VARselect(inputs, lag.max = 8, type = 'both')$selection
model1 <- VAR(inputs, p = 4, type = 'both', exog = exog)
# Portmanteau test - passed!
serial.test(model1, type = 'PT.asymptotic')
# ARCH-LM test - passed!
arch.test(model1, lags.multi = 4)
summary(model1)


##### Hypothesis set 3 IRF plots
## PAL violence in Israel & area C -> ISR violence in Gaza & areas A/B
irf_m2 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'mean_palevent_area_c'
              , response = 'mean_isrevent_area_ab')
irf_data1 <- data.frame(mean = irf_m2[[1]], lower = irf_m2[[2]], upper = irf_m2[[3]], period = c(1:6))
names(irf_data1) <- c('mean', 'lower', 'upper', 'week')

dev.off()
pdf(file = 'isrLocPalLoc.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data1) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Weeks after simulated shock to PAL operations in Israel and Area C'
       , y = 'Change in ISR operations in Areas A/B') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()



####### Hypothesis set 4: PAL terrorism and ISR repression
inputs = xts(input_data[,list(mean_palscore, mean_isrscore
)], order.by = input_data$week)
VARselect(inputs, lag.max = 8, type = 'both')$selection
model1 <- VAR(inputs, p = 4, type = 'both', exog = exog)
# Portmanteau test
serial.test(model1, type = 'PT.asymptotic')
# ARCH-LM test
arch.test(model1, lags.multi = 4)
summary(model1)


##### Hypothesis set 4 IRF plots
irf_m2 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'mean_palscore'
              , response = 'mean_isrscore')
irf_data1 <- data.frame(mean = irf_m2[[1]], lower = irf_m2[[2]], upper = irf_m2[[3]], period = c(1:6))
names(irf_data1) <- c('mean', 'lower', 'upper', 'week')

dev.off()
pdf(file = 'palTerIsrRep.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data1) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Weeks after simulated shock to PAL terrorism strategy'
       , y = 'Change in ISR large-scale repression strategy') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()



####### Hypothesis set 5: ISR military targeting, PAL event intensity, ISR small arms
inputs = xts(input_data[,list(isr_miltargeting, pal_event, isr_smalltech
)], order.by = input_data$week)
model1 <- VAR(inputs, p = 4, type = 'both', exog = exog)
# Consider using VECM model because ISR small tech is not stationary
# model1 <- VECM(inputs, lag = 4, include = 'both', estim = '2OLS', exogen = as.matrix(exog))
summary(model1)


##### Hypothesis set 5 IRF plots
## ISR military targeting -> PAL event intensity
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'isr_miltargeting', response = 'pal_event')
dev.off()
pdf(file = 'iactPact.pdf', height = 5, width = 8)
plot(irf_m1, main = 'ISR Miltary Targeting', ylab = 'PAL activity', xlab = 'foo')
dev.off()


### ONLY FOR VISUALIZATION - REVERSE SIGN ON PAL EVENTS TO SHOW NEGATIVE SHOCK
## ISR ACT UP - ISR TARG DOWN - PAL ACT DOWN - ISR TECH DOWN
input_data$pal_event2 <- -1 * input_data$pal_event
inputs = xts(input_data[,list(isr_miltargeting, pal_event2, isr_smalltech)]
             , order.by = input_data$week)
# Using VECM model because ISR small tech is not stationary
# model1 <- VECM(inputs, lag = 4, include = 'both', estim = '2OLS', exogen = as.matrix(exog))
model1 <- VAR(inputs, p = 4, type = 'both', exog = exog)

## PAL event intensity -> ISR small arms
irf_m3 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'pal_event2', response = 'isr_smalltech')
irf_data3 <- data.frame(mean = irf_m3[[1]], lower = irf_m3[[2]], upper = irf_m3[[3]], period = c(1:6))
names(irf_data3) <- c('mean', 'lower', 'upper', 'week')

dev.off()
pdf(file = 'pactItech.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data3) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data3) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Weeks after simulated shock to PAL weekly activity'
       , y = 'Change in ISR reliance on light arms') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()






