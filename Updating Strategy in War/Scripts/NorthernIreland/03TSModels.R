rm(list = ls())
setwd('/Users/jesse/Dropbox/Dissertation/Data/NorthernIreland')
# setwd('C:\\Users\\Jesse\\Dropbox\\Dissertation\\Data\\NorthernIreland')
# setwd('/media/jesse/Files/Dropbox/Dissertation/Data/NorthernIreland')
if(!'pacman' %in% row.names(installed.packages())){install.packages('pacman')}
pacman::p_load(rgdal,foreign, data.table, raster, rgeos,maptools, pscl, e1071, reshape2, psych, TSA, vars, zoo, lubridate, xts, ggplot2)

## Read in data
all_deaths <- fread('NI_quarter_deaths.csv')
belfast_deaths <- fread('belfast_quarter_deaths.csv')
rural_deaths <- fread('rural_quarter_deaths.csv')

#################
##
## VAR models and diagnostics
##
#################
library(vars)

var_function <- function(x, y, p = 2){
  x$quarter <- as.Date(as.yearqtr(x$quarter))
  input_data <- xts(x[, -1, with = F], order.by = x$quarter)

  exog <- scale(cbind(input_data[, c('CATH_CASRATIO')]
                      , lag(input_data[, c('CATH_CASRATIO')], 1)
                      , lag(input_data[, c('CATH_CASRATIO')], 2)))
  exog[is.na(exog)] <- 0

  # Union kills - repub kills - state kills
  model1 <- VAR(y = scale(input_data[, y])
                , p = p, type = 'both', exogen = exog)

  print(arch.test(model1, lags.single = 4, lags.multi = 4))
  return(model1)
}

exog <- data.frame(CATH_CASRATIO = scale(cbind(all_deaths[, CATH_CASRATIO]
                    , CATH_CASRATIO_1 = lag(all_deaths[, CATH_CASRATIO], 1)
                    , CATH_CASRATIO_2 = lag(all_deaths[, CATH_CASRATIO], 2))))
exog[is.na(exog)] <- 0


## Hypothesis 1:
## State violence in Belfast/Derry -> Republican violence in rural areas
model1 <- var_function(all_deaths, c('STATEURBAN', 'REPUBURBAN', 'UNIONURBAN'))

## Hypothesis set 1 IRF plots
## State urban violence -> Republican urban violence
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'STATEURBAN', response = 'REPUBURBAN')
irf_data1 <- data.frame(mean = irf_m1[[1]], lower = irf_m1[[2]], upper = irf_m1[[3]], period = c(1:6))
names(irf_data1) <- c('mean', 'lower', 'upper', 'week')
dev.off()
pdf(file = 'StateUrbanRepubUrban.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data1) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Quarters after simulated shock to British focus on urban violence'
       , y = 'Change in Republican focus on urban violence') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()


## Hypothesis 2:
## Republican attacks on civilians -> Unionist indiscriminate -> Republican attacks on civilians
model1 <- var_function(all_deaths, c('repub_civ_pct', 'union_indis_pct', 'repub_indis_pct'))    # No ARCH

## Hypothesis set 2 IRF plots
# Republican civilian violence -> Union indiscriminate violence
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'repub_civ_pct', response = 'union_indis_pct')
irf_data1 <- data.frame(mean = irf_m1[[1]], lower = irf_m1[[2]], upper = irf_m1[[3]], period = c(1:6))
names(irf_data1) <- c('mean', 'lower', 'upper', 'week')
dev.off()
pdf(file = 'RepubCivUnionIndis.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data1) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Quarters after simulated shock to Republican civilian targeting'
       , y = 'Change in Unionist use of indiscriminate weaponry') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()

# Union indiscriminate violence -> Republican indiscriminate violence
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'union_indis_pct', response = 'repub_indis_pct')
irf_data1 <- data.frame(mean = irf_m1[[1]], lower = irf_m1[[2]], upper = irf_m1[[3]], period = c(1:6))
names(irf_data1) <- c('mean', 'lower', 'upper', 'week')
dev.off()
pdf(file = 'UnionIndisRepubIndis.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data1) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Quarters after simulated shock to Unionist use of indiscriminate'
       , y = 'Change in Republican use of indiscriminate weaponry') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()

## Hypothesis 3:
## State killing Republicans -> Republican kills -> State killing Republicans
model1 <- var_function(all_deaths, c('STATEMILKILLS', 'REPUBKILLS', 'REPUBKILLSBASES'))    # ARCH

## Hypothesis set 3 IRF plots
# State kills of Republican fighters -> Republican kills
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'STATEMILKILLS', response = 'REPUBKILLS')
irf_data1 <- data.frame(mean = irf_m1[[1]], lower = irf_m1[[2]], upper = irf_m1[[3]], period = c(1:6))
names(irf_data1) <- c('mean', 'lower', 'upper', 'week')
dev.off()
pdf(file = 'StateKillsRepubKills.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data1) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Quarters after simulated shock to British killings of Republican fighters'
       , y = 'Change in level of Republican killings') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()

# State kills of Republican fighters -> Republican operations near British bases
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'STATEMILKILLS', response = 'REPUBKILLSBASES')
irf_data1 <- data.frame(mean = irf_m1[[1]], lower = irf_m1[[2]], upper = irf_m1[[3]], period = c(1:6))
names(irf_data1) <- c('mean', 'lower', 'upper', 'week')
dev.off()
pdf(file = 'StateKillsRepubBases.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data1) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Quarters after simulated shock to British killings of Republican fighters'
       , y = 'Change in mean security density in areas Republicans attack') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()

## Hypothesis 4:
## Unionist violence in Catholic areas -> Republican violence in Protestant areas
model1 <- var_function(all_deaths, c('REPUBKILLSCATH', 'UNIONKILLSCATH'))    # No ARCH

## Hypothesis set 4 IRF plots
## Unionist violence in Catholic areas -> Republican violence in Protestant areas
irf_m1 <- irf(model1, n.ahead = 5, runs = 1000, impulse = 'REPUBKILLSCATH', response = 'UNIONKILLSCATH')
irf_data1 <- data.frame(mean = irf_m1[[1]], lower = irf_m1[[2]], upper = irf_m1[[3]], period = c(1:6))
names(irf_data1) <- c('mean', 'lower', 'upper', 'week')
dev.off()
pdf(file = 'RepubCathUnionCath.pdf', width = 10, height = 6)
ggplot() +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = 2) +
  geom_line(aes(y = mean, x = week), color = 'red', data = irf_data1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = week)
              , alpha = 0.25, fill = 'blue', data = irf_data1) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = as.character(seq(0,5))) +
  labs(x = 'Quarters after simulated shock to Republican attacks in Protestant areas'
       , y = 'Change in Unionist focus on attacking Catholic areas') +
  theme(panel.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , axis.text=element_text(size=12)
        , axis.title=element_text(size=14))
dev.off()
