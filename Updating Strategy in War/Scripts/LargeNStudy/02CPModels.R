rm(list = ls())
if(!'pacman' %in% row.names(installed.packages())){install.packages('pacman')}
pacman::p_load(foreign, data.table, lubridate, xts, ecp, changepoint, survival, simPH)
set.seed(100)
# setwd('C:/Users/Jesse/Dropbox/Dissertation/Data/LargeNStudy')
setwd('/Users/jesse/Dropbox/Dissertation/Data/LargeNStudy')
# setwd('/media/jesse/Files/Dropbox/Dissertation/Data/LargeNStudy')

## Read in data
ts_data <- fread('monthly_violence.csv')
conf_episodes <- unique(ts_data[, list(conflict_id, episode)])

ts_data[, rebel_change := 0L]
ts_data[, state_change := 0L]
for(i in 1:nrow(conf_episodes)){
  this_conf <- conf_episodes$conflict_id[i]
  this_ep <- conf_episodes$episode[i]
  this_data <- ts_data[conflict_id %in% this_conf & episode %in% this_ep]
  state_ts <- ts(this_data[, state_combined2], frequency = 12)
  rebel_ts <- ts(this_data[, rebel_combined2], frequency = 12)

  state_ts <- this_data[, state_combined2]
  rebel_ts <- this_data[, rebel_combined2]

#   state_cp <- e.divisive(as.matrix(state_ts), R = 999, sig.lvl = 0.01, min.size = 3)
#   rebel_cp <- e.divisive(as.matrix(rebel_ts), R = 999, sig.lvl = 0.01, min.size = 3)
#
#   if(length(state_cp$estimates) > 1 & length(state_cp$estimates) < .5*length(state_ts)){
#     state_change_idx <- state_cp$estimates[-c(1,length(state_cp$estimates))]
#     this_data[state_change_idx, state_change := 1]
#   }
#
#   if(length(rebel_cp$estimates) > 1 & length(rebel_cp$estimates) < .5*length(rebel_ts)){
#     rebel_change_idx <- rebel_cp$estimates[-c(1,length(rebel_cp$estimates))]
#     this_data[rebel_change_idx, rebel_change := 1]
#   }

  max_cp <- round(sqrt(length(state_ts)))
  state_cp <- cpt.mean(state_ts, pen.value = 0.1, Q = max_cp, minseglen = 4
                       , penalty = 'AIC', method = 'BinSeg', class = F)
  if(length(state_cp) > 1){
    state_change_idx <- state_cp[-c(length(state_cp))]
    this_data[state_change_idx, state_change := 1]
  }

  rebel_cp <- cpt.mean(rebel_ts, pen.value = 0.1, Q = max_cp, minseglen = 4
                       , penalty = 'AIC', method = 'BinSeg', class = F)
  if(length(rebel_cp) > 1){
    rebel_change_idx <- rebel_cp[-c(length(rebel_cp))]
    this_data[rebel_change_idx, rebel_change := 1]
  }

  ts_data[conflict_id %in% this_conf & episode %in% this_ep] <- this_data

}

ts_data[, state_idx := cumsum(state_change), by = list(conflict_id, episode)]
ts_data[, rebel_idx := cumsum(rebel_change), by = list(conflict_id, episode)]
ts_data[, state_change_time := seq(.N), by = list(conflict_id, episode, rebel_idx)]
ts_data[, rebel_change_time := seq(.N), by = list(conflict_id, episode, state_idx)]
ts_data[, months := .N, by = list(conflict_id, episode)]
ts_data[, state_changes := sum(state_change), by = list(conflict_id, episode)]
ts_data[, rebel_changes := sum(rebel_change), by = list(conflict_id, episode)]
setkeyv(ts_data, c('conflict_id', 'episode'))
ts_data[, ep_end:=0L]
ts_data[ts_data[unique(ts_data),,mult="last", which=T], ep_end:=1L]


lagfun <- function(x) return(c(x[-1], NA))
ts_data[, state_change := lagfun(state_change), by = list(conflict_id, episode)]
ts_data[, rebel_change := lagfun(rebel_change), by = list(conflict_id, episode)]


sum_data <- ts_data[state_change == 1 | rebel_change == 1 | ep_end == 1]
# sum_data <-ts_data[state_change == 1 | rebel_change == 1]
sum_data[, mean_rebel_time := mean(rebel_change_time), by = list(conflict_id, episode)]
sum_data[rebel_changes == 0, mean_rebel_time := sum_data[rebel_changes == 0, months]]
sum_data[, mean_state_time := mean(state_change_time), by = list(conflict_id, episode)]
sum_data[state_changes == 0, mean_state_time := sum_data[state_changes == 0, months]]
sum_data[outcome %in% c(3,5), outcome3 := -1]
sum_data[outcome %in% c(1,4), outcome3 := 1]
sum_data[outcome %in% c(2,6), outcome3 := 0]

sum_data[outcome %in% c(2,3,5,6), outcome2 := -1]
sum_data[outcome %in% c(1,4), outcome2 := 1]

setkeyv(sum_data, c('conflict_id', 'episode'))
sum_data <- unique(sum_data)
sum_data <- sum_data[, list(month, gwno, conflict_id, episode, months, state_changes
                            , rebel_changes, mean_rebel_time, mean_state_time
                            , outcome, outcome2, outcome3)]

sum_data[state_changes > 0, rebel_change_ratio := rebel_changes / state_changes]
sum_data[is.na(rebel_change_ratio), rebel_change_ratio := 1]

# Weak coding: rebels are faster OR EQUAL TO state updating speed
# sum_data[, rebel_faster := mean_rebel_time <= mean_state_time]

# # Strong coding: rebels are faster state updating speed
sum_data[, rebel_faster := mean_rebel_time < mean_state_time]

#
# # Medium coding: rebels are faster / equal / slower than state
# sum_data[mean_rebel_time < mean_state_time, rebel_faster := -1]
# sum_data[mean_rebel_time > mean_state_time, rebel_faster := 1]
# sum_data[mean_rebel_time == mean_state_time, rebel_faster := 0]


# Table: do rebels that update more quickly win conflicts?
outcomes_tab <- table(sum_data$rebel_faster, sum_data$outcome2)
dimnames(outcomes_tab)[[1]] <- c('StateFast', 'RebFast')
dimnames(outcomes_tab)[[2]] <- c('StateWin', 'RebWin')
outcomes_tab

foo <- c(6/31, 4/14)
## Do faster rebels win more?
prop.test(c(6,4), n = c(31, 14), correct = T, conf.level = 0.05)
## Do rebel victories involve faster rebels?
prop.test(c(10,4), n = c(35, 10), correct = T, conf.level = 0.05)

t.test(sum_data$rebel_faster[sum_data$outcome2 == -1], sum_data$rebel_faster[sum_data$outcome2 == 1])

# t-test: do conflicts where rebels update faster last longer?
t.test(sum_data[rebel_faster == 0, months], sum_data[rebel_faster == 1, months])

# survival model: do conflicts where rebels update faster last longer?
numDEpi(X1 = sum_data$reb_faster)

powerCT.default0(k = 10/35, m = 45, RR = 0.75, alpha = 0.05)

sum_data[, rebel_faster := rebel_faster * 1]
test1 <- coxph( Surv(months, rep(1, nrow(sum_data))) ~
                  rebel_faster +
                  cluster(conflict_id), data = sum_data)
summary(test1)
cox.zph(test1)

Sim1 <- coxsimLinear(test1, b = "rebel_faster", Xj = 0:1, nsim = 1000)
dev.off()
pdf(file = './RebelUpdatingHazard', height = 6, width = 6)
simGG(Sim1, type = 'points', method = 'lm', alpha = 0.5
      , rug = T, rug_position = 'identity'
      , xlab = 'Rebel faster military updating'
      , ylab = 'Relative hazard ratio'
      , yaxis = NA) +
  scale_x_continuous(breaks=seq(0, 1, 1))
dev.off()



dev.off()
pdf(file = './RebelUpdatingHazRate', height = 6, width = 10)
plot(survfit(Surv(sum_data$months, rep(1, nrow(sum_data))) ~
               sum_data$rebel_faster), conf.int = T
     , col = c('red', 'blue')
     , xlab = 'Conflict episode duration (months)'
     , ylab = 'Estimated Hazard Rate')
legend('topright', legend = c('Faster Rebels', 'Faster State')
       , col = c('blue', 'red')
       , lty = c(1,1))
dev.off()




# Survival model: are conflicts likely to end after strategic switch?
ts_data[, id := paste(conflict_id, episode, sep = '_')]
ts_data[, time := seq(1:.N), by = list(conflict_id, episode)]
ts_data[, time0 := time-1]

sum_data2 <- ts_data[state_change == 1 | rebel_change == 1 | ep_end == 1]
sum_data2[, time1 := 1:.N, by = id]
cutpoints <- unique(sum_data2[state_change == 1 | rebel_change == 1 | ep_end == 1, time])

test_data <- survSplit(data = sum_data2, cut = cutpoints, end = 'time', start = 'time0', event = 'ep_end')
test_data <- test_data[order(test_data$id, test_data$time0), ]


test1 <- coxph( Surv(time0, time, ep_end, type = 'counting') ~
                  (state_change_time + rebel_change_time) +
                  cluster(conflict_id), data = ts_data)
summary(test1)
cox.zph(test1)
stargazer(test1)


test2 <- coxph( Surv(time0, time, ep_end, type = 'counting') ~
                  (state_change_time * rebel_change_time) +
                  cluster(conflict_id), data = ts_data)
summary(test2)
cox.zph(test2)

Sim2 <- coxsimInteract(test2, b1 = "state_change_time", b2 = 'rebel_change_time'
                       , qi = 'Marginal Effect'
                      , X2 = seq(1, 24, 8), nsim = 100)

dev.off()
pdf(file = './RebelUpdatingMargEff', height = 6, width = 10)
simGG(Sim2, xlab = 'Months since last change by state'
      , ylab = 'Hazard ratio: Months since last rebel change')
dev.off()
