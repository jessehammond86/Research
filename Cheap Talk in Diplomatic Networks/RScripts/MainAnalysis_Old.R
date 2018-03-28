###############################################################################
##
## MainAnalysis.R
## 
## Purpose: Run a set of models on the finalized data.
##
## Depends on: MergeAnalysisData.R
##
## Function: Loads finished data, does some on-the-fly processing,
##  and runs a set of predictive models.
##
## Output: .Rdata objects containing models.
##
## Output files: 
##  - TBA
##
###############################################################################

rm(list=ls())

## Load a bunch of dependencies

if (!'pacman' %in% installed.packages()) install.packages('pacman')
if(!'phoenixNet' %in% installed.packages()) devtools::install_github('jrhammond/phoenixNet')
pacman::p_load(data.table, igraph, rms, survAUC, stargazer)

## Set working directory

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  # setwd('/Users/jesse/Dropbox/NetworkSignedComm')
  setwd('/Users/localadmin/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/NetworkSignedComm')
}


###############################################################################
##
## Load in data on political relevance for subsetting data
##
###############################################################################

## Political relevance (1995 - 2001)
## Object: yearly_relevance (UNDIRECTED)
load('./Data/AnalysisData/yearly_relevance.Rdata')


###############################################################################
##
## Load in data for analysis
##
###############################################################################

load('./Data/AnalysisData/yearly_data.Rdata')


###############################################################################
##
## Generate some on-the-fly variables
##
###############################################################################

## One-year lead for MID occurrence (DV)
yearly_data[, lag_midstart := shift(mid_start, 1, type = 'lead'), by = c('statea', 'stateb')]
yearly_data[is.na(lag_midstart) & year == 2000, lag_midstart := 0]

## One-year lead for alliance formation (DV)
yearly_data[, lag_allianceformation := shift(alliance_formed, 1, type = 'lead'), by = c('statea', 'stateb')]

## Cumulative sum of events (for stratification)
yearly_data[, sum_mids := NA_real_]
yearly_data[, sum_mids := cumsum(mid_start), by = c('statea', 'stateb')]
yearly_data[, sum_mids := shift(sum_mids, 1, type = 'lag'), by = c('statea', 'stateb')]
yearly_data[is.na(sum_mids), sum_mids := 0]

## Generate B -> A ICEWS weight
temp_data <- yearly_data[, list(year, statea, stateb, weight_pos, weight_neg)]
setnames(temp_data, c('year', 'stateb', 'statea', 'weight_pos_ba', 'weight_neg_ba'))
yearly_data <- merge(yearly_data, temp_data, by = c('year', 'statea', 'stateb'))

## Calculate undirected ICEWS weight variables
yearly_data[, mean_weight_pos := rowMeans(as.matrix(yearly_data[, list(weight_pos, weight_pos_ba)]))]
yearly_data[, mean_weight_neg := abs(rowMeans(as.matrix(yearly_data[, list(weight_neg, weight_neg_ba)])))]
# yearly_data[, net_weight := abs(weight - weight_ba)]
# yearly_data[, min_weight := pmin(weight, weight_ba, na.rm = T)]
# yearly_data[, max_weight := pmax(weight, weight_ba, na.rm = T)]

## Dyad ID variable
yearly_data[, dyad_id := paste(statea, stateb, sep = '_')]

## Re-sort and remove duplicates
setkeyv(yearly_data, c('year', 'statea', 'stateb'))
yearly_data <- subset(unique(yearly_data))


## Temp hack for trade data and MIDs in 2001
yearly_data[is.nan(trade_flow), trade_flow := 0]
yearly_data[is.na(trade_flow), trade_flow := 0]

yearly_data[is.na(mid_start) & year == 2000, mid_start := 0]
yearly_data[is.na(mid_start) & year == 2001, mid_start := 0]
yearly_data[is.na(mid_start) & year == 2002, mid_start := 0]

###############################################################################
##
## Transform the data to generate TWO undirected-dyad data sets:
##  1. Politically relevant dyads ONLY
##  2. All dyads
##
###############################################################################

## Data set 1: Merge in data on political relevance. 
##  This will both subset the number of states/dyads,
##  AND change from A->B/B->A to A--B undirected dyad format.

yearly_data_relevant <- merge(
  yearly_data
  , yearly_relevance
  , by = c('year', 'statea', 'stateb')
)


## Data set 2: Subset data from directed-dyad to undirected-dyad format
yearly_data_full <- yearly_data[
  as.integer(as.factor(statea)) >
    as.integer(as.factor(stateb))
  ]



###############################################################################
##
## Model set 1
##  DV: MID onset
##  Data: Relevant dyads
##
##  Modeling approach: Cox hazard analysis
##
###############################################################################

##### Pre-processing function to generate training/test sets
prep_data <- function(input_data, out_var = 'mid', out_year = 2000){
  ## Subset by year <= 2001
  analysis_data <- input_data[year <= out_year]
  
  ## Keep only complete cases
  analysis_data <- analysis_data[
    complete.cases(
      analysis_data[
        , 
        list(lag_midstart, mean_weight_pos, mean_weight_neg
             , alliance_present, trade_flow
             , igo_overlap, relig_distance, joindem
             , relcap, polity_diff)
        ]
    )
    ]
  
  ## Generate vector of test data dyads
  dyad_ids <- unique(analysis_data$dyad_id)
  n_dyads <- length(dyad_ids)
  n_sample <- as.integer(n_dyads*.2)
  sample_dyads <- sample(dyad_ids, n_sample, replace = F)
  
  ##### Censoring variable for Surv input
  if (out_var == 'mid'){
    ## 0 - left-censored
    analysis_data[, status := 0]
    ## 1- Event at time t
    analysis_data[lag_midstart == 1, status := 1]
    ## 2 - right-censored
    # analysis_data[year == 2001 & lag_midstart == 0, status := 2]
  } else if (out_var == 'alliance'){
    ## 0 - left-censored
    analysis_data[, status := 0]
    ## 1- Event at time t
    analysis_data[lag_allianceformation == 1, status := 1]
    ## 2 - right-censored
    # analysis_data[year == 2001 & lag_midstart == 0, status := 2]
    
    # analysis_data[, sum_ally_years := sum(alliance_present), by = c('statea', 'stateb')]
    # analysis_data <- analysis_data[sum_ally_years < 7]
  }
  
  
  ## Time variable starting with 1
  analysis_data[, time := year - 1994]
  
  ## Subset into training/testing data
  train_data <- analysis_data[!dyad_id %in% sample_dyads]
  test_data <- analysis_data[dyad_id %in% sample_dyads]
  
  ## Return data as list
  return(list(train_data, test_data))
}


##### Environmental variable: Create datadist() object calculating
##  appropriate testing/plotting values for all variables
model_vars <- yearly_data_relevant[
  , list(mean_weight_pos, mean_weight_neg, alliance_present, trade_flow, igo_overlap
         , relig_distance, joindem, polity_diff, relcap, sum_mids)
  ]

dd <- datadist(model_vars)
options(datadist = 'dd')


##### Generate training and testing data
set.seed(80085)
cleaned_data_relevant <- prep_data(yearly_data_relevant)
mid_train_relevant <- cleaned_data_relevant[[1]]
mid_test_relevant <- cleaned_data_relevant[[2]]


## Generate Survival objects
mid_train_onset_relevant <- Surv(
  time = mid_train_relevant$time
  # , time2 = mid_train_relevant$time+1
  , event = mid_train_relevant$status
)

mid_test_onset_relevant <- Surv(
  time = mid_test_relevant$time
  , time2 = mid_test_relevant$time+1
  , event = mid_test_relevant$status
)

##### Running models
## Baseline model, time only
mid_surv_0 <- survfit(mid_train_onset_relevant ~ 1, data = mid_train_relevant)

## Model 1: Functional variables 
##  (trade, capacity, alliance)
mid_cox_1 <- cph(
  mid_train_onset_relevant
  ~ alliance_present
  + log(trade_flow + 1)
  + relcap
  + sum_mids*year
  , data = mid_train_relevant
  , x = T
  , y = T
  , surv = T)

## Model 2: Adding normative/structural variables 
##  (joint democracy, religious distance, IGOs)
mid_cox_2 <-  cph(
  mid_train_onset_relevant
  ~ alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  + sum_mids*year
  , data = mid_train_relevant
  , x = T
  , y = T
  , surv = T)

  
## Model 3: Adding low-salience trust variable
##  (mean weight)
mid_cox_3 <- cph(
  mid_train_onset_relevant
  ~ mean_weight_pos
  + mean_weight_neg
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  + sum_mids*year
  , data = mid_train_relevant
  , x = T
  , y = T
  , surv = T)


## Model 4: Alternative specification of regime similarity
##  (POLITY difference instead of joint democracy binary)
mid_cox_4 <- cph(
  mid_train_onset_relevant
  ~ mean_weight_pos*mean_weight_neg
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  + sum_mids*year
  , data = mid_train_relevant
  , x = T
  , y = T
  , surv = T)

mid_cox_4

cox.zph(mid_cox_4)

stargazer(mid_cox_1, mid_cox_2, mid_cox_3, mid_cox_4)


###############################################################################
##
## Model set 2
##  DV: MID onset
##  Data: Relevant dyads
##
##  Modeling approach: logistic regression
##
###############################################################################

## Model 1: Functional variables 
##  (trade, capacity, alliance)
mid_logit_1 <- glm(
  lag_midstart
  ~ alliance_present
  + log(trade_flow + 1)
  + relcap
  + sum_mids
  + as.factor(year)
  , data = mid_train_relevant
  , family = binomial(link = 'logit')
)


## Model 2: Adding normative/structural variables 
##  (joint democracy, religious distance, IGOs)
mid_logit_2 <-  glm(
  lag_midstart
  ~ alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  + sum_mids
  + as.factor(year)
  , data = mid_train_relevant
  , family = binomial(link = 'logit')
  )


## Model 3: Adding low-salience trust variable
##  (mean weight)
mid_logit_3 <- glm(
  lag_midstart
  ~ mean_weight_pos*mean_weight_neg
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem*polity_diff
  + relcap
  + sum_mids
  + as.factor(year)
  , data = mid_train_relevant
  , family = binomial(link = 'logit')
)

mid_logit_31 <- glm(
  lag_midstart
  ~ mean_weight_pos
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem*polity_diff
  + relcap
  + sum_mids
  + as.factor(year)
  , data = mid_train_relevant
  , family = binomial(link = 'logit')
)

mid_logit_32 <- glm(
  lag_midstart
  ~ mean_weight_neg
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem*polity_diff
  + relcap
  + sum_mids
  + as.factor(year)
  , data = mid_train_relevant
  , family = binomial(link = 'logit')
)


summary(mid_logit_3)
interplot(
  mid_logit_3
  , var1 = 'mean_weight_neg'
  , var2 = 'mean_weight_pos'
  , hist = T
) +
  xlab('positive weight') +
  ylab('negative weight (coefficient)')

interplot(
  mid_logit_3
  , var2 = 'mean_weight_neg'
  , var1 = 'mean_weight_pos'
  , hist = T
) +
  xlab('negative weight') +
  ylab('positive weight (coefficient)')


### Predict test data using model
pred_data_m3 <- data.frame(
  lag_midstart = mid_test_relevant$lag_midstart
  , pred = predict(mid_logit_3, newdata = mid_test_relevant, type = 'response')
)

pred_data_m2 <- data.frame(
  lag_midstart = mid_test_relevant$lag_midstart
  , pred = predict(mid_logit_2, newdata = mid_test_relevant, type = 'response')
)


plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$lag_midstart == 1, "TP", v)
  v <- ifelse(df$pred >= threshold & df$lag_midstart == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df$lag_midstart == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df$lag_midstart == 0, "TN", v)
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=lag_midstart, y=pred)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}

pacman::p_load(gridExtra, ggplot2, lattice)

calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$lag_midstart == 1) / sum(df$lag_midstart == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$lag_midstart == 0) / sum(df$lag_midstart == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(df$pred >= threshold & df$lag_midstart == 0) * cost_of_fp + 
      sum(df$pred < threshold & df$lag_midstart == 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}
plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  library(gridExtra)
  
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)
  
  grid.arrange(p_roc, p_cost, ncol=2, sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
}

plot_pred_type_distribution(pred_data, 0.25)
pred_roc <- calculate_roc(pred_df, 1, 1, n = 100)
plot_roc(pred_roc, 0.1, 1, 1)

library(pROC)
auc(pred_data_m2$lag_midstart, pred_data_m2$pred)
auc(pred_data_m3$lag_midstart, pred_data_m3$pred)










###############################################################################
##
## Model set 2
##  DV: Alliance formation
##  Data: All dyads
##
###############################################################################

##### Generate training and testing data
set.seed(80085)
cleaned_data_relevant <- prep_data(yearly_data_full, 'alliance', out_year = 2005)
ally_train_relevant <- cleaned_data_relevant[[1]]
ally_test_relevant <- cleaned_data_relevant[[2]]


## Generate Survival objects
ally_train_onset_relevant <- Surv(
  time = ally_train_relevant$time
  , time2 = ally_train_relevant$time+1
  , event = ally_train_relevant$status
)

ally_test_onset_relevant <- Surv(
  time = ally_test_relevant$time
  , time2 = ally_test_relevant$time+1
  , event = ally_test_relevant$status
)

##### Running models
## Baseline model, time only
ally_surv_0 <- survfit(ally_train_onset_relevant ~ 1, data = ally_train_relevant)

## Model 1: Functional variables 
##  (trade, capacity, alliance)
ally_cox_1 <- cph(
  ally_train_onset_relevant
  ~ sum_mids
  + log(trade_flow + 1)
  + relcap
  , data = ally_train_relevant
  , x = T
  , y = T
  , surv = T)

## Model 2: Adding normative/structural variables 
##  (joint democracy, religious distance, IGOs)
ally_cox_2 <-  cph(
  ally_train_onset_relevant
  ~ sum_mids
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  , data = ally_train_relevant
  , x = T
  , y = T
  , surv = T)


## Model 3: Adding low-salience trust variable
##  (mean weight)
ally_cox_3 <- cph(
  ally_train_onset_relevant
  ~ mean_weight
  + sum_mids
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  , data = ally_train_relevant
  , x = T
  , y = T
  , surv = T)


## Model 4: Alternative specification of regime similarity
##  (POLITY difference instead of joint democracy binary)
ally_cox_4 <- cph(
  ally_train_onset_relevant
  ~ mean_weight
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + polity_diff*joindem
  + relcap
  + logdist
  , data = ally_train_relevant
  , x = T
  , y = T
  , surv = T)

ally_cox_4








##### ally onset using all dyads
ally_data_full <- prep_data(yearly_data_full)
ally_onset_full <- Surv(
  time = ally_data_full$time_start
  , event = ally_data_full$ally_censor
)
ally_coxmodel_full <- cph(
  ally_onset_full
  ~ mean_weight
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  # + polity_diff
  + joindem
  + relcap
  + logdist
  + mean_weight:time_start
  + strat(sum_allys)
  , data = ally_data_full)
ally_coxmodel_full
cox.zph(ally_cox_4)



###############################################################################
##
## Model set 2 
##  DV: 
##
##  Modeling approach: 
##
##
###############################################################################

pacman::p_load(nnet)
civil_data_relevant <- yearly_data_relevant[civil_conflict == 1]
civil_data_relevant[, support_state := as.factor(support_state)]

test <- multinom(
  support_state
  ~ mean_weight
  , data = civil_data_relevant
)



##### MID onset using politically relevant dyads
mid_logit_relevant <- glm(
  lag_midstart
  ~ mean_weight
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  # + polity_diff
  + joindem
  + relcap
  + sum_mids
  + as.factor(year)
  , data = mid_data_relevant
  , family = binomial(link = 'logit'))
summary(mid_logit_relevant)


##### MID onset using all dyads
mid_logit_full <- glm(
  lag_midstart
  ~ mean_weight
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  # + polity_diff
  + joindem
  + relcap
  + logdist
  + sum_mids
  + as.factor(year)
  , data = mid_data_full
  , family = binomial(link = 'logit'))
summary(mid_logit_full)




##### Event-history model of alliance formation

ally_data <- yearly_data[year <= 2001]
## Dyad ID variable
ally_data[, dyad_id := paste(statea, stateb, sep = '_')]

ally_dyads <- unique(ally_data[alliance_formed == 1, list(dyad_id, year)])
ally_dyads <- ally_dyads[!duplicated(dyad_id)]
ally_dyads <- ally_dyads[year < 2001]
for(this_dyad in 1:nrow(ally_dyads)){
  ally_data[
    which(
      year > ally_dyads[this_dyad, year]
      & dyad_id == ally_dyads[this_dyad, dyad_id]
    )
    , weight := NA
    ]
  print(paste(this_dyad, nrow(ally_data)))
}


##### Censoring variable for Surv input
### Interval type
# # 0 - right-censored
# ally_data[, ally_censor := 0]
# # 1- Event at time t
# ally_data[lag_allianceformation == 1, ally_censor := 1]
# ## 2- left-censored
# ally_data[year == 2012 & alliance_present == 1, ally_censor := 2]
# ## 3 -Interval censored
# ally_data[year == 1995 & alliance_present == 1, ally_censor := 3]

### Interval2 type
# 0 - non-censored
ally_data[, ally_censor := 0]
# 1- Event at time t
ally_data[lag_allianceformation == 1, ally_censor := 1]
# 2- left-censored
# ally_data[year == 1995 & alliance_present == 1, ally_censor := 2]
# 3 -right-censored
# ally_data[year == 2012 & alliance_present == 1, ally_censor := NA]


## Interval-based time variables starting with 1
ally_data[, time_start := year - 1994]
ally_data[, time_end := year - 1993]

ally_formation <- Surv(
  time = ally_data$time_start
  , time2 = ally_data$time_end
  , event = ally_data$ally_censor
)

ally_coxmodel <- coxph(
  ally_formation
  ~ mean_weight
  + mid_start
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + polity_diff
  + relcap
  , data = ally_data)
summary(ally_coxmodel)
cox.zph(ally_coxmodel)
