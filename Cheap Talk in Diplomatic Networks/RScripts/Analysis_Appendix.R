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

# rm(list=ls())

## Set working directory

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  # setwd('/Users/jesse/Dropbox/NetworkSignedComm')
  setwd('/Users/localadmin/Dropbox/Research/CheapTalk')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/Research/CheapTalk')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/Research/CheapTalk')
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

load('./Data/AppendixData/yearly_data.Rdata')


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

## One-year lead for civil war intervention (DV)
yearly_data[, lag_civilconflict := shift(civil_conflict, 1, type = 'lead'), by = c('statea', 'stateb')]
yearly_data[, lag_supportstate := shift(support_state, 1, type = 'lead'), by = c('statea', 'stateb')]


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
yearly_data[, mean_weight_net := mean_weight_pos - mean_weight_neg]
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
  
  ## Time variable starting with 1
  analysis_data[, time := year - 1994]
  
  ## Convert year to factor for modeling
  analysis_data[, year := as.factor(year)]
  
  
  ## Generate vector of test data dyads
  dyad_ids <- unique(analysis_data$dyad_id)
  n_dyads <- length(dyad_ids)
  n_sample <- as.integer(n_dyads*.3)
  sample_dyads <- sample(dyad_ids, n_sample, replace = F)
  
  ##### Process according to desired task
  if (out_var == 'mid'){
    
    ## 0 - left-censored
    analysis_data[, status := 0]
    ## 1- Event at time t
    analysis_data[lag_midstart == 1, status := 1]
    ## 2 - right-censored
    # analysis_data[year == 2001 & lag_midstart == 0, status := 2]
    
    ## Subset into training/testing data
    train_rows <- createDataPartition(analysis_data$lag_midstart, times = 1, p = 0.7)
    train_rows <- train_rows$Resample1
    train_data <- analysis_data[train_rows]
    test_data <- analysis_data[-train_rows]
    
    
  } else if (out_var == 'alliance'){
    ## 0 - left-censored
    analysis_data[, status := 0]
    ## 1- Event at time t
    analysis_data[lag_allianceformation == 1, status := 1]
    ## 2 - right-censored
    # analysis_data[year == 2001 & lag_midstart == 0, status := 2]
    
    analysis_data[, sum_ally_years := sum(alliance_present), by = c('statea', 'stateb')]
    analysis_data[, constant_alliance := 0L]
    analysis_data[sum_ally_years == 6, constant_alliance := 1L]
    # analysis_data <- analysis_data[sum_ally_years < 6]
    
    ## Subset into training/testing data
    train_rows <- createDataPartition(analysis_data$lag_allianceformation, times = 1, p = 0.7)
    train_rows <- train_rows$Resample1
    train_data <- analysis_data[train_rows]
    test_data <- analysis_data[-train_rows]
    
    
  } else if (out_var == 'intervention'){
    
    analysis_data[is.na(analysis_data$lag_supportstate), lag_supportstate := 0]
    analysis_data[, lag_supportstate := factor(lag_supportstate)]
    analysis_data[, lag_supportstate := relevel(lag_supportstate, '0')]
    
    analysis_data <- analysis_data[lag_civilconflict == 1]
    
    ## Subset into training/testing data
    train_rows <- createDataPartition(analysis_data$lag_supportstate, times = 1, p = 0.7)
    train_rows <- train_rows$Resample1
    train_data <- analysis_data[train_rows]
    test_data <- analysis_data[-train_rows]
    
    
  }

  ## Return data as list
  return(list(train_data, test_data))
}


##### Generate training and testing data
set.seed(8002)
cleaned_data_relevant <- prep_data(yearly_data_relevant)
mid_train_relevant <- cleaned_data_relevant[[1]]
mid_test_relevant <- cleaned_data_relevant[[2]]


###############################################################################
##
## Model set 1
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
  + year
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
  # + polity_diff
  # + joindem:polity_diff
  + relcap
  + sum_mids
  + year
  , data = mid_train_relevant
  , family = binomial(link = 'logit')
  )


## Model 3.0: Adding low-salience trust variable
##  (mean POSITIVE weight)
mid_logit_30 <- glm(
  lag_midstart
  ~ mean_weight_net
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  # + polity_diff
  # + joindem:polity_diff
  + relcap
  + sum_mids
  + year
  , data = mid_train_relevant
  , family = binomial(link = 'logit')
)

## Model 3.1: Adding low-salience trust variable
##  (mean NEGATIVE weight)
mid_logit_31 <- glm(
  lag_midstart
  ~ mean_weight_neg
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  # + polity_diff
  # + joindem:polity_diff
  + relcap
  + sum_mids
  + year
  , data = mid_train_relevant
  , family = binomial(link = 'logit')
)

## Model 3.2: Adding low-salience trust variable
##  (BOTH weights)
mid_logit_32 <- glm(
  lag_midstart
  ~ mean_weight_pos
  + mean_weight_neg
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  # + polity_diff
  # + joindem:polity_diff
  + relcap
  + sum_mids
  + year
  , data = mid_train_relevant
  , family = binomial(link = 'logit')
)


## Model 3.3: Adding low-salience trust variable
##  (BOTH weights)
mid_logit_33 <- glm(
  lag_midstart
  ~ mean_weight_pos * mean_weight_neg
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  # + polity_diff
  # + joindem:polity_diff
  + relcap
  + sum_mids
  + year
  , data = mid_train_relevant
  , family = binomial(link = 'logit')
)
summary(mid_logit_1)
summary(mid_logit_2)
summary(mid_logit_30)
summary(mid_logit_31)
summary(mid_logit_32)
summary(mid_logit_33)

interplot(
  mid_logit_33
  , var1 = 'mean_weight_neg'
  , var2 = 'mean_weight_pos'
) +
  xlab('positive weight') +
  ylab('negative weight (coefficient)')

interplot(
  mid_logit_33
  , var2 = 'mean_weight_neg'
  , var1 = 'mean_weight_pos'
) +
  xlab('negative weight') +
  ylab('positive weight (coefficient)')




###############################################################################
##
## Model set 2
##  DV: Alliance formation
##  Data: Relevant dyads
##
## Modeling approach: Heckman selection model
##  First stage: alliance present/absent
##  Second stage: alliance formation
##
###############################################################################

##### Generate training and testing data
# set.seed(8002365)
cleaned_data_relevant <- prep_data(yearly_data_relevant, 'alliance')
ally_train_relevant <- cleaned_data_relevant[[1]]
ally_test_relevant <- cleaned_data_relevant[[2]]

### MOVE TO APPENDIX SECTION ###
# ally_1 <- glm(
#   lag_allianceformation
#   ~ sum_mids
#   + log(trade_flow + 1)
#   + relcap
#   + year
#   , data = ally_train_relevant
# )
# 
# 
# ally_2 <- glm(
#   lag_allianceformation
#   ~ alliance_present
#   + igo_overlap
#   + relig_distance
#   + joindem
#   + log(trade_flow + 1)
#   + relcap
#   + year
#   , data = ally_train_relevant
# )
# summary(ally_2)
# 
# 
# ally_3 <- glm(
#   lag_allianceformation
#   ~ alliance_present
#   + mean_weight_pos * mean_weight_neg
#   + igo_overlap
#   + relig_distance
#   + joindem
#   + log(trade_flow + 1)
#   + relcap
#   + year
#   , data = ally_train_relevant
# )
# summary(ally_3)
# 
# ### Predict test data using model
# pred_ally_m2 <- data.frame(
#   lag_allianceformation = mid_test_relevant$lag_allianceformation
#   , pred = predict(ally_2, newdata = mid_test_relevant, type = 'response')
# )
# 
# pred_ally_m3 <- data.frame(
#   lag_allianceformation = mid_test_relevant$lag_allianceformation
#   , pred = predict(ally_3, newdata = mid_test_relevant, type = 'response')
# )
# 
# ## Calculate AUC
# auc(pred_ally_m2$lag_allianceformation, pred_ally_m2$pred)
# auc(pred_ally_m3$lag_allianceformation, pred_ally_m3$pred)
# 
# 
# 
################################################################################

## Model 1: functional variables only
ally_1 <- selection(
  constant_alliance == 0
  ~ log(trade_flow + 1)
  + relcap
  , lag_allianceformation
  ~ sum_mids
  + log(trade_flow + 1)
  + relcap
  + year
  , data = ally_train_relevant
)
summary(ally_1)

## Model 2: add constructivist structural variables
ally_2 <- selection(
  constant_alliance == 0
  ~ log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  , lag_allianceformation
  ~ sum_mids
  + igo_overlap
  + relig_distance
  + joindem
  + log(trade_flow + 1)
  + relcap
  + year
  , data = ally_train_relevant
)
summary(ally_2)

## Model 3.0: positive weight in final stage
ally_30 <- selection(
  constant_alliance == 0
  ~ log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  , lag_allianceformation
  ~ mean_weight_pos
  + sum_mids
  + igo_overlap
  + relig_distance
  + joindem
  + log(trade_flow + 1)
  + relcap
  + year
  , data = ally_train_relevant
)
summary(ally_30)

## Model 3.1: negative weight in final stage
ally_31 <- selection(
  constant_alliance == 0
  ~ log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  , lag_allianceformation
  ~ mean_weight_neg
  + sum_mids
  + igo_overlap
  + relig_distance
  + joindem
  + log(trade_flow + 1)
  + relcap
  + year
  , data = ally_train_relevant
)
summary(ally_31)

## Model 3.2: positive + negative weight in final stage
ally_32 <- selection(
  constant_alliance == 0
  ~ log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  , lag_allianceformation
  ~ mean_weight_pos
  + mean_weight_neg
  + sum_mids
  + igo_overlap
  + relig_distance
  + joindem
  + log(trade_flow + 1)
  + relcap
  + year
  , data = ally_train_relevant
)
summary(ally_32)

## Model 3.3: positive * negative weight in final stage
ally_33 <- selection(
  constant_alliance == 0
  ~ log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  , lag_allianceformation
  ~ mean_weight_pos*mean_weight_neg
  + sum_mids
  + igo_overlap
  + relig_distance
  + joindem
  + log(trade_flow + 1)
  + relcap
  + year
  , data = ally_train_relevant
)


summary(ally_1)
summary(ally_2)
summary(ally_30)
summary(ally_31)
summary(ally_32)
summary(ally_33)



########## Calculate model fit on TRAINING data

## Model 2: no weight
pred_ally_m2 <- data.frame(
  lag_allianceformation = ally_train_relevant$lag_allianceformation
  , pred = predict(ally_2, newdata = ally_train_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.0: positive weight
pred_ally_m30 <- data.frame(
  lag_allianceformation = ally_train_relevant$lag_allianceformation
  , pred = predict(ally_30, newdata = ally_train_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.1: negative weight
pred_ally_m31 <- data.frame(
  lag_allianceformation = ally_train_relevant$lag_allianceformation
  , pred = predict(ally_31, newdata = ally_train_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.2: positive + negative weight
pred_ally_m32 <- data.frame(
  lag_allianceformation = ally_train_relevant$lag_allianceformation
  , pred = predict(ally_32, newdata = ally_train_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.3: positive * negative weight
pred_ally_m33 <- data.frame(
  lag_allianceformation = ally_train_relevant$lag_allianceformation
  , pred = predict(ally_33, newdata = ally_train_relevant, part = 'outcome', type = 'unconditional')
)


## Calculate AUC
auc(pred_ally_m2$lag_allianceformation, pred_ally_m2$pred)
auc(pred_ally_m30$lag_allianceformation, pred_ally_m30$pred)
auc(pred_ally_m31$lag_allianceformation, pred_ally_m31$pred)
auc(pred_ally_m32$lag_allianceformation, pred_ally_m32$pred)
auc(pred_ally_m33$lag_allianceformation, pred_ally_m33$pred)


### Predict test data using model

## Model 2: no weight
pred_ally_m2 <- data.frame(
  lag_allianceformation = ally_test_relevant$lag_allianceformation
  , pred = predict(ally_2, newdata = ally_test_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.0: positive weight
pred_ally_m30 <- data.frame(
  lag_allianceformation = ally_test_relevant$lag_allianceformation
  , pred = predict(ally_30, newdata = ally_test_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.1: negative weight
pred_ally_m31 <- data.frame(
  lag_allianceformation = ally_test_relevant$lag_allianceformation
  , pred = predict(ally_31, newdata = ally_test_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.2: positive + negative weight
pred_ally_m32 <- data.frame(
  lag_allianceformation = ally_test_relevant$lag_allianceformation
  , pred = predict(ally_32, newdata = ally_test_relevant, part = 'outcome', type = 'unconditional')
)

## Model 3.3: positive * negative weight
pred_ally_m33 <- data.frame(
  lag_allianceformation = ally_test_relevant$lag_allianceformation
  , pred = predict(ally_33, newdata = ally_test_relevant, part = 'outcome', type = 'unconditional')
)


## Calculate AUC
auc(pred_ally_m2$lag_allianceformation, pred_ally_m2$pred)
auc(pred_ally_m30$lag_allianceformation, pred_ally_m30$pred)
auc(pred_ally_m31$lag_allianceformation, pred_ally_m31$pred)
auc(pred_ally_m32$lag_allianceformation, pred_ally_m32$pred)
auc(pred_ally_m33$lag_allianceformation, pred_ally_m33$pred)

## t-test analyzing cooperation within long-term alliances vs outside
ally_ttest_data <- rbind(ally_train_relevant, ally_test_relevant)
t.test(
  ally_ttest_data[constant_alliance == 1, mean_weight_pos]
  , ally_ttest_data[constant_alliance == 0, mean_weight_pos]
  )


###############################################################################
##
## Model set 3
##  DV: Choice to intervene and whether to intervene positive/negative
##
##  Modeling approach: Multinomial model
##    Baseline: 0 (do nothing)
##    Level 1: -1 (intervene to help rebels)
##    Level 2: +1 (intervene to help state)
##
##
###############################################################################

##### Generate training and testing data
# set.seed(800085)
cleaned_data_relevant <- prep_data(yearly_data_relevant, 'intervention')
inter_train_relevant <- cleaned_data_relevant[[1]]
inter_test_relevant <- cleaned_data_relevant[[2]]

## Model 1: Functional variables 
##  (trade, capacity, alliance)
inter_1 <- multinom(
  lag_supportstate
  ~ alliance_present
  + log(trade_flow + 1)
  + relcap
  + sum_mids
  + year
  , data = inter_train_relevant
)


## Model 2: Adding normative/structural variables 
##  (joint democracy, religious distance, IGOs)
inter_2 <-  multinom(
  lag_supportstate
  ~ alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  + sum_mids
  + year
  , data = inter_train_relevant
  , weights = ifelse(inter_train_relevant$lag_supportstate == '0', 1, 20)
)


## Model 3.0: Adding low-salience trust variable
##  (mean POSITIVE weight)
inter_30 <- multinom(
  lag_supportstate
  ~ mean_weight_pos
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  + sum_mids
  + year
  , data = inter_train_relevant
  , weights = ifelse(inter_train_relevant$lag_supportstate == '0', 1, 4)
)

## Model 3.1: Adding low-salience trust variable
##  (mean NEGATIVE weight)
inter_31 <- multinom(
  lag_supportstate
  ~ mean_weight_neg
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  + sum_mids
  + year
  , data = inter_train_relevant
  , weights = ifelse(inter_train_relevant$lag_supportstate == '0', 1, 4)
)

## Model 3.2: Adding low-salience trust variable
##  (BOTH weights)
inter_32 <- multinom(
  lag_supportstate
  ~ mean_weight_pos
  + mean_weight_neg
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  + sum_mids
  + year
  , data = inter_train_relevant
  , weights = ifelse(inter_train_relevant$lag_supportstate == '0', 1, 4)
)


## Model 3.3: Adding low-salience trust variable
##  (BOTH weights)

inter_33 <- multinom(
  lag_supportstate
  ~ mean_weight_pos*mean_weight_neg
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  + sum_mids
  + year
  , data = inter_train_relevant
  , weights = ifelse(inter_train_relevant$lag_supportstate == '0', 1, 20)
)


## Model 3.3: Adding low-salience trust variable
##  (BOTH weights) FOR PLOTTING ONLY

inter_34 <- multinom(
  lag_supportstate
  ~ mean_weight_neg*mean_weight_pos
  + alliance_present
  + log(trade_flow + 1)
  + igo_overlap
  + relig_distance
  + joindem
  + relcap
  + sum_mids
  + year
  , data = inter_train_relevant
  , weights = ifelse(inter_train_relevant$lag_supportstate == '0', 1, 20)
)

summary(inter_1)
summary(inter_2)
summary(inter_30)
summary(inter_31)
summary(inter_32)
summary(inter_33)


plot(
  effect(
  "mean_weight_pos*mean_weight_neg"
  , inter_33
  , xlevels = list(
    mean_weight_pos = seq(0, 9, 5)
    , mean_weight_neg = seq(0, 9, 5)
    )
  )
)

plot(
  effect(
    "mean_weight_neg*mean_weight_pos"
    , inter_34
    , xlevels = list(
      mean_weight_pos = seq(0, 9, 5)
      , mean_weight_neg = seq(0, 9, 5)
    )
  )
)



## Model 2: no weight
pred_inter_m2 <- data.frame(
  lag_supportstate = inter_test_relevant$lag_supportstate
  , pred = predict(inter_2, newdata = inter_test_relevant)
)

## Model 3.0: positive weight
pred_inter_m30 <- data.frame(
  lag_supportstate = inter_test_relevant$lag_supportstate
  , pred = predict(inter_30, newdata = inter_test_relevant)
)

## Model 3.1: negative weight
pred_inter_m31 <- data.frame(
  lag_supportstate = inter_test_relevant$lag_supportstate
  , pred = predict(inter_31, newdata = inter_test_relevant)
)

## Model 3.2: positive + negative weight
pred_inter_m32 <- data.frame(
  lag_supportstate = inter_test_relevant$lag_supportstate
  , pred = predict(inter_32, newdata = inter_test_relevant)
)

## Model 3.3: positive * negative weight
pred_inter_m33 <- data.frame(
  lag_supportstate = inter_test_relevant$lag_supportstate
  , pred = predict(inter_33, newdata = inter_test_relevant)
)

##### Generate confusion matrices
confusionMatrix(table(pred_inter_m2$lag_supportstate, pred_inter_m2$pred, dnn = c('Observed', 'Predicted')))

# confusionMatrix(table(pred_inter_m32$lag_supportstate, pred_inter_m32$pred, dnn = c('Observed', 'Predicted')))

confusionMatrix(table(pred_inter_m33$lag_supportstate, pred_inter_m33$pred, dnn = c('Observed', 'Predicted')))

latextable(table(pred_inter_m2$lag_supportstate, pred_inter_m2$pred, dnn = c('Observed', 'Predicted')))
latextable(table(pred_inter_m30$lag_supportstate, pred_inter_m30$pred, dnn = c('Observed', 'Predicted')))
latextable(table(pred_inter_m31$lag_supportstate, pred_inter_m31$pred, dnn = c('Observed', 'Predicted')))
latextable(table(pred_inter_m32$lag_supportstate, pred_inter_m32$pred, dnn = c('Observed', 'Predicted')))
latextable(table(pred_inter_m33$lag_supportstate, pred_inter_m33$pred, dnn = c('Observed', 'Predicted')))



## Calculate AUC






