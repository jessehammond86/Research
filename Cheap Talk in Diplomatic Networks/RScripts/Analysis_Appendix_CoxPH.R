###############################################################################
##
## Analysis_Appendix_CoxPH.R
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
  setwd('/Users/jesse/Dropbox/Research/Cheaptalk')
  # setwd('/Users/localadmin/Dropbox/Research/CheapTalk')
  
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
mid_cox_30 <- cph(
  mid_train_onset_relevant
  ~ mean_weight_net
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
