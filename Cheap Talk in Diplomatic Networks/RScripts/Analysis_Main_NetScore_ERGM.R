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

## Load dependencies
pacman::p_load(data.table, caret, sampleSelection, nnet, statnet)

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

## Political relevance (1995 - 2010)
## Object: yearly_relevance (DIRECTED)
load('./Data/AnalysisData/yearly_relevance.Rdata')


###############################################################################
##
## Load in data for analysis
##
###############################################################################

load('./Data/AnalysisData/yearly_data.Rdata')

###############################################################################
##
## Load in CAMEO code data
##
###############################################################################

cameocodes <- data.table::fread('/Users/jesse/Dropbox/Research/CheapTalk/Paper/PlotsTables/CameoScores.csv')

###############################################################################
##
## Generate some on-the-fly variables
##
###############################################################################

## One-year lead for MID occurrence (DV)
yearly_data[, lag_midstart := shift(mid_start, 1, type = 'lead'), by = c('statea', 'stateb')]
yearly_data[is.na(lag_midstart) & year == 2009, lag_midstart := 0]

## One-year lead for alliance formation (DV)
yearly_data[, lag_allianceformation := shift(alliance_formed, 1, type = 'lead'), by = c('statea', 'stateb')]

## One-year lead for civil war intervention (DV)
yearly_data[, lag_civilconflict := shift(civil_conflict, 1, type = 'lead'), by = c('statea', 'stateb')]
yearly_data[, lag_supportstate := shift(support_state, 1, type = 'lead'), by = c('statea', 'stateb')]


## Cumulative sum of events (for stratification)
yearly_data[, sum_mids := NA_integer_]
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

yearly_data[is.na(mid_start) & year == 2009, mid_start := 0]
yearly_data[is.na(mid_start) & year == 2010, mid_start := 0]
yearly_data[is.na(mid_start) & year == 2011, mid_start := 0]

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
yearly_data_relevant <- yearly_data_relevant[
  as.integer(as.factor(statea)) >
    as.integer(as.factor(stateb))
  ]


###############################################################################
##
## Generate yearly networks
##
###############################################################################

state_factors <- as.factor(unique(c(unique(yearly_data_relevant$statea), unique(yearly_data_relevant$stateb))))
yearly_data_relevant[, statea := factor(statea, levels = levels(state_factors))]
yearly_data_relevant[, stateb := factor(stateb, levels = levels(state_factors))]
n_states <- length(state_factors)




years <- 1995:2009
for(this_year in years){
  
  print(this_year)
  
  this_data_edges <- yearly_data_relevant[year %in% c(this_year+1) & mid_start == 1, list(year, statea, stateb, mid_start)]
  
  this_data_dyads <- yearly_data_relevant[
    year %in% this_year
    , list(statea, stateb, mean_weight_pos, mean_weight_neg, mean_weight_net
           , sharedcomm
           , alliance_present, trade_flow, igo_overlap, relig_distance, polity_diff, joindem, relcap, logdist
           , mid_start, sum_mids)
    ]
  this_data_dyads[is.na(sharedcomm), sharedcomm := 0]
  
  
  this_data_nodes <- yearly_data_relevant[year %in% this_year, list(statea, comm_size_a, comm_indeg_a, comm_outdeg_a, comm_between_a, comm_embedded_a)]
  this_data_nodes <- unique(this_data_nodes)
  
  this_net <- network.initialize(length(state_factors), directed = F)
  network.vertex.names(this_net) <- as.integer(this_data_nodes$statea)
  this_net[as.matrix(this_data_edges[, list(as.integer(statea), as.integer(stateb))])] <- 1
  
  this_net %v% 'comm_size' <- this_data_nodes$comm_size_a
  this_net %v% 'comm_embedded' <- this_data_nodes$comm_embedded
  this_net %v% 'comm_indeg' <- this_data_nodes$comm_indeg_a
  # this_net %v% 'comm_size' <- this_data_nodes$comm_size_a
  # this_net %v% 'comm_size' <- this_data_nodes$comm_size_a
  
  
  cols <- names(this_data_dyads)[-c(1:2)]
  dyad_array <- array(dim = c(n_states, n_states, length(cols)))
  dimnames(dyad_array)[[3]] <- cols
  for(col in 1:length(cols)){
    this_col <- cols[col]
    new_mat <- matrix(nrow = n_states, ncol = n_states, 0)
    new_mat[as.matrix(this_data_dyads[, list(as.integer(statea), as.integer(stateb))])] <- this_data_dyads[[this_col]]
    new_mat[upper.tri(new_mat)] <- t(new_mat[lower.tri(new_mat)])
    new_mat[is.na(new_mat)] <- 0
    dyad_array[,,col] <- new_mat
    
  }
  
  
  test <- ergm(this_net ~ edges
               + isolates
               # + gwdsp(fixed = T, alpha = 1.25)
               + degreepopularity
               + absdiff('comm_size')
               + nodecov('comm_embedded')
               + absdiff('comm_embedded')
               + absdiff('comm_indeg')
               + dyadcov(dyad_array[,,'mean_weight_pos'])
               + dyadcov(dyad_array[,,'mean_weight_neg'])
               # + edgecov(dyad_array[,,'mean_weight_net'])
               + edgecov(dyad_array[,,'sharedcomm'])
               + edgecov(dyad_array[,,'alliance_present'])
               + edgecov(dyad_array[,,'trade_flow'])
               + edgecov(dyad_array[,,'relig_distance'])
               + edgecov(dyad_array[,,'joindem'])
               + edgecov(dyad_array[,,'relcap'])
               + edgecov(dyad_array[,,'logdist'])
               # + edgecov(dyad_array[,,'sum_mids'])
               , estimate = 'MPLE'
               , parallel = T
               , cores = 6
               )
  print(summary(test))
}



