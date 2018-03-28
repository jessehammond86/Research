###############################################################################
##
## ProcessICEWSData.R
##
## Depends on: GetICEWSData.R
## 
## Purpose: Processes the data downloaded and munged through PhoenixNet in
##  a previous step, and writes the output to disk as an .Rdata file.
##
## Function: Intakes an .Rdata object created by the previous step and converts
##  temporal multi-layered event networks into simplified positive/negative/
##  mixed-interaction adjacency matrices.
##
## IMPORTANT NOTE: because I am interested in "low-salience" interactions,
##  I drop all events with Goldstein scores above 7 or below -7.
##
##
## Output: A list object containing a set of 3-layer matrix arrays, one per
##  month for the time period covered in the previous step. The three layers
##  in each array include:
##  1. The positive score of interactions between states that month.
##  2. The negative score of interactions between states that month.
##  3. The net (pos - neg) score of interactions between states that month.
##
## Output files: 
##  - monthly_arrays_raw.Rdata: list of arrays representing raw counts of 
##    cooperative/conflictual events between states, normalized by rootcode
##    density per month.
##  - monthly_arrays_gold.Rdata: list of arrays representing Goldstein-weighted
##    counts of cooperative/conflictual events between states, normalized by
##    rootcode density per month.
##
###############################################################################

rm(list=ls())

## Load a bunch of dependencies

if (!'pacman' %in% installed.packages()) install.packages('pacman')
if(!'phoenixNet' %in% installed.packages()) devtools::install_github('jrhammond/phoenixNet')
pacman::p_load(phoenixNet, igraph)

## Set working directory

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  setwd('/Users/localadmin/Dropbox/NetworkSignedComm')
  phoenixnet_dir <- '/Users/localadmin/Dropbox/Minerva'
  # setwd('/Users/jesse/Dropbox/NetworkSignedComm')
  # phoenixnet_dir <- '/Users/jesse/Dropbox/Minerva'
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkSignedComm')
  phoenixnet_dir <- '/media/jesse/Files/Dropbox/Minerva'
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/NetworkSignedComm')
  phoenixnet_dir <- 'C:/Users/Jesse/Dropbox/Minerva'
}


###############################################################################
##
## Loading and setting up analysis data
##
###############################################################################

## Load network data
load('./Data/EventNetworks/rootcode_month_data.Rdata')
load('./Data/EventNetworks/rootcode_year_data.Rdata')

## Load CAMEO codes data for Goldstein score assignment
cameo <- fread('./Data/AnalysisData/CameoScores.csv')


###############################################################################
##
## Processing CAMEO data: getting mean Goldstein/CAMEO score per root code.
## Raw CAMEO score data taken from:
##  http://eventdata.parusanalytics.com/cameo.dir/CAMEO.scale.html
##
###############################################################################

cameo <- cameo[, mean(GoldScore), by = RootCode]
setnames(cameo, 'V1', 'MeanGoldScore')


###############################################################################
##
## Function: NetworkExtract
##
## Purpose: split each tsna object into a list of monthly arrays,
##  where every array slice maps onto one root code and every element (i, j, x)
##  contains the *normalized* level of interaction between states i and j on
##  dimension x during a given month.
##
## Arguments:
##  - in_data: list of tsna objects produced by phoenix_net() function.
##  - tie_weights ('raw', 'goldstein'): should ties be weighted by count (raw)
##    or by count * Goldstein score (goldstein)? Goldstein scores include
##    more nuance about event severity, but are also unreliable/uninformative.
##
## Outputs:
##  - monthly_weighted_posneg: list of 3-layer arrays where each array is 
##    one month's sociomatrix and each layer corresponds to positive, negative,
##    and net-pos-neg edges between states.
##
###############################################################################

NetworkExtract <- function(
  in_data
  , tie_weights = 'raw'
  , ties_normalize = 'state'
  , exclude_codes = NULL
  , time_window = 'month'
  ){
  ## Set up dates
  global_start_end <- get.network.attribute(in_data$dailynets[[1]],'net.obs.period')$observations[[1]]
  global_start_date_int <- global_start_end[1]
  global_end_date_int <- global_start_end[2]
  global_start_date <- as.Date(as.character(global_start_date_int), format = '%Y%m%d')
  global_end_date <- as.Date(as.character(global_end_date_int), format = '%Y%m%d')
  dates <- as.integer(format(seq.Date(global_start_date, global_end_date, time_window), '%Y%m%d'))
  start_date <- dates[1]
  end_date <- last(dates)
  
  ## Get size of global network
  n_states <- network.size(in_data$dailynets[[1]])
  
  ## Get names of states
  states <- network::get.vertex.attribute(in_data$dailynets[[1]], 'vertex.names')
  
  ## Subset rootcodes to exclude certain classes of event
  rootcodes <- 1:20
  if(!is.null(exclude_codes)){
    rootcodes <- setdiff(rootcodes, exclude_codes)
  }
  
  ## Pull out individual rootcode-networks, normalize, and store each adjacency
  ##  matrix as one layer in a monthly event-network-array.
  interactions <- list()
  events_array <- array(0, dim = c(n_states, n_states, 20))
  
  for(this_date in 1:length(dates)){
    
    ## Create array to hold event network matrices
    thismonth_events <- events_array
    
    for(this_code in rootcodes){
      
      ## Extract single event network
      events_tsna <- in_data$dailynets[[paste0('code', this_code)]]
      
      ## Create matrix for event interaction data at time T
      events_net <- network.collapse(events_tsna, at = dates[this_date])
      events_mat <- as.matrix.network(events_net, attrname = 'N')
      
      ## Normalize the events network: what share of state i's interactions does
      ##  dyad (i,j) make up?
      ## NOTE: The goal of normalization here is to allow more direct comparison
      ##  between different categories of events, as some events are much more
      ##  common (~10x) than others. Normalizing each event type means that we 
      ##  can directly compare "conflictual" and "cooperative" relations despite
      ##  cooperative acts generally being more common.If 90% of state i's 
      ##  conflictual relations are with state j, then that is likely a more
      ##  fraught relationship, even (especially?) if state i does not engage
      ##  in a high leve of overall conflict.
      if (ties_normalize == 'state'){
        if(sum(events_mat) > 0){
          # events_mat <- events_mat / sum(events_mat)
          events_rowsums <- rowSums(events_mat)
          events_rowsums[events_rowsums == 0] <- 1
          events_mat <- events_mat / events_rowsums
        }
      } else if (ties_normalize == 'global'){
        if(sum(events_mat) > 0){
          events_mat <- events_mat / sum(events_mat)
        }
      }
      
      ## Scoring networks: assign each element (i, j, x) a value corresponding
      ##  to the mean Goldstein score associated with that root code. The resulting
      ##  value will be:
      ##  (i, j, x) = normalized count (i, j, x) * goldstein score [x]
      if (tie_weights == 'goldstein'){
        
        events_mat <- events_mat * cameo[RootCode %in% this_code, MeanGoldScore]
        
      }
      
      
      ## Store normalized event-month network in this array
      thismonth_events[, , this_code] <- events_mat
    }
    
    ## Store fully populated array in array list by date
    interactions[[this_date]] <- thismonth_events
    
  }
  
  
  
  ###############################################################################
  ##
  ## Aggregating scored networks: collapse rootcode-level arrays to pos/neg
  ##  array by summing all positive and all negative array layers
  ##
  ###############################################################################
  
  weighted_array <- array(0, dim = c(n_states, n_states, 3))
  dimnames(weighted_array)[[1]] <- states
  dimnames(weighted_array)[[2]] <- states
  
  weighted_posneg <- list()
  
  for(this_date in 1:length(dates)){
    
    weighted_array[, , 1] <- apply(interactions[[this_date]][, , 1:8], c(1, 2), function(x) sum(x))
    weighted_array[, , 2] <- apply(interactions[[this_date]][, , 9:20], c(1, 2), function(x) sum(x))
    weighted_array[, , 3] <- weighted_array[, , 1] - abs(weighted_array[, , 2])
    weighted_posneg[[this_date]] <- weighted_array
    
  }
  
  return(weighted_posneg)
}

###############################################################################
##
## Run functions: extract network data from tsna objects.
##
## NOTE: For this analysis, I am leaving out two rootcode categories as they
##  may overlap with another data source:
##  - rootcode 15 includes escalating force posture (MID cat 1, 2)
##  - rootcode 19 includes military conflict (MID cat 3, 4)
##
###############################################################################

########## Monthly data
## Raw counts
# monthly_arrays_raw <- NetworkExtract(
#   rootcode_month_data
#   , tie_weights = 'raw'
#   , exclude_codes = c(15, 19)
#   , time_window = 'month'
#   , ties_normalize = 'state'
#   )
# ## Goldstein-weighted
# monthly_arrays_gold <- NetworkExtract(
#   rootcode_month_data
#   , tie_weights = 'goldstein'
#   , exclude_codes = c(15, 19)
#   , time_window = 'month'
#   , ties_normalize = 'state'
#   )


########## Yearly data data
## Raw counts
yearly_arrays_raw <- NetworkExtract(
  rootcode_year_data
  , tie_weights = 'raw'
  , exclude_codes = c(7, 15, 18, 19, 20)
  , time_window = 'year'
  , ties_normalize = 'state'
)
## Goldstein-weighted
yearly_arrays_gold <- NetworkExtract(
  rootcode_year_data
  , tie_weights = 'goldstein'
  , exclude_codes = c(7, 15, 18, 19, 20)
  , time_window = 'year'
  , ties_normalize = 'state'
)


## Goldstein-weighted
# yearly_arrays_gold <- NetworkExtract(
#   rootcode_year_data
#   , tie_weights = 'goldstein'
#   , exclude_codes = c(15, 19)
#   , time_window = 'year'
#   , ties_normalize = 'global'
# )
# summary(as.vector(yearly_arrays_gold[[1]][,,3]))
# sum(yearly_arrays_gold[[21]][,,3] > 0)
# sum(yearly_arrays_gold[[21]][,,3] < 0)
# 
# 
# yearly_arrays_gold2 <- NetworkExtract(
#   rootcode_year_data
#   , tie_weights = 'goldstein'
#   , exclude_codes = c(15, 19)
#   , time_window = 'year'
#   , ties_normalize = 'state'
# )
# summary(as.vector(yearly_arrays_gold2[[1]][,,3]))
# sum(yearly_arrays_gold2[[21]][,,3] > 0)
# sum(yearly_arrays_gold2[[21]][,,3] < 0)
# 
# 
# yearly_arrays_gold3 <- NetworkExtract(
#   rootcode_year_data
#   , tie_weights = 'goldstein'
#   , exclude_codes = c(15, 19)
#   , time_window = 'year'
#   , ties_normalize = 'none'
# )
# summary(as.vector(yearly_arrays_gold3[[1]][,,3]))
# sum(yearly_arrays_gold3[[21]][,,3] > 0)
# sum(yearly_arrays_gold3[[21]][,,3] < 0)

###############################################################################
##
## Write data to files:
##  1. Save monthly network-array data as .Rdata file
##  2. Write state list as data.table to CSV file
##
###############################################################################

# save(monthly_arrays_raw, file = './Data/AnalysisData/monthly_arrays_raw.Rdata')
# save(monthly_arrays_gold, file = './Data/AnalysisData/monthly_arrays_gold.Rdata')

save(yearly_arrays_raw, file = './Data/AnalysisData/yearly_arrays_raw.Rdata')
save(yearly_arrays_gold, file = './Data/AnalysisData/yearly_arrays_gold.Rdata')

# statelist <- data.table(rownames(monthly_arrays_raw[[1]][,,1]))
# setnames(statelist, 'state')
# write.csv(statelist, file = './Data/statelist.csv', row.names = F)
